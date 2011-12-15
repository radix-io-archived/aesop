#ifndef __AE_CTL_H__
#define __AE_CTL_H__

#include "src/aesop/ae-types.h"
#include "src/aesop/ae-thread.h"
#include "src/aesop/ae-list.h"
#include "src/aesop/ae-error.h"
#include "src/aesop/hints.h"

#include <opa_primitives.h>

enum ae_ctl_state
{
    /* inputs for worker functions */
    AE_CTL_START = 1,
    AE_CTL_CALL_COMPLETE = 1 << 1,

    /* outputs from worker functions */

    AE_CTL_FN_COMPLETE      = 1 << 2,  /** the entire blocking function completed (hit a return statement) */
    AE_CTL_POST_SUCCESS     = 1 << 3,  /** a blocking function was successfully posted */
    AE_CTL_POST_FAILED      = 1 << 4,  /** a blocking function failed to post */
    AE_CTL_PBRANCH_DONE     = 1 << 5,  /** this pbranch completed, but not all others have completed yet */
    AE_CTL_PWAIT_DONE       = 1 << 6,  /** this pbranch completed and all others were also complete */
    AE_CTL_LONE_PBRANCH_DONE = 1 << 7  /** a lone pbranch completed */
};

enum ae_pbranch_state
{
    AE_PBRANCH_NULL = 0,
    AE_PBRANCH_INIT = 1,
    AE_PBRANCH_INPROGRESS = 2,
    AE_PBRANCH_DONE = 3
};

/**
 * 
 */ 
enum ae_pwait_command
{
    AE_PWAIT_NONE = 0,
    AE_PWAIT_CONTINUE = 1,
    AE_PWAIT_YIELD = 2
};

#define ae_pwait_command_string(__cmd)                                \
    (__cmd == AE_PWAIT_NONE) ?                                        \
        "NONE" : ((__cmd == AE_PWAIT_CONTINUE) ? "CONTINUE" :         \
                  ((__cmd == AE_PWAIT_YIELD) ? "YIELD" : "UNKNOWN"))

typedef struct ae_context *ae_context_t;

/* The ae_ctl structure is used by the aesop generated code to manage parallel and
 * nested operations.  This structure is not needed by resource writers or aesop
 * code.
 * TODO: move these to a separate header
 */
struct ae_ctl
{
    const char *name;
    void *state_label;
    struct ae_ctl *parent;
    void *spec_ctl;
    ae_op_id_t current_op_id;
    ae_mutex_t mutex;
    ae_list_t children;
    struct ae_list_link link;
    int posted;
    int completed;
    int allposted;
    int in_pwait;
    ae_hints_t *hints;
    ae_context_t context;
    OPA_int_t refcount;
};

/**
 * We keep track of lone pbranches using a global list.
 * These functions manipulate that list safely.
 */
void ae_lone_pbranches_add(struct ae_ctl *ctl);
void ae_lone_pbranches_remove(struct ae_ctl *ctl);
int ae_lone_pbranches_count(void);

static inline void ae_ctl_init(struct ae_ctl *ctl,
                               void *fctl,
                               const char *name,
                               ae_hints_t *hints,
                               ae_context_t context,
                               int internal,
                               void *user_ptr)
{
    ctl->name = name;
    ctl->posted = 0;
    ctl->completed = 0;
    ctl->allposted = 0;
    ctl->in_pwait = 0;

    /* by default, we just set the hints pointer, assuming the lifetime
     * of the hint pointer passed in will live for the entire blocking call
     * this control structure is allocated for
     */
    ctl->hints = hints;

    ctl->context = context;
    ae_mutex_init(&ctl->mutex, NULL);
    ae_list_init(&ctl->children);
    OPA_store_int (&ctl->refcount, 1);
    ae_op_id_clear(ctl->current_op_id);
    if(internal) ctl->parent = (struct ae_ctl *)user_ptr;
    else ctl->parent = NULL;
    ctl->spec_ctl = fctl;
}

static inline void ae_ctl_destroy(void *tctl, struct ae_ctl *ctl)
{
    free(tctl);
}

static inline int ae_ctl_refcount(struct ae_ctl *ctl)
{
    int rc;
    rc = OPA_load_int (&ctl->refcount);
    return rc;
}

/**
 * Atomically increment the refcount and return the new value.
 */
static inline int ae_ctl_refinc(struct ae_ctl *ctl)
{
    int rc;
    rc = OPA_fetch_and_incr_int (&ctl->refcount) + 1;
    return rc;
}

static inline void ae_ctl_addref(struct ae_ctl *ctl)
{
    OPA_incr_int (&ctl->refcount);
}

/**
 * Atomically decrement refcount and return the new value
 */
static inline int ae_ctl_refdec(struct ae_ctl *ctl)
{
    int rc;
    rc = OPA_fetch_and_decr_int (&ctl->refcount) - 1;
    return rc;
}

static inline void ae_ctl_done(struct ae_ctl *ctl)
{
    int refcount;
    void *fctl = ctl->spec_ctl;


    refcount = ae_ctl_refdec(ctl);
    assert(refcount >= 0);
    if(refcount == 0)
    {
        free(fctl);
    }
}

static inline void ae_ctl_pwait_start(struct ae_ctl *ctl)
{
    ctl->in_pwait = 1;
    ctl->posted = 0;
    ctl->completed = 0;
    ctl->allposted = 0;
    assert(ae_list_count(&ctl->children) == 0);
}

static inline void ae_ctl_lone_pbranch_start(struct ae_ctl *ctl)
{
    ae_list_link_clear(&ctl->link);
    ae_lone_pbranches_add(ctl);
    /* parent context disappears, we need to copy the hint */
    ae_hints_dup(ctl->parent->hints, &ctl->hints);
    ae_ctl_refinc(ctl->parent);
}

static inline void ae_ctl_lone_pbranch_done(struct ae_ctl *ctl, enum ae_ctl_state *state)
{
    /* remove myself from lone pbranch list */
    ae_lone_pbranches_remove(ctl);
    /* destroy my hints */
    ae_hints_destroy(ctl->hints);
    *state |= AE_CTL_LONE_PBRANCH_DONE;
}

static inline void ae_ctl_pbranch_start(struct ae_ctl *ctl)
{
    ae_hints_dup(ctl->parent->hints, &ctl->hints);
    ae_mutex_lock(&ctl->parent->mutex);
    ctl->parent->posted++;
    ae_list_link_clear(&ctl->link);
    ae_list_add_back(&ctl->link, &ctl->parent->children);
    ae_mutex_unlock(&ctl->parent->mutex);
}

static inline enum ae_pwait_command ae_ctl_pbranch_done(struct ae_ctl *ctl, enum ae_ctl_state *state)
{
    /* lock parent mutex */
    ae_mutex_lock(&ctl->parent->mutex);
    /* remove myself from parent list (parent->children) */
    ae_list_del(&ctl->link);
    /* destroy my hints */
    ae_hints_destroy(ctl->hints);
    /* increment completed count of parent ctl */
    ctl->parent->completed++;

    *state |= AE_CTL_PBRANCH_DONE;
    if(ctl->parent->allposted && (ctl->parent->posted == ctl->parent->completed))
    {
        /* set state of this control so that callback can do the right thing */
        *state |= AE_CTL_PWAIT_DONE;
        /* tell pwait to continue */
        ae_mutex_unlock(&ctl->parent->mutex);
        return AE_PWAIT_CONTINUE;
    }

    ae_mutex_unlock(&ctl->parent->mutex);
    return AE_PWAIT_YIELD;
}

static inline enum ae_pwait_command ae_ctl_pwait_init_done(struct ae_ctl *ctl)
{
    ae_mutex_lock(&ctl->mutex);

    /* This has to be the initial pass through the pbranches
     * of the pwait.
     */
    assert(ctl->allposted == 0);
    ctl->allposted = 1;

    /* We know the posted field is set now, so we can
     * check If everything has completed already,
     * and return CONTINUE if so.
     */
    if(ctl->posted == ctl->completed)
    {
        ae_mutex_unlock(&ctl->mutex);
        return AE_PWAIT_CONTINUE;
    }

    /* Otherwise (not all pbranches have completed),
     * we need to tell the pwait to yield.
     */
    ae_mutex_unlock(&ctl->mutex);
    return AE_PWAIT_YIELD;
}

#endif
