/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#ifndef __AE_CTL_H__
#define __AE_CTL_H__

#include <aesop/triton-thread.h>
#include <aesop/triton-list.h>
#include <aesop/ae-types.h>
#include <aesop/ae-error.h>
#include <aesop/hints.h>

#include <opa_primitives.h>

enum ae_ctl_state
{
    /* inputs for worker functions */
    AE_CTL_START = 1,

    /**
     * AE_CTL_CALL_COMPLETE indicates that a blocking call made from within
     * this blocking function completed, and that execution can continue at
     * the label specified in __ae_ctl->gen.state_label.
     */
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
    AE_PBRANCH_INIT,
    AE_PBRANCH_INPROGRESS,
    AE_PBRANCH_DONE
};

/**
 * 
 */ 
enum ae_pwait_command
{
    AE_PWAIT_NONE = 0,
    AE_PWAIT_CONTINUE,
    AE_PWAIT_YIELD
};

#define ae_pwait_command_string(__cmd)                                \
    (__cmd == AE_PWAIT_NONE) ?                                        \
        "NONE" : ((__cmd == AE_PWAIT_CONTINUE) ? "CONTINUE" :         \
                  ((__cmd == AE_PWAIT_YIELD) ? "YIELD" : "UNKNOWN"))

typedef struct ae_context *ae_context_t;

enum op_state
{
   /** This context is cancelled, all ops should return immediately */
   OP_CANCELLED   = 0x01,

   /** Indicates that the op logically completed even though
    * callback might not have been called yet.
    * It does mean it can no longer be cancelled. */
   OP_COMPLETED_NORMAL = 0x02,
   
   /**
    * A cancel request for the op was made; This does not mean that the
    * resource->cancel has been called, simply that it will be called in the
    * near future and that any attempt to normally complete this op will fail.
    */
   OP_COMPLETED_CANCELLED = 0x04,

   /**
    * Indicates if the op logically completed (either normal or cancel).
    */
   OP_COMPLETED = OP_COMPLETED_NORMAL | OP_COMPLETED_CANCELLED,

   /**
    * This flag indicates that the ctl structure is in a cancelled state,
    * meaning any newly posted operations should cancel immediately.
    */
   OP_REQUEST_CANCEL = 0x08
};

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
    triton_mutex_t mutex;
    triton_list_t children;
    struct triton_list_link link;
    int posted;
    int completed;
    int allposted;
    int in_pwait;
    ae_hints_t *hints;
    ae_context_t context;
    OPA_int_t refcount;
    int op_state;
};

/**
 * We keep track of lone pbranches using a global list.
 * These functions manipulate that list safely.
 */
void ae_lone_pbranches_add(struct ae_ctl *ctl);
void ae_lone_pbranches_remove(struct ae_ctl *ctl);
int ae_lone_pbranches_count(void);

/**
 * Initialize ctl structure.
 * Sets the reference count to 1.
 */
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
    ctl->op_state = 0;

    /* by default, we just set the hints pointer, assuming the lifetime
     * of the hint pointer passed in will live for the entire blocking call
     * this control structure is allocated for
     */
    ctl->hints = hints;

    ctl->context = context;
    triton_mutex_init(&ctl->mutex, NULL);
    triton_list_init(&ctl->children);
    OPA_store_int (&ctl->refcount, 1);
    ae_op_id_clear(ctl->current_op_id);

    if (internal)
        ctl->parent = (struct ae_ctl *)user_ptr;
            else
        ctl->parent = NULL;

    ctl->spec_ctl = fctl;

    //printf (stderr, "ae_ctl_init(%s) parent=(%s) ref=1\n",
    //      ctl->name, ctl->parent ? ctl->parent->name : "none");
}

static inline void ae_ctl_destroy(struct ae_ctl *ctl)
{
    free (ctl);
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
    //fprintf (stderr, "ae_ctl_addref(%s): new ref: %i\n",
     //    ctl->name, rc);
    return rc;
}

static inline void ae_ctl_addref(struct ae_ctl *ctl)
{
#if 0
   OPA_incr_int (&ctl->refcount);
#endif
   ae_ctl_refinc (ctl);
}

/**
 * Atomically decrement refcount and return the new value
 */
static inline int ae_ctl_refdec(struct ae_ctl *ctl)
{
    int rc;
    rc = OPA_fetch_and_decr_int (&ctl->refcount) - 1;
    //fprintf (stderr, "ae_ctl_refdec(%s): new ref: %i\n",
    //      ctl->name, rc);
    return rc;
}

static inline void ae_ctl_done(struct ae_ctl *ctl)
{
   int refcount;
   //void *fctl = ctl->spec_ctl;

   refcount = ae_ctl_refdec(ctl);
   assert(refcount >= 0);
   if(refcount == 0)
   {
      ae_ctl_destroy (ctl);
   }
}

static inline void ae_ctl_pwait_start(struct ae_ctl *ctl)
{
    ctl->in_pwait = 1;
    ctl->posted = 0;
    ctl->completed = 0;
    ctl->allposted = 0;
    assert(triton_list_count(&ctl->children) == 0);
}

static inline void ae_ctl_lone_pbranch_start(struct ae_ctl *ctl)
{
    /* the lonely pbranch takes over the cancelled state from the parent */
    triton_list_link_clear(&ctl->link);
    ae_lone_pbranches_add(ctl);
    /* parent context disappears, we need to copy the hint
     *   ^^^ is this correct? parent ref is incremented? */
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

/**
 * First locking the parent makes sure that no pbranches can start or stop
 * while the parent is locked.
 */
static inline void ae_ctl_pbranch_start(struct ae_ctl *ctl)
{
    triton_mutex_lock(&ctl->parent->mutex);

    ae_hints_dup(ctl->parent->hints, &ctl->hints);

    /* take over cancelled status from parent, in case we're creating a branch
     * at the same time a cancel is ongoing */
    ctl->op_state |= (ctl->parent->op_state & OP_REQUEST_CANCEL);

    ctl->parent->posted++;
    triton_list_link_clear(&ctl->link);
    triton_list_add_back(&ctl->link, &ctl->parent->children);

    triton_mutex_unlock(&ctl->parent->mutex);
}

static inline enum ae_pwait_command ae_ctl_pbranch_done(struct ae_ctl *ctl, enum ae_ctl_state *state)
{
    /* lock parent mutex */
    triton_mutex_lock(&ctl->parent->mutex);
    /* remove myself from parent list (parent->children) */
    triton_list_del(&ctl->link);
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
        triton_mutex_unlock(&ctl->parent->mutex);
        return AE_PWAIT_CONTINUE;
    }

    triton_mutex_unlock(&ctl->parent->mutex);
    return AE_PWAIT_YIELD;
}

static inline enum ae_pwait_command ae_ctl_pwait_init_done(struct ae_ctl *ctl)
{
    triton_mutex_lock(&ctl->mutex);

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
        triton_mutex_unlock(&ctl->mutex);
        return AE_PWAIT_CONTINUE;
    }

    /* Otherwise (not all pbranches have completed),
     * we need to tell the pwait to yield.
     */
    triton_mutex_unlock(&ctl->mutex);
    return AE_PWAIT_YIELD;
}

#endif

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
