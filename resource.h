#ifndef __RESOURCE_H__
#define __RESOURCE_H__

#include "src/common/triton-thread.h"
#include "src/common/triton-list.h"
#include "src/aesop/hints.h"

#define AE_MAX_RESOURCES 255
#define AE_MAX_CONTEXTS 1024

#define AE_RESOURCE_MASK (((uint64_t)0xFF)<<56)
#define AE_GET_RESOURCE_MASK(resource_id) (AE_RESOURCE_MASK&(((uint64_t)resource_id)<<56))
#define AE_GET_RESOURCE_ID(op_id) (((op_id&AE_RESOURCE_MASK)>>56)&0xFF)

/* ae_post_blocking allows us to post a blocking function from normal C code and if we're doing a syntax
 * check with the normal compiler (GCC), we don't get a 'too many arguments to function' error.
 */
#ifdef AESOP_PARSER
#define ae_post_blocking(__fname, __callback, __user_ptr, __hints, __resource_ctx, __op_id, __fargs...) \
    __fname(__callback, __user_ptr, __hints, __resource_ctx, __op_id, 0, ##__fargs)
#else
#define ae_post_blocking(__fname, __callback, __user_ptr, __hints, __resource_ctx, __op_id, __fargs...) TRITON_SUCCESS
#endif

#define ae_define_post(__ret_type, __fname, __fargs...) \
    triton_ret_t __fname(void (*__ae_callback)(void *ptr, __ret_type ret), \
                         void *__ae_user_ptr, \
                         ae_hints_t __ae_hints, \
                         ae_context_t __ae_ctx, \
                         ae_op_id_t *__ae_op_id, \
                         int __ae_internal, \
                         ##__fargs)

typedef struct ae_context *ae_context_t;

typedef uint128_t ae_op_id_t;

#define ae_op_id_equal(id1, id2) !memcmp(&id1, &id2, sizeof(ae_op_id_t))

ae_op_id_t ae_id_gen(int resource_id, uint64_t ptr);
uint64_t ae_id_lookup(ae_op_id_t id, int *resource_id);

/* The resource structure is defined by a given resource, and registered
 * to the aesop management code during resource initialization.
 */
struct ae_resource
{
    const char *resource_name;
    triton_ret_t (*test)(ae_op_id_t id, int ms_timeout);
    triton_ret_t (*poll_context)(ae_context_t context, int ms_timeout);
    triton_ret_t (*cancel)(ae_context_t ctx, ae_op_id_t id);
    triton_ret_t (*register_context)(ae_context_t context);
    triton_ret_t (*unregister_context)(ae_context_t context);
};

/* Called by resources to register themselves to the resource framework */
triton_ret_t ae_resource_register(struct ae_resource *resource, int *newid);
void ae_resource_unregister(int resource_id);

/* Contexts are created to allow separation of polling for different logical
 * groups of operations.
 */
triton_ret_t ae_context_create(ae_context_t *context, int resource_count, ...);

/* Context destruction.  Called to cleanup state allocated in ae_context_create.
 */
triton_ret_t ae_context_destroy(ae_context_t context);

/* Poll for completion of the operations within the given context up to a
 * timeout value.
 */
triton_ret_t ae_poll(ae_context_t context, int ms);

/* Cancel an operation */
triton_ret_t ae_cancel_op(ae_context_t context, ae_op_id_t op_id);

/* The ae_ctl structure is used by the aesop generated code to manage parallel and
 * nested operations.  This structure is not needed by resource writers or aesop 
 * code.
 * TODO: move these to a separate header
 */
struct ae_ctl
{
    const char *name;
    struct ae_ctl *parent;
    ae_op_id_t current_op_id;
    int cancelled;
    triton_mutex_t mutex;
    triton_list_t children;
    struct triton_list_link link;
    int posted;
    int completed;
    int allposted;
    int hit_pbreak;
    int in_pwait;    
    ae_hints_t hints;
    ae_context_t context;
    int refcount;
};

static inline void ae_ctl_init(struct ae_ctl *ctl,
                               const char *name,
                               ae_hints_t hints,
                               ae_context_t context,
                               int internal,
                               void *user_ptr)
{
    ctl->name = name;
    ctl->posted = 0;
    ctl->completed = 0;
    ctl->allposted = 0;
    ctl->hit_pbreak = 0;
    ctl->in_pwait = 0;
    ctl->hints = hints;
    ctl->context = context;
    ctl->cancelled = 0;
    triton_mutex_init(&ctl->mutex, NULL);
    triton_list_init(&ctl->children);
    ctl->refcount = 1;
    triton_uint128_setzero(ctl->current_op_id);
    if(internal) ctl->parent = (struct ae_ctl *)user_ptr;
    else ctl->parent = NULL;
}

static inline void ae_ctl_destroy(void *tctl, struct ae_ctl *ctl)
{
    free(tctl);
}

static inline int ae_ctl_refcount(struct ae_ctl *ctl)
{
    int rc;
    rc = ctl->refcount;
    return rc;
}

static inline int ae_ctl_refinc(struct ae_ctl *ctl)
{
    int rc;
    rc = ++ctl->refcount;
    return rc;
}

static inline int ae_ctl_refdec(struct ae_ctl *ctl)
{
    int rc;
    rc = --ctl->refcount;
    return rc;
}

/* internal function -- used by generated code */
triton_ret_t ae_cancel_children(ae_context_t ctx, struct ae_ctl *ctl);

void ae_backtrace(void);

void ae_lone_pbranches_add(struct ae_ctl *ctl);
void ae_lone_pbranches_remove(struct ae_ctl *ctl);
int ae_lone_pbranches_count(void);

triton_ret_t ae_cancel_branches(struct ae_ctl *ctl);

int ae_count_branches(struct ae_ctl *ctl);

#ifdef AESOP_PARSER
#define aesop_cancel_branches() ae_cancel_branches(ctl->parent ? &ctl->parent->gen : NULL)
#define aesop_count_branches() ae_count_branches(ctl->parent ? &ctl->parent->gen : NULL)
#else
static inline triton_ret_t aesop_cancel_branches(void) { return TRITON_ERR_NOSYS; }
static inline int aesop_count_branches(void) { return -1; }
#endif

static inline void ae_print_stack(FILE *outstream, struct ae_ctl *ctl)
{
    int i = 0, top;
    char *stack[512];
    do
    {
        stack[i++] = strdup(ctl->name);
        ctl = ctl->parent;
    } while(ctl);

    top = 0;
    for(--i; i >= 0; --i)
    {
        fprintf(outstream, "%d: %s\n", top++, stack[i]);
    }

    for(i = 0; i < top; ++i)
    {
        free(stack[i]);
    }
}
    
#define aesop_main_set(__main_blocking_function__, context_count, ...) \
static int __main_done=0; \
static int __main_ret; \
static void __main_cb(void *user_ptr, int t) \
{ \
      __main_done = 1; \
      __main_ret = t; \
} \
int main(int argc, char **argv)  \
{ \
    ae_context_t __main_ctx; \
    ae_op_id_t __main_opid; \
    triton_init(); \
    ae_context_create(&__main_ctx, context_count, __VA_ARGS__); \
    ae_post_blocking(__main_blocking_function__, __main_cb, NULL, NULL, __main_ctx, &__main_opid, argc, argv); \
    while(!__main_done) \
    { \
        ae_poll(__main_ctx, 0); \
    } \
    ae_context_destroy(__main_ctx); \
    triton_finalize(); \
    return __main_ret; \
}

#endif /* __AE_RESOURCE_H__ */
