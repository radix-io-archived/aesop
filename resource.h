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

#define ext_post_blocking(__fname, __callback, __user_ptr, __hints, __resource_ctx, __op_id, __fargs...) \
    __fname(__callback, __user_ptr, __hints, __resource_ctx, __op_id, 0, ##__fargs)

#define ae_define_post(__ret_type, __fname, __fargs...)                    \
    triton_ret_t __fname(void (*__ae_callback)(void *ptr, __ret_type ret), \
                         void *__ae_user_ptr,                              \
                         ae_hints_t *__ae_hints,                           \
                         ae_context_t __ae_ctx,                            \
                         ae_op_id_t *__ae_op_id,                           \
                         int __ae_internal,                                \
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
    triton_ret_t (*poll_context)(ae_context_t context);
    triton_ret_t (*cancel)(ae_context_t ctx, ae_op_id_t id);
    triton_ret_t (*register_context)(ae_context_t context);
    triton_ret_t (*unregister_context)(ae_context_t context);
};

/* Called by resources to register themselves to the resource framework */
triton_ret_t ae_resource_register(struct ae_resource *resource, int *newid);
void ae_resource_unregister(int resource_id);
void ae_resource_request_poll(ae_context_t context, int resource_id);
#ifdef __AESOP_LIBEV
/* this function is used by resources that want access to the event loop
 * used by aesop for this context
 */
struct ev_loop * ae_resource_get_eloop(ae_context_t context);
#endif

/* Contexts are created to allow separation of polling for different logical
 * groups of operations.  Don't use this function.  Instead, use the associated
 * ae_context_create macro.
 */
triton_ret_t _ae_context_create(ae_context_t *context, const char *format, int resource_count, ...) __attribute__((format (printf, 2, 4))) ;

/* macro to calculate the number of resources passed in as string 
 * arguments so we don't have to pass in the count explicitly. */
#define ae_context_create(_context, ...) \
    _ae_context_create(_context, FORMAT_ARGS(__VA_ARGS__) , ##__VA_ARGS__)

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
    ae_hints_t *hints;
    ae_context_t context;
    int refcount;
};

static inline void ae_ctl_init(struct ae_ctl *ctl,
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
    ctl->hit_pbreak = 0;
    ctl->in_pwait = 0;

    /* by default, we just set the hints pointer, assuming the lifetime
     * of the hint pointer passed in will live for the entire blocking call
     * this control structure is allocated for
     */
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

void ae_get_stack(struct ae_ctl *ctl, triton_string_t *stack, int *inout_count);

#ifdef AESOP_PARSER
#define aesop_cancel_branches() ae_cancel_branches(ctl->parent ? &ctl->parent->gen : NULL)
#define aesop_count_branches() ae_count_branches(ctl->parent ? &ctl->parent->gen : NULL)
#else
static inline triton_ret_t aesop_cancel_branches(void) { return TRITON_ERR_NOSYS; }
static inline int aesop_count_branches(void) { return -1; }
#endif

void ae_print_stack(FILE *outstream, struct ae_ctl *ctl);

#ifdef AESOP_PARSER
#define aesop_get_stack(stack, io_count) ae_get_stack(&ctl->gen, stack, io_count)
#else
static inline void aesop_get_stack(triton_string_t *strings, int *inout_count) { *inout_count = 0; }
#endif

#ifdef AESOP_PARSER
#define aesop_print_stack(stream) ae_print_stack(stream, &ctl->gen)
#else
static inline void aesop_print_stack(FILE *stream) { }
#endif

triton_ret_t ae_error_wrap_stack(struct ae_ctl *ctl, triton_ret_t);

#if defined(AESOP_PARSER) && !defined(NDEBUG)
#define aesop_error_wrap_stack(__ret) ae_error_wrap_stack(&ctl->gen, __ret)
#else
static inline triton_ret_t aesop_error_wrap_stack(triton_ret_t ret) { return ret; }
#endif

/* A macro to implement the boilerplate main function that posts an initial
 * blocking function.  The blocking function should have the same signature as main().
 * The variable parameters are the const char * names of the resources to initialize
 * for the context.  In this macro, triton_init() is called with no parameters, so
 * all modules get initialized.
 *
 * Example:
 * __blocking int aesop_main(int argc, char **argv) { ... }
 * aesop_main_set(aesop_main, "timer", "bdb", "sched");
 */
#define aesop_main_set(__main_blocking_function__, ...) \
static int __main_done=0; \
static int __main_ret; \
static void __main_cb(void *user_ptr, int t) \
{ \
      __main_done = 1; \
      __main_ret = t; \
} \
int main(int argc, char **argv)  \
{ \
    ae_context_t __main_ctx = NULL; \
    ae_hints_t __main_hints; \
    ae_op_id_t __main_opid; \
    triton_ret_t ret; \
    ret = triton_init(); \
    triton_error_assert(ret); \
    if(COUNT_ARGS(__VA_ARGS__) > 0){ \
        ret = ae_context_create(&__main_ctx, ##__VA_ARGS__); \
        triton_error_assert(ret); \
    } \
    ret = ae_hints_init(&__main_hints); \
    ret = ae_post_blocking(__main_blocking_function__, __main_cb, NULL, &__main_hints, __main_ctx, &__main_opid, argc, argv); \
    triton_error_assert(ret); \
    while(!__main_done) \
    { \
        ret = ae_poll(__main_ctx, 10); \
        if(ret == TRITON_ERR_TIMEDOUT) continue; \
        triton_error_assert(ret); \
    } \
    ae_hints_destroy(&__main_hints); \
    if(COUNT_ARGS(__VA_ARGS__) > 0){ \
        ae_context_destroy(__main_ctx); \
    } \
    triton_finalize(); \
    return __main_ret; \
}

/* Similar to above, but this one takes an initialization function
 * that gets called before triton_init,
 * and a single string to specify the triton module to initialize.
 *
 * Example:
 *
 * __blocking int aesop_main(int argc, char **argv) { ... }
 * triton_ret_t set_zeroconf_params(void) { ... }
 * aesop_main_set_with_init(set_zeroconf_params, "triton.client", aesop_main, "bdb", "file");
 */
#define aesop_main_set_with_init(__init_before_triton__, \
                                 __init_module__, \
                                 __main_blocking_function__, ...) \
static int __main_done=0; \
static int __main_ret; \
static void __main_cb(void *user_ptr, int t) \
{ \
      __main_done = 1; \
      __main_ret = t; \
} \
int main(int argc, char **argv)  \
{ \
    ae_context_t __main_ctx = NULL; \
    ae_hints_t __main_hints; \
    ae_op_id_t __main_opid; \
    triton_ret_t ret; \
    triton_ret_t (*__main_init_func)(int, char**) = __init_before_triton__; \
    if(__init_before_triton__) \
    { \
        ret = __main_init_func(argc, argv); \
        triton_error_assert(ret); \
    } \
    ret = triton_init(__init_module__); \
    triton_error_assert(ret); \
    if(COUNT_ARGS(__VA_ARGS__) > 0){ \
        ret = ae_context_create(&__main_ctx, ##__VA_ARGS__); \
        triton_error_assert(ret); \
    } \
    triton_error_assert(ret); \
    ret = ae_hints_init(&__main_hints); \
    ret = ae_post_blocking( \
        __main_blocking_function__, \
        __main_cb, \
        NULL, \
        &__main_hints, \
        __main_ctx, \
        &__main_opid, \
        argc, \
        argv); \
    triton_error_assert(ret); \
    while(!__main_done) \
    { \
        ret = ae_poll(__main_ctx, 10); \
        if(ret == TRITON_ERR_TIMEDOUT) continue; \
        triton_error_assert(ret); \
    } \
    ae_hints_destroy(&__main_hints); \
    if(COUNT_ARGS(__VA_ARGS__) > 0){ \
        ae_context_destroy(__main_ctx); \
    } \
    triton_finalize(); \
    return __main_ret; \
}
#endif /* __AE_RESOURCE_H__ */

/*
 * Local Variables:
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
