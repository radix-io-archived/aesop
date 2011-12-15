#ifndef __RESOURCE_H__
#define __RESOURCE_H__

#include "src/aesop/ae-types.h"
#include "src/aesop/ae-thread.h"
#include "src/aesop/ae-list.h"
#include "src/aesop/ae-error.h"
#include "src/aesop/ae-ctl.h"
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
#define ae_post_blocking(__fname, __callback, __user_ptr, __hints, __resource_ctx, __op_id, __fargs...) AE_SUCCESS
#endif

#define ext_post_blocking(__fname, __callback, __user_ptr, __hints, __resource_ctx, __op_id, __fargs...) \
    __fname(__callback, __user_ptr, __hints, __resource_ctx, __op_id, 0, ##__fargs)

#define ae_define_post(__ret_type, __fname, __fargs...)                    \
    ae_ret_t __fname(void (*__ae_callback)(void *ptr, __ret_type ret), \
                         void *__ae_user_ptr,                              \
                         ae_hints_t *__ae_hints,                           \
                         ae_context_t __ae_ctx,                            \
                         ae_op_id_t *__ae_op_id,                           \
                         int __ae_internal,                                \
                         __ret_type *__ae_retval,                          \
                         ##__fargs)

int ae_check_debug_flag(int resource_id);

#define ae_debug(__resource_id, __format, ...) \
    if(ae_check_debug_flag(__resource_id)) fprintf(stderr, __format, ## __VA_ARGS__)

ae_op_id_t ae_id_gen(int resource_id, intptr_t ptr);
intptr_t ae_id_lookup(ae_op_id_t id, int *resource_id);

/* this structure defines a configuration parameter for a resource */
struct ae_resource_config
{
    const char* name;
    const char* default_value;
    const char* description;
    int (*updater)(const char* key, const char* value);
};

/* The resource structure is defined by a given resource, and registered
 * to the aesop management code during resource initialization.
 */
struct ae_resource
{
    const char *resource_name;
    ae_ret_t (*test)(ae_op_id_t id, int ms_timeout);
    ae_ret_t (*poll_context)(ae_context_t context);
    ae_ret_t (*cancel)(ae_context_t ctx, ae_op_id_t id);
    ae_ret_t (*register_context)(ae_context_t context);
    ae_ret_t (*unregister_context)(ae_context_t context);
    struct ae_resource_config* config_array;  /* terminated by entry with NULL name */
};

/* called to register the initialization and finalization methods for a
 * resource with the aesop framework.  Does not activate the resource.
 */
triton_ret_t ae_resource_init_register(const char* resource_name, 
    triton_ret_t (*init)(void),
    void (*finalize)(void));

/* initializes the resource named in the argument.  This function is called
 * by aesop itself when it wants to activate a resource. 
 */
triton_ret_t ae_resource_init(const char* resource);
triton_ret_t ae_resource_init_all(void);

/* Called by resources to register themselves to the resource framework once
 * they are initialized and ready for use by aesop */
ae_ret_t ae_resource_register(struct ae_resource *resource, int *newid);
void ae_resource_unregister(int resource_id);

/* Called by resources to request polling from the event loop */
void ae_resource_request_poll(ae_context_t context, int resource_id);
/* Called by c programs to break ae_poll() calls once callbacks are complete */
void ae_poll_break(ae_context_t context);
/* this function is used by resources that want access to the event loop
 * used by aesop for this context
 */
struct ev_loop * ae_resource_get_eloop(ae_context_t context);

/* Contexts are created to allow separation of polling for different logical
 * groups of operations.  Don't use this function.  Instead, use the associated
 * ae_context_create macro.
 */
ae_ret_t _ae_context_create(ae_context_t *context, const char *format, int resource_count, ...) __attribute__((format (printf, 2, 4))) ;

/* macro to calculate the number of resources passed in as string 
 * arguments so we don't have to pass in the count explicitly. */
#define ae_context_create(_context, ...) \
    _ae_context_create(_context, FORMAT_ARGS(__VA_ARGS__) , ##__VA_ARGS__)

/* Context destruction.  Called to cleanup state allocated in ae_context_create.
 */
ae_ret_t ae_context_destroy(ae_context_t context);

/* Poll for completion of the operations within the given context up to a
 * timeout value.
 */
ae_ret_t ae_poll(ae_context_t context, int ms);

/* Cancel an operation */
ae_ret_t ae_cancel_op(ae_context_t context, ae_op_id_t op_id);

void ae_backtrace(void);

ae_ret_t ae_cancel_branches(struct ae_ctl *ctl);

int ae_count_branches(struct ae_ctl *ctl);

void ae_get_stack(struct ae_ctl *ctl, ae_string_t *stack, int *inout_count);

#ifdef AESOP_PARSER
#define aesop_cancel_branches() ae_cancel_branches(__ae_ctl->parent ? &__ae_ctl->parent->gen : NULL)
#define aesop_count_branches() ae_count_branches(__ae_ctl->parent ? &__ae_ctl->parent->gen : NULL)
#else
static inline ae_ret_t aesop_cancel_branches(void) { return AE_NOSYS; }
static inline int aesop_count_branches(void) { return -1; }
#endif

void ae_print_stack(FILE *outstream, struct ae_ctl *ctl);

#ifdef AESOP_PARSER
#define aesop_get_stack(stack, io_count) ae_get_stack(&__ae_ctl->gen, stack, io_count)
#else
static inline void aesop_get_stack(ae_string_t *strings, int *inout_count)
{
    *inout_count = 0;
}
#endif

#ifdef AESOP_PARSER
#define aesop_print_stack(stream) ae_print_stack(stream, &__ae_ctl->gen)
#else
static inline void aesop_print_stack(FILE *stream) { }
#endif

ae_ret_t ae_error_wrap_stack(struct ae_ctl *ctl, ae_ret_t);

#if defined(AESOP_PARSER) && !defined(NDEBUG)
#define aesop_error_wrap_stack(__ret) ae_error_wrap_stack(&__ae_ctl->gen, __ret)
#else
static inline ae_ret_t aesop_error_wrap_stack(ae_ret_t ret) { return ret; }
#endif

/* Set this to zero to cause the main_set macros to busy spin on ae_poll(),
 * although this probably is not a good idea.
 */
#define AESOP_MAIN_SET_POLL_TIMEOUT 100

/* A macro to implement the boilerplate main function that posts an initial
 * blocking function.  The blocking function should have the same signature as main().
 * The variable parameters are the const char * names of the resources to initialize
 * for the context.  In this macro, ae_init() is called with no parameters, so
 * all modules get initialized.
 *
 * Example:
 * __blocking int aesop_main(int argc, char **argv) { ... }
 * aesop_main_set(aesop_main, "timer", "bdb", "sched");
 */
#define aesop_main_set(__main_blocking_function__, ...)        \
static int __main_done=0;                                      \
static int __main_ret;                                         \
static void __main_cb(void *user_ptr, int t)                   \
{                                                              \
      ae_context_t ctx = (ae_context_t)user_ptr;               \
      __main_done = 1;                                         \
      __main_ret = t;                                          \
      ae_poll_break(ctx);                                      \
}                                                              \
int main(int argc, char **argv)                                \
{                                                              \
    ae_context_t __main_ctx = NULL;                            \
    ae_hints_t __main_hints;                                   \
    ae_op_id_t __main_opid;                                    \
    ae_ret_t ret;                                              \
    ret = aesop_init("");                                      \
    aesop_error_assert(ret);                                   \
    if(COUNT_ARGS(__VA_ARGS__) > 0)                            \
    {                                                          \
        ret = ae_context_create(&__main_ctx, ##__VA_ARGS__);   \
        triton_error_assert(ret);                              \
    }                                                          \
    ret = ae_hints_init(&__main_hints);                        \
    ret = ae_post_blocking(                                    \
        __main_blocking_function__,                            \
        __main_cb,                                             \
        __main_ctx,                                            \
        &__main_hints,                                         \
        __main_ctx,                                            \
        &__main_opid,                                          \
        &__main_ret,                                           \
        argc,                                                  \
        argv);                                                 \
    if(ret == AE_SUCCESS)                                      \
    {                                                          \
        while(!__main_done)                                    \
        {                                                      \
            ret = ae_poll(__main_ctx, AESOP_MAIN_SET_POLL_TIMEOUT); \
            if(ret == AE_TIMEDOUT) continue;                   \
            aesop_error_assert(ret);                           \
        }                                                      \
    }                                                          \
    aesop_error_assert(ret);                                   \
    ae_hints_destroy(&__main_hints);                           \
    if(COUNT_ARGS(__VA_ARGS__) > 0)                            \
    {                                                          \
        ae_context_destroy(__main_ctx);                        \
    }                                                          \
    aesop_finalize();                                          \
    return __main_ret;                                         \
}

/* Similar to above, but this one takes an initialization function
 * that gets called before ae_init,
 * and a single string to specify the aesop module to initialize.
 *
 * Example:
 *
 * __blocking int aesop_main(int argc, char **argv) { ... }
 * ae_ret_t set_zeroconf_params(void) { ... }
 * aesop_main_set_with_init(set_zeroconf_params, "aesop.client", aesop_main, "bdb", "file");
 */
#define aesop_main_set_with_init(__init_before_main__,            \
                                 __resource_list__,               \
                                 __main_blocking_function__, ...) \
static int __main_done=0;                                         \
static int __main_ret;                                            \
static void __main_cb(void *user_ptr, int t)                      \
{                                                                 \
      ae_context_t ctx = (ae_context_t)user_ptr;                  \
      __main_done = 1;                                            \
      __main_ret = t;                                             \
      ae_poll_break(ctx);                                         \
}                                                                 \
int main(int argc, char **argv)                                   \
{                                                                 \
    ae_context_t __main_ctx = NULL;                               \
    ae_hints_t __main_hints;                                      \
    ae_op_id_t __main_opid;                                       \
    ae_ret_t ret;                                                 \
    ae_ret_t (*__main_init_func)(void) = __init_before_main__;    \
    if(__init_before_main__)                                      \
    {                                                             \
        ret = __main_init_func();                                 \
        aesop_error_assert(ret);                                  \
    }                                                             \
    ret = aesop_init(__resource_list__);                          \
    aesop_error_assert(ret);                                      \
    if(COUNT_ARGS(__VA_ARGS__) > 0)                               \
    {                                                             \
        ret = ae_context_create(&__main_ctx, ##__VA_ARGS__);      \
        triton_error_assert(ret);                                 \
    }                                                             \
    ret = ae_hints_init(&__main_hints);                           \
    ret = ae_post_blocking(                                       \
        __main_blocking_function__,                               \
        __main_cb,                                                \
        __main_ctx,                                               \
        &__main_hints,                                            \
        __main_ctx,                                               \
        &__main_opid,                                             \
        &__main_ret,                                              \
        argc,                                                     \
        argv);                                                    \
    if(ret == AE_SUCCESS)                                         \
    {                                                             \
        while(!__main_done)                                       \
        {                                                         \
            ret = ae_poll(__main_ctx, AESOP_MAIN_SET_POLL_TIMEOUT); \
            if(ret == AE_TIMEDOUT) continue;                      \
            aesop_error_assert(ret);                              \
        }                                                         \
    }                                                             \
    aesop_error_assert(ret);                                      \
    ae_hints_destroy(&__main_hints);                              \
    if(COUNT_ARGS(__VA_ARGS__) > 0)                               \
    {                                                             \
        ae_context_destroy(__main_ctx);                           \
    }                                                             \
    aesop_finalize();                                             \
    return __main_ret;                                            \
}
#endif /* __AE_RESOURCE_H__ */

/*
 * Local Variables:
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
