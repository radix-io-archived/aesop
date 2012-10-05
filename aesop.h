/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#ifndef __AESOP_H__
#define __AESOP_H__

#include <aesop/triton-base.h>
#include <aesop/triton-list.h>
#include <aesop/triton-thread.h>
#include <aesop/ae-error.h>
#include <aesop/ae-log.h>
#include <aesop/ae-debug.h>
#include <aesop/ae-ctl.h>
#include <aesop/triton-uint128.h>

int aesop_init(const char *resource_list);
void aesop_finalize(void);

int aesop_set_config(const char* key, const char* value);
int aesop_set_debugging(const char* resource, int value);
int aesop_debug_from_env (void);

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

/* Called by c programs to break ae_poll() calls once callbacks are complete */
void ae_poll_break(ae_context_t context);

/* Contexts are created to allow separation of polling for different logical
 * groups of operations.  Don't use this function.  Instead, use the associated
 * ae_context_create macro.
 */
int _ae_context_create(ae_context_t *context, const char *format, int resource_count, ...) __attribute__((format (printf, 2, 4))) ;

/* macro to calculate the number of resources passed in as string 
 * arguments so we don't have to pass in the count explicitly. */
#define ae_context_create(_context, ...) \
    _ae_context_create(_context, FORMAT_ARGS(__VA_ARGS__) , ##__VA_ARGS__)

/* Context destruction.  Called to cleanup state allocated in ae_context_create.
 */
int ae_context_destroy(ae_context_t context);

/* Poll for completion of the operations within the given context up to a
 * timeout value.
 */
int ae_poll(ae_context_t context, int ms);

int ae_cancel_branches(struct ae_ctl *ctl);

int ae_count_branches(struct ae_ctl *ctl);

#ifdef AESOP_PARSER

#define aesop_cancel_branches() ae_cancel_branches(__ae_ctl->parent ? &__ae_ctl->parent->gen : NULL)
#define aesop_count_branches() ae_count_branches(__ae_ctl->parent ? &__ae_ctl->parent->gen : NULL)
#define aesop_clear_cancel () ae_clear_cancel (__ae_ctl ? &__ae_ctl->gen : NULL)

#else

/**
 * This function tries to cancel the enclosing pwait. (it can only be called
 * from within a pbranch of a pwait).
 *
 * It returns AE_SUCCESS if all resource cancel functions returned success,
 * and returns the last error returned by a cancel function otherwise.
 *
 * In addition, it marks each pbranch (and children) as cancelled, meaning
 * that every new posted operation should return immediately indicating
 * cancellation.
 */
static inline int aesop_cancel_branches(void) { return AE_ERR_SYSTEM; }

static inline int aesop_count_branches(void) { return -1; }

/**
 * This function clears the cancelled state of the caller.
 * It should not be called from within a pbranch.
 */
static inline void aesop_clear_cancel(void) { }

#endif /* AESOP_PARSER */

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
   aesop_main_set_with_init (0, "", __main_blocking_function__, ##__VA_ARGS__);

/* Similar to above, but this one takes an initialization function
 * that gets called before ae_init,
 * and a single string to specify the aesop module to initialize.
 *
 * Example:
 *
 * __blocking int aesop_main(int argc, char **argv) { ... }
 * int set_zeroconf_params(void) { ... }
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
    int ret;                                                      \
    int (*__main_init_func)(void) = __init_before_main__;         \
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
        aesop_error_assert(ret);                                 \
    }                                                             \
    ret = ae_hints_init(&__main_hints);                          \
    assert(ret == 0); \
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
            if(ret == AE_ERR_TIMEDOUT) continue;                  \
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

ae_op_id_t ae_id_gen(int resource_id, intptr_t ptr);
intptr_t ae_id_lookup(ae_op_id_t id, int *resource_id);

#endif

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
