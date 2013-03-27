/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#ifndef __AESOP_H__
#define __AESOP_H__

#include <triton-base.h>

#include <triton-list.h>
#include <triton-thread.h>
#include <triton-uint128.h>

#include <aesop/ae-error.h>
#include <aesop/ae-log.h>
#include <aesop/ae-debug.h>
#include <aesop/ae-ctl.h>

int aesop_init(void);
void aesop_finalize(void);

int aesop_set_debugging(const char* resource, int value);
int aesop_debug_from_env (void);

/* ae_post_blocking allows us to post a blocking function from normal C code and if we're doing a syntax
 * check with the normal compiler (GCC), we don't get a 'too many arguments to function' error.
 */
#ifdef AESOP_PARSER
#define ae_post_blocking(__fname, __callback, __user_ptr, __hints, __op_id, __fargs...) \
    __fname(__callback, __user_ptr, __hints, __op_id, 0, ##__fargs)
#else
#define ae_post_blocking(__fname, __callback, __user_ptr, __hints, __op_id, __fargs...) AE_SUCCESS
#endif

#define ext_post_blocking(__fname, __callback, __user_ptr, __hints, __op_id, __fargs...) \
    __fname(__callback, __user_ptr, __hints, __op_id, 0, ##__fargs)

/* Called by c programs to break ae_poll() calls once callbacks are complete */
void ae_poll_break(void);

/* Poll for completion of operations up to a timeout value.
 */
int ae_poll(int ms);

int ae_cancel_branches(struct ae_ctl *ctl);

int ae_count_branches(struct ae_ctl *ctl);

#ifdef AESOP_PARSER

#define aesop_cancel_branches() ae_cancel_branches(__ae_ctl->parent ? &__ae_ctl->parent->gen : NULL)
#define aesop_count_branches() ae_count_branches(__ae_ctl->parent ? &__ae_ctl->parent->gen : NULL)
#define aesop_clear_cancel() ae_clear_cancel (__ae_ctl ? &__ae_ctl->gen : NULL)
#define aesop_set_cancel() ae_set_cancel (__ae_ctl ? &__ae_ctl->gen : NULL)

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

static inline void aesop_set_cancel (void) { }


#endif /* AESOP_PARSER */

/* Set this to zero to cause the main_set macros to busy spin on ae_poll(),
 * although this probably is not a good idea.
 */
#define AESOP_MAIN_SET_POLL_TIMEOUT 100

/* A macro to implement the boilerplate main function that posts an initial
 * blocking function.  The blocking function should have the same signature as main().
 *
 * Example:
 * __blocking int aesop_main(int argc, char **argv) { ... }
 * aesop_main_set(aesop_main);
 */
#define aesop_main_set(__main_blocking_function__)        \
   aesop_main_set_with_init (0, __main_blocking_function__);

/* Similar to above, but this one takes an initialization function
 * that gets called before aesop_init.
 *
 * Example:
 *
 * __blocking int aesop_main(int argc, char **argv) { ... }
 * int set_zeroconf_params(void) { ... }
 * aesop_main_set_with_init(set_zeroconf_params, aesop_main);
 */
#define aesop_main_set_with_init(__init_before_main__,            \
                                 __main_blocking_function__, ...) \
static int __main_done=0;                                         \
static int __main_ret;                                            \
static void __main_cb(void *user_ptr, int t)                      \
{                                                                 \
      __main_done = 1;                                            \
      __main_ret = t;                                             \
      ae_poll_break();                                         \
}                                                                 \
int main(int argc, char **argv)                                   \
{                                                                 \
    ae_hints_t __main_hints;                                      \
    ae_op_id_t __main_opid;                                       \
    int ret;                                                      \
    int (*__main_init_func)(int argc, char** argv) = __init_before_main__; \
    if(__main_init_func)                                      \
    {                                                             \
        ret = __main_init_func(argc, argv);                       \
        aesop_error_assert(ret);                                  \
    }                                                             \
    ret = aesop_init();                          \
    aesop_error_assert(ret);                                      \
    ret = ae_hints_init(&__main_hints);                          \
    assert(ret == 0); \
    ret = ae_post_blocking(                                       \
        __main_blocking_function__,                               \
        __main_cb,                                                \
        NULL,                                               \
        &__main_hints,                                            \
        &__main_opid,                                             \
        &__main_ret,                                              \
        argc,                                                     \
        argv);                                                    \
    if(ret == AE_SUCCESS)                                         \
    {                                                             \
        while(!__main_done)                                       \
        {                                                         \
            ret = ae_poll(AESOP_MAIN_SET_POLL_TIMEOUT); \
            if(ret == AE_ERR_TIMEDOUT) continue;                  \
            aesop_error_assert(ret);                              \
        }                                                         \
    }                                                             \
    if (ae_lone_pbranches_count ())                               \
    {                                                               \
       fprintf (stderr,"\n\n!! WARNING !!"                          \
             "Waiting for %i lonely pbranches!!\n\n",       \
             ae_lone_pbranches_count ());      \
       while(ae_lone_pbranches_count ())                             \
       {                                                               \
          ret = ae_poll(AESOP_MAIN_SET_POLL_TIMEOUT);     \
          if(ret == AE_ERR_TIMEDOUT) continue;                         \
          aesop_error_assert(ret);                                    \
       }                                                               \
    }                                                         \
    aesop_error_assert(ret);                                      \
    ae_hints_destroy(&__main_hints);                              \
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
