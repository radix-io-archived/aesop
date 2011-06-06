#ifndef __AE_BLOCKING_PARSER_H__
#define __AE_BLOCKING_PARSER_H__

/**
 * Defines macros that get used by the AESOP parser to generate C code from
 * aesop files.
 * Each macro translates a set of parameters into
 * either a C statement, a C declaration, or a C expression.
 */

#include "src/aesop/aesop.h"
#include "src/aesop/ae-error.h"
#include "src/aesop/ae-log.h"
#include "src/aesop/ae-debug.h"

#define AE_MK_START_OF_BLOCKING(__fname__) \
    ae_debug_blocking("[START]: %s (%p)\n", #__fname__, __ae_ctl);

#define AE_MK_END_OF_BLOCKING(__fname__, __string) \
    ae_debug_blocking("[END%s]: %s (%p)\n", __string, #__fname__, __ae_ctl);

#define AE_MK_PARENT_POINTER_DECL(__parent) \
    struct __parent##_ctl *parent;

#define AE_MK_BLOCKING_PARAMS_FOR_STRUCT_DECLS() \
    struct ae_ctl gen; \
    void *user_ptr;

#define AE_MK_BLOCKING_PARAMS_FUN_PTR_DECLS() \
    void *__ae_user_ptr; \
    ae_hints_t *__ae_hints; \
    ae_context_t __ae_context; \
    ae_op_id_t *__ae_op_id; \
    int __ae_internal;
    /* void (*callback) (void *user_ptr [, __ret_type __ae_ret]); */


#define AE_MK_CB_FN_START(__fname)                     \
    ae_ret_t cbret;                                    \
    enum ae_ctl_state state;                           \
    struct __fname##_ctl *__ae_ctl;                    \
    struct __fname##_ctl *__ae_ctl_done __unused__;    \
                                                       \
    __ae_ctl = (struct __fname##_ctl *)__ae_ptr;       \
    ae_mutex_lock(&__ae_ctl->gen.mutex);               \
    state = AE_CTL_CALL_COMPLETE;                      \
    ae_op_id_clear(__ae_ctl->gen.current_op_id);       \
    ae_mutex_unlock(&__ae_ctl->gen.mutex);
 
#define AE_MK_CB_FN_INVOKE_WORKER(__bfun, __fname, __pos_str) \
    cbret = __ae_worker_##__bfun(__ae_ctl, &state);           \
    if(cbret != AE_SUCCESS)                                   \
    {                                                         \
        aesop_err(                                            \
            "INVALID STATE: "                                 \
            "blocking function did not return "               \
            "AE_SUCCESS (posted) or AE_IMMEDIATE_COMPLETION " \
            "in function: %s, trace exiting\n",               \
            #__fname);                                        \
        aesop_error_assert(cbret);                            \
        return;                                               \
    }                                                         \
                                                              \
    if(state & AE_CTL_PWAIT_DONE)                             \
    {                                                         \
        __ae_ctl_done = __ae_ctl;                             \
        __ae_ctl = __ae_ctl->parent;                          \
        ae_debug_pbranch("ctl_done: pwait_done: %p\n", __ae_ctl_done); \
        ae_ctl_done(&__ae_ctl_done->gen);                     \
    }                                                         \
    else if(state & AE_CTL_PBRANCH_DONE ||                    \
            state & AE_CTL_LONE_PBRANCH_DONE)                 \
    {                                                         \
        ae_debug_pbranch("ctl_done: pbranch_done: %p\n", __ae_ctl); \
        ae_ctl_done(&__ae_ctl->gen);                          \
        return;                                               \
    }
 
#define AE_MK_DONE_DECLS()                       \
        void * __ae_local_up;                    \
        int __ae_refcount;                       \
        __ae_local_cb = __ae_ctl->__ae_callback; \
        __ae_local_up = __ae_ctl->user_ptr;
 
#define AE_MK_CALLBACK_FN(__bfun, __fname, __pos_str, __fret_type, __cb_ret_type, __cb_ret_param)     \
static void __bfun##_##__fname##_##__pos_str##_callback(void *__ae_ptr, __cb_ret_type __cb_ret_param) \
{                                                                                                     \
    AE_MK_CB_FN_START(__bfun);                                                                        \
                                                                                                      \
    __ae_ctl->return_params.__cb_ret_param = __cb_ret_param;                                          \
                                                                                                      \
    AE_MK_CB_FN_INVOKE_WORKER(__bfun, __fname, __pos_str);                                            \
                                                                                                      \
    /* if the blocking function has completed, call its callback */                                   \
    if(state & AE_CTL_FN_COMPLETE)                                                                    \
    {                                                                                                 \
        void (* __ae_local_cb)(void *, __fret_type);                                                  \
        __fret_type __ae_ret = __ae_ctl->return_value;                                                \
        AE_MK_DONE_DECLS();                                                                           \
                                                                                                      \
        AE_MK_END_OF_BLOCKING(__bfun, "");                                                            \
        ae_debug_pbranch("ctl_done: cb: fn_complete: %p\n", __ae_ctl);                                \
        __ae_local_cb(__ae_local_up, __ae_ret);                                                       \
        ae_ctl_done(&__ae_ctl->gen);                                                                  \
    }                                                                                                 \
                                                                                                      \
    return;                                                                                           \
}

#define AE_MK_CALLBACK_VOIDFN(__bfun, __fname, __pos_str, __cb_ret_type, __cb_ret_param)              \
static void __bfun##_##__fname##_##__pos_str##_callback(void *__ae_ptr, __cb_ret_type __cb_ret_param) \
{                                                                                                     \
    AE_MK_CB_FN_START(__bfun);                                                                        \
                                                                                                      \
    __ae_ctl->return_params.__cb_ret_param = __cb_ret_param;                                          \
                                                                                                      \
    AE_MK_CB_FN_INVOKE_WORKER(__bfun, __fname, __pos_str);                                            \
                                                                                                      \
    if(state & AE_CTL_FN_COMPLETE)                                                                    \
    {                                                                                                 \
        void (* __ae_local_cb)(void *);                                                               \
        AE_MK_DONE_DECLS();                                                                           \
                                                                                                      \
        AE_MK_END_OF_BLOCKING(__bfun, "");                                                            \
        ae_debug_pbranch("ctl_done: cb: fn_complete: %p\n", __ae_ctl);                                \
        __ae_local_cb(__ae_local_up);                                                                 \
        ae_ctl_done(&__ae_ctl->gen);                                                                  \
    }                                                                                                 \
                                                                                                      \
    return;                                                                                           \
}

#define AE_MK_CALLBACK_FN_VOIDCB(__bfun, __fname, __pos_str, __fret_type)    \
static void __bfun##_##__fname##_##__pos_str##_callback(void *__ae_ptr)      \
{                                                                            \
    AE_MK_CB_FN_START(__bfun);                                               \
                                                                             \
    AE_MK_CB_FN_INVOKE_WORKER(__bfun, __fname, _pos_str);                    \
                                                                             \
    if(state & AE_CTL_FN_COMPLETE)                                           \
    {                                                                        \
        void (* __ae_local_cb)(void *, __fret_type);                         \
        AE_MK_DONE_DECLS();                                                  \
        __fret_type __ae_ret = __ae_ctl->return_value;                       \
                                                                             \
        AE_MK_END_OF_BLOCKING(__bfun, "");                                   \
        ae_debug_pbranch("ctl_done: cb: fn_complete: %p\n", __ae_ctl);       \
        __ae_local_cb(__ae_local_up, __ae_ret);                              \
        ae_ctl_done(&__ae_ctl->gen);                                         \
    }                                                                        \
                                                                             \
    return;                                                                  \
}

#define AE_MK_CALLBACK_VOIDFN_VOIDCB(__bfun, __fname, __pos_str)        \
static void __bfun##_##__fname##_##__pos_str##_callback(void *__ae_ptr) \
{                                                                       \
    AE_MK_CB_FN_START(__bfun);                                          \
                                                                        \
    AE_MK_CB_FN_INVOKE_WORKER(__bfun, __fname, __pos_str);              \
                                                                        \
    if(state & AE_CTL_FN_COMPLETE)                                      \
    {                                                                   \
        void (* __ae_local_cb)(void *);                                 \
        AE_MK_DONE_DECLS();                                             \
                                                                        \
        AE_MK_END_OF_BLOCKING(__bfun, "");                              \
        ae_debug_pbranch("ctl_done: cb: fn_complete: %p\n", __ae_ctl);  \
        __ae_local_cb(__ae_local_up);                                   \
        ae_ctl_done(&__ae_ctl->gen);                                    \
    }                                                                   \
                                                                        \
    return;                                                             \
}

#define AE_MK_POST_CALL(__fname, __bcall, __bcallName, __pos_str)            \
    __ae_postret = __bcall(__fname##_##__bcallName##_##__pos_str##_callback, \
                           __ae_ctl,                                         \
                           __ae_ctl->gen.hints,                              \
                           __ae_ctl->gen.context,                            \
                           &__ae_ctl->gen.current_op_id,                     \
                           1);

#define AE_MK_POST_RET_PARAM(__bcall, __pos_str)                          \
    /* only the RHS of the assignment expression is used in this macro */ \
    ret = &__ae_ctl->return_params.__bcall##_##__pos_str##_ret;

#define AE_MK_WORKER_PARAMS(__fname) \
    struct __fname##_ctl *__ae_ctl; \
    enum ae_ctl_state *__ae_state;

#define AE_MK_WORKER_RET_DECL() \
    ae_ret_t __ae_postret __unused__;

 #define AE_MK_WORKER_DECLS() \
    ae_ret_t __ae_postret __unused__; \
    ae_ret_t __ae_myret;

#define AE_MK_WORKER_START_STMTS()                      \
    __ae_myret = AE_SUCCESS;                            \
    if(*__ae_state == AE_CTL_CALL_COMPLETE)             \
    {                                                   \
        assert(__ae_ctl->gen.state_label);              \
        goto *__ae_ctl->gen.state_label;                \
    }

#define AE_MK_WORKER_END_STMTS() \
__ae_blocking_function_done:     \
{                                \
    return __ae_myret;           \
}

#define AE_MK_WORKER_RETURN(__ret_expr)       \
{                                             \
    __ae_ctl->return_value = __ret_expr;      \
    *__ae_state |= AE_CTL_FN_COMPLETE;        \
    goto __ae_blocking_function_done;         \
}

#define AE_MK_WORKER_VOID_RETURN()            \
{                                             \
    *__ae_state |= AE_CTL_FN_COMPLETE;        \
    goto __ae_blocking_function_done;         \
}

#define AE_MK_WORKER_BEFORE_POST(__fname, __bcall, __pos_str) \
    __fname##_##__bcall##_##__pos_str##_before_label:         \
    __ae_ctl->gen.state_label =                               \
        &&__fname##_##__bcall##_##__pos_str##_after_label;

#define AE_MK_WORKER_AFTER_POST(__fname, __bcall, __pos_str) \
    if(__ae_postret == AE_SUCCESS)                           \
    {                                                        \
       __ae_myret = AE_SUCCESS;                              \
        *__ae_state |= AE_CTL_POST_SUCCESS;                  \
        goto __ae_blocking_function_done;                    \
    }                                                        \
    else if(__ae_postret != AE_IMMEDIATE_COMPLETION)         \
    {                                                        \
        __ae_myret = __ae_postret;                           \
        *__ae_state = AE_CTL_POST_FAILED;                    \
        goto __ae_blocking_function_done;                    \
    }                                                        \
    __fname##_##__bcall##_##__pos_str##_after_label: {}

#define AE_MK_WORKER_BEFORE_POST_IN_PBRANCH(__fname, __bcall, __pos_str, __pbranch_pos_str) \
    __fname##_##__bcall##_##__pos_str##_before_label:                                       \
    __ae_ctl->gen.state_label =                                                             \
        &&__fname##_##__bcall##_##__pos_str##_after_label;                                  \
    ae_debug_pbranch("ctl_addref: before post: %p\n", __ae_ctl);                            \
    ae_ctl_addref(&__ae_ctl->gen);

#define AE_MK_WORKER_AFTER_POST_IN_PBRANCH(__fname, __bcall, __pos_str, __pbranch_pos_str) \
   if(__ae_postret == AE_SUCCESS)                                                          \
   {                                                                                       \
       *__ae_state |= AE_CTL_POST_SUCCESS;                                                 \
       __ae_myret = AE_SUCCESS;                                                            \
       goto __ae_pbranch_##__pbranch_pos_str##_inprogress;                                 \
   }                                                                                       \
   else if(__ae_postret == AE_IMMEDIATE_COMPLETION)                                        \
   {                                                                                       \
       ae_debug_pbranch("ctl_done: post ic: %p\n", __ae_ctl);                              \
       ae_ctl_done(&__ae_ctl->gen);                                                        \
   }                                                                                       \
   else                                                                                    \
   {                                                                                       \
       __ae_myret = __ae_postret;                                                          \
       *__ae_state = AE_CTL_POST_FAILED;                                                   \
       goto __ae_blocking_function_done;                                                   \
   }                                                                                       \
   __fname##_##__bcall##_##__pos_str##_after_label:                                        \
    if(*__ae_state == AE_CTL_CALL_COMPLETE)                                                \
    {                                                                                      \
        __ae_pbranch_##__pbranch_pos_str##_state = AE_PBRANCH_INPROGRESS;                  \
    }

#define AE_MK_WORKER_RETURN_VAR_EXPR(__bfun, __bcall, __pos_str) \
    __ae_ctl->return_params.__bcall##_##__pos_str##_ret;

#define AE_MK_WORKER_PBRANCH_DECLS(__fname, __pbranch_pos_str) \
    enum ae_pbranch_state __ae_pbranch_##__pbranch_pos_str##_state;

#define AE_MK_WORKER_PBRANCH_START_STMTS(__fname, __pwait_pos_str, __pbranch_pos_str) \
__ae_pbranch_##__pbranch_pos_str##_start:                                             \
{                                                                                     \
    struct __fname##_ctl *__ae_ctl_parent;                                            \
    __ae_pbranch_##__pbranch_pos_str##_state = AE_PBRANCH_INIT;                       \
    __ae_ctl_parent = __ae_ctl;                                                       \
    __ae_ctl = malloc(sizeof(*__ae_ctl));                                             \
    if(__ae_ctl == NULL)                                                              \
    {                                                                                 \
        return AE_NOMEM;                                                              \
    }                                                                                 \
    ae_ctl_init(&__ae_ctl->gen,                                                       \
                __ae_ctl,                                                             \
                #__fname "_ctl:pbranch_" #__pbranch_pos_str,                          \
                NULL,                                                                 \
                __ae_ctl_parent->gen.context,                                         \
                1,                                                                    \
                __ae_ctl_parent);                                                     \
    ae_debug_pbranch("starting pbranch: %s (ctl=%p, parent=%p)\n",                    \
                     #__fname "_ctl:pbranch_" #__pbranch_pos_str,                     \
                     __ae_ctl,                                                        \
                     __ae_ctl_parent);                                                \
    __ae_ctl->parent = __ae_ctl_parent;                                               \
    __ae_ctl->params = __ae_ctl_parent->params;                                       \
    memcpy(&__ae_ctl->pwait_##__pwait_pos_str##_params.private,                       \
           &__ae_ctl_parent->pwait_##__pwait_pos_str##_params.private,                \
           sizeof(__ae_ctl->pwait_##__pwait_pos_str##_params.private));               \
    __ae_ctl->pwait_##__pwait_pos_str##_params.shared_params =                        \
        &__ae_ctl_parent->pwait_##__pwait_pos_str##_params.shared;                    \
    ae_ctl_pbranch_start(&__ae_ctl->gen);                                             \
}

/**
 * Removes itself from the parent list of in-progress pbranches
 * Sets __ae_ctl back to parent and jumps to pwait end
 */
#define AE_MK_WORKER_PBRANCH_END_STMTS(__fname, __pwait_pos_str, __pbranch_pos_str)     \
__ae_pbranch_##__pbranch_pos_str##_done:                                                \
{                                                                                       \
    struct __fname##_ctl *__pbranch_ctl = __ae_ctl;                                     \
    __ae_ctl = __ae_ctl->parent;                                                        \
    if(__ae_pbranch_##__pbranch_pos_str##_state == AE_PBRANCH_INIT)                     \
    {                                                                                   \
        ae_debug_pbranch("pbranch completed inline: %s (ctl=%p, parent=%p)\n",          \
                          #__fname "_ctl:pbranch_" #__pbranch_pos_str,                  \
                         __pbranch_ctl,                                                 \
                         __ae_ctl);                                                     \
        ae_ctl_pbranch_done(&__pbranch_ctl->gen, __ae_state);                           \
        ae_debug_pbranch("ctl_done: pbranch done (init): %p\n", __ae_ctl);              \
        ae_ctl_done(&__pbranch_ctl->gen);                                               \
        goto __ae_pbranch_##__pbranch_pos_str##_after;                                  \
    }                                                                                   \
    else                                                                                \
    {                                                                                   \
        ae_debug_pbranch("pbranch completed: %s (ctl=%p, parent=%p)\n",                 \
                          #__fname "_ctl:pbranch_" #__pbranch_pos_str,                  \
                         __pbranch_ctl,                                                 \
                         __ae_ctl);                                                     \
        __ae_pwait_##__pwait_pos_str##_command =                                        \
            ae_ctl_pbranch_done(&__pbranch_ctl->gen, __ae_state);                       \
        goto __ae_pwait_##__pwait_pos_str##_end;                                        \
    }                                                                                   \
}                                                                                       \
__ae_pbranch_##__pbranch_pos_str##_inprogress:                                          \
{                                                                                       \
    struct __fname##_ctl *__pbranch_ctl = __ae_ctl;                                     \
    __ae_ctl = __ae_ctl->parent;                                                        \
    if(__ae_pbranch_##__pbranch_pos_str##_state == AE_PBRANCH_INIT)                     \
    {                                                                                   \
        ae_debug_pbranch("pbranch init -> inprogress: %s (ctl=%p, parent=%p)\n",        \
                          #__fname "_ctl:pbranch_" #__pbranch_pos_str,                  \
                         __pbranch_ctl,                                                 \
                         __ae_ctl);                                                     \
        ae_debug_pbranch("ctl_done: pbranch inprogress (init): %p\n", __ae_ctl);        \
        ae_ctl_done(&__pbranch_ctl->gen);                                               \
        goto __ae_pbranch_##__pbranch_pos_str##_after;                                  \
    }                                                                                   \
    else                                                                                \
    {                                                                                   \
        ae_debug_pbranch("pbranch inprogress -> inprogress: %s (ctl=%p, parent=%p)\n",  \
                          #__fname "_ctl:pbranch_" #__pbranch_pos_str,                  \
                         __pbranch_ctl,                                                 \
                         __ae_ctl);                                                     \
        ae_debug_pbranch("ctl_done: pbranch inprogress: %p\n", __ae_ctl);               \
        ae_ctl_done(&__pbranch_ctl->gen);                                               \
        __ae_pwait_##__pwait_pos_str##_command = AE_PWAIT_YIELD;                        \
        goto __ae_pwait_##__pwait_pos_str##_end;                                        \
    }                                                                                   \
}                                                                                       \
__ae_pbranch_##__pbranch_pos_str##_after: {}

#define AE_MK_WORKER_PWAIT_DECLS(__fname, __pwait_pos_str) \
    enum ae_pwait_command __ae_pwait_##__pwait_pos_str##_command;

#define AE_MK_WORKER_PWAIT_START_STMTS(__fname, __pwait_pos_str)                  \
__ae_pwait_##__pwait_pos_str##_start:                                             \
    __ae_pwait_##__pwait_pos_str##_command = AE_PWAIT_NONE;                       \
    __ae_ctl->pwait_##__pwait_pos_str##_params.shared_params =                    \
        &__ae_ctl->pwait_##__pwait_pos_str##_params.shared;                       \
    ae_ctl_pwait_start(&__ae_ctl->gen);

#define AE_MK_WORKER_PWAIT_END_STMTS(__fname, __pwait_pos_str)                         \
__ae_pwait_##__pwait_pos_str##_init_done:                                              \
{                                                                                      \
    ae_debug_pbranch("pwait completed: %s (ctl=%p)\n",                                 \
                     #__fname "_ctl:pwait_" #__pwait_pos_str,                          \
                     __ae_ctl);                                                        \
    __ae_pwait_##__pwait_pos_str##_command =                                           \
        ae_ctl_pwait_init_done(&__ae_ctl->gen);                                        \
}                                                                                      \
__ae_pwait_##__pwait_pos_str##_end:                                                    \
{                                                                                      \
    assert(__ae_pwait_##__pwait_pos_str##_command != AE_PWAIT_NONE);                   \
    ae_debug_pbranch("pwait end: %s (command=%s, ctl=%p)\n",                           \
                     #__fname "_ctl:pwait_" #__pwait_pos_str,                          \
                     ae_pwait_command_string(__ae_pwait_##__pwait_pos_str##_command),  \
                     __ae_ctl);                                                        \
    if(__ae_pwait_##__pwait_pos_str##_command == AE_PWAIT_YIELD)                       \
    {                                                                                  \
        goto __ae_blocking_function_done;                                              \
    }                                                                                  \
}                                                                                      \
__ae_pwait_##__pwait_pos_str##_after: {}

#define AE_MK_WORKER_PBREAK(__pbranch_pos_str, __pbreak_pos_str) \
__ae_pbreak_##__pbreak_pos_str: {                                \
    goto __ae_pbranch_##__pbranch_pos_str##_done;                \
}

#define AE_MK_WORKER_LONE_PBRANCH_DECLS(__fname, __pbranch_pos_str) \
    enum ae_pbranch_state __ae_pbranch_##__pbranch_pos_str##_state;

#define AE_MK_WORKER_LONE_PBRANCH_START_STMTS(__fname, __pbranch_pos_str) \
__ae_pbranch_##__pbranch_pos_str##_start:                                 \
{                                                                         \
    struct __fname##_ctl *__ae_ctl_parent;                                \
    __ae_pbranch_##__pbranch_pos_str##_state = AE_PBRANCH_INIT;           \
    __ae_ctl_parent = __ae_ctl;                                           \
    __ae_ctl = malloc(sizeof(*__ae_ctl));                                 \
    if(__ae_ctl == NULL)                                                  \
    {                                                                     \
        return AE_NOMEM;                                                  \
    }                                                                     \
    ae_ctl_init(&__ae_ctl->gen,                                           \
                __ae_ctl,                                                 \
                #__fname "_ctl:pbranch_" #__pbranch_pos_str,              \
                NULL,                                                     \
                __ae_ctl_parent->gen.context,                             \
                1,                                                        \
                __ae_ctl_parent);                                         \
    ae_debug_pbranch("starting lone pbranch: %s (ctl=%p, parent=%p)\n",   \
                     #__fname "_ctl:pbranch_" #__pbranch_pos_str,         \
                     __ae_ctl,                                            \
                     __ae_ctl_parent);                                    \
    __ae_ctl->parent = __ae_ctl_parent;                                   \
    __ae_ctl->params = __ae_ctl_parent->params;                           \
    ae_ctl_lone_pbranch_start(&__ae_ctl->gen);                            \
}

#define AE_MK_WORKER_LONE_PBRANCH_END_STMTS(__fname, __pbranch_pos_str)               \
__ae_pbranch_##__pbranch_pos_str##_done:                                              \
{                                                                                     \
    struct __fname##_ctl *__pbranch_ctl = __ae_ctl;                                   \
    __ae_ctl = __ae_ctl->parent;                                                      \
    ae_ctl_lone_pbranch_done(&__pbranch_ctl->gen, __ae_state);                        \
    if(__ae_pbranch_##__pbranch_pos_str##_state == AE_PBRANCH_INIT)                   \
    {                                                                                 \
        ae_debug_pbranch("lone pbranch completed inline: %s (ctl=%p, parent=%p)\n",   \
                          #__fname "_ctl:pbranch_" #__pbranch_pos_str,                \
                         __pbranch_ctl,                                               \
                         __ae_ctl);                                                   \
        ae_ctl_done(&__pbranch_ctl->gen);                                             \
        goto __ae_pbranch_##__pbranch_pos_str##_after;                                \
    }                                                                                 \
    else                                                                              \
    {                                                                                 \
        ae_debug_pbranch("lone pbranch completed: %s (ctl=%p, parent=%p)\n",          \
                          #__fname "_ctl:pbranch_" #__pbranch_pos_str,                \
                         __pbranch_ctl,                                               \
                         __ae_ctl);                                                   \
        ae_ctl_done(&__ae_ctl->gen);                                             \
        goto __ae_blocking_function_done;                                             \
    }                                                                                 \
}                                                                                     \
__ae_pbranch_##__pbranch_pos_str##_inprogress:                                        \
{                                                                                     \
    struct __fname##_ctl *__pbranch_ctl = __ae_ctl;                                   \
    __ae_ctl = __ae_ctl->parent;                                                      \
    if(__ae_pbranch_##__pbranch_pos_str##_state == AE_PBRANCH_INIT)                   \
    {                                                                                 \
        ae_debug_pbranch("lone pbranch init -> inprogress: %s (ctl=%p, parent=%p)\n", \
                          #__fname "_ctl:pbranch_" #__pbranch_pos_str,                \
                         __pbranch_ctl,                                               \
                         __ae_ctl);                                                   \
        ae_ctl_done(&__pbranch_ctl->gen);                                             \
        goto __ae_pbranch_##__pbranch_pos_str##_after;                                \
    }                                                                                 \
    else                                                                              \
    {                                                                                 \
        ae_debug_pbranch(                                                             \
            "lone pbranch inprogress -> inprogress: %s (ctl=%p, parent=%p)\n",        \
            #__fname "_ctl:pbranch_" #__pbranch_pos_str,                              \
            __pbranch_ctl,                                                            \
            __ae_ctl);                                                                \
        ae_ctl_done(&__pbranch_ctl->gen);                                             \
        goto __ae_blocking_function_done;                                             \
    }                                                                                 \
}                                                                                     \
__ae_pbranch_##__pbranch_pos_str##_after: {}

#define AE_MK_BFUN_RET_DECL() \
    ae_ret_t __ae_myret;

#define AE_MK_BFUN_PARAMS_FOR_STRUCT_DECLS() \
    struct ae_ctl gen; \
    void *user_ptr;

#define AE_MK_BFUN_PARAMS_DECLS() \
    void *__ae_user_ptr; \
    ae_hints_t *__ae_hints; \
    ae_context_t __ae_context; \
    ae_op_id_t *__ae_op_id; \
    int __ae_internal;
    /* void (*callback) (void *user_ptr [, __ret_type __ae_ret]); */

#define AE_MK_BFUN_PARAMS_FUN_PTR_DECLS(__ret_type) \
    void *__ae_user_ptr; \
    ae_hints_t *__ae_hints; \
    ae_context_t __ae_context; \
    ae_op_id_t *__ae_op_id; \
    int __ae_internal; \
    __ret_type *retval;
    /* void (*callback) (void *user_ptr [, __ret_type __ae_ret]); */

#define AE_MK_BFUN_DECLS(__fname)                       \
    struct __fname##_ctl *__ae_ctl;                     \
    ae_ret_t __ae_myret;                                \
    enum ae_ctl_state state;

#define AE_MK_BFUN_INIT_BLOCK(__fname)                                \
    __ae_ctl = malloc(sizeof(*__ae_ctl));                             \
    if(__ae_ctl == NULL)                                              \
    {                                                                 \
        return AE_NOMEM;                                              \
    }                                                                 \
    ae_ctl_init(&__ae_ctl->gen,                                       \
                __ae_ctl,                                             \
                #__fname,                                             \
                __ae_hints,                                           \
                __ae_context,                                         \
                __ae_internal,                                        \
                __ae_user_ptr);                                       \
    AE_MK_START_OF_BLOCKING(__fname);                                 \
    __ae_ctl->params = &__ae_ctl->fields;                             \
    __ae_ctl->user_ptr = __ae_user_ptr;                               \
    __ae_ctl->__ae_callback = __ae_callback;                          \
    if(__ae_op_id) *__ae_op_id = ae_id_gen(0, (intptr_t)__ae_ctl);

#define AE_MK_BFUN_INVOKE_WORKER(__fname, __ret_type)                 \
                                                                      \
    state = AE_CTL_START;                                             \
                                                                      \
    __ae_myret = __ae_worker_##__fname(__ae_ctl, &state);             \
    if(__ae_myret != AE_SUCCESS)                                      \
    {                                                                 \
        aesop_err(                                                    \
            "INVALID STATE: "                                         \
            "blocking function did not return "                       \
            "AE_SUCCESS"                                              \
            "in function: %s, trace exiting\n",                       \
            #__fname);                                                \
        return __ae_myret;                                            \
    }                                                                 \
                                                                      \
    if(!(state & AE_CTL_FN_COMPLETE))                                 \
    {                                                                 \
        return __ae_myret;                                            \
    }                                                                 \
                                                                      \
    __fname##_immediate_completion:                                   \
    {                                                                 \
        void (* __ae_local_cb)(void *, __ret_type);                   \
        AE_MK_DONE_DECLS();                                           \
        AE_MK_END_OF_BLOCKING(__fname, " (IC)");                      \
        if(__ae_retvalue)                                             \
        {                                                             \
            *__ae_retvalue = __ae_ctl->return_value;                  \
            ae_debug_pbranch("ctl_done: bfun ic: %p\n", __ae_ctl);    \
            ae_ctl_done(&__ae_ctl->gen);                              \
            return AE_IMMEDIATE_COMPLETION;                           \
        }                                                             \
        else                                                          \
        {                                                             \
            __ret_type __ae_local_retval = __ae_ctl->return_value;    \
            ae_debug_pbranch("ctl_done: bfun ic: %p\n", __ae_ctl);    \
            __ae_local_cb(__ae_local_up, __ae_local_retval);          \
            ae_ctl_done(&__ae_ctl->gen);                              \
        }                                                             \
    }                                                                 \
    return __ae_myret;

#define AE_MK_BFUN_INVOKE_WORKER_VOIDFN(__fname)                      \
                                                                      \
    state = AE_CTL_START;                                             \
                                                                      \
    __ae_myret = __ae_worker_##__fname(__ae_ctl, &state);             \
    if(__ae_myret != AE_SUCCESS)                                      \
    {                                                                 \
        aesop_err(                                                    \
            "INVALID STATE: "                                         \
            "blocking function did not return "                       \
            "AE_SUCCESS (posted) "                                    \
            "in function: %s, trace exiting\n",                       \
            #__fname);                                                \
        return __ae_myret;                                            \
    }                                                                 \
                                                                      \
    if(!(state & AE_CTL_FN_COMPLETE))                                 \
    {                                                                 \
        return __ae_myret;                                            \
    }                                                                 \
                                                                      \
    __fname##_immediate_completion:                                   \
    {                                                                 \
        void (* __ae_local_cb)(void *);                               \
        AE_MK_DONE_DECLS();                                           \
        AE_MK_END_OF_BLOCKING(__fname, " (IC)");                      \
        ae_debug_pbranch("ctl_done: bfun ic: %p\n", __ae_ctl);        \
        ae_ctl_done(&__ae_ctl->gen);                                  \
        return AE_IMMEDIATE_COMPLETION;                               \
    }                                                                 \
                                                                      \
    return __ae_myret;


#endif /* __AE_BLOCKING_PARSER_H__ */
