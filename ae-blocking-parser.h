#ifndef __AE_BLOCKING_H__
#define __AE_BLOCKING_H__

/**
 * Defines macros that get used by the parser to generate C code for blocking and remote parsers.
 * Each macro translates a set of parameters into either a C statement or a C declaration.
 * C statements can "one line" statements of the form:
 *
 * baz = foo(bar);
 *
 * or can be compound statements:
 *
 * {
 *     int foo1, foo2;
 *     baz1 = bar(foo1);
 *     baz2 = bar(foo2);
 * }
 *
 * or can be more complex statements:
 *
 * if(foo == 1) { baz = bar(foo); return baz; }
 *
 * Note that in the one line case, the statement _must_ end with a semi-colon, whereas in the compound statement
 * and if conditional, the curly braces end the statement.
 *
 * Declarations are of the form:
 *
 * int bar;
 * struct foo foo1, foo2;
 * char *baz = "foobar";
 *
 * Note that declarations must always end with a semi-colon.
 */

#include "src/aesop/aesop.h"
#include "src/common/triton-error.h"
#include "src/common/triton-log.h"
#include "src/common/triton-debug.h"

#define AE_MK_START_OF_BLOCKING(__fname__) \
    triton_debug(ae_debug_blocking_funs, "[START]: %s (%p)\n", #__fname__, ctl);

#define AE_MK_END_OF_BLOCKING(__fname__) \
    triton_debug(ae_debug_blocking_funs, "[END]: %s (%p)\n", #__fname__, ctl);

#define AE_MK_RET_DECL() \
    triton_ret_t __ae_postret;
    
#define AE_MK_POSTCB_STMT(__fname, __ret_type, __ctl_name, __location) \
    if(__ae_postret != TRITON_SUCCESS) \
    { \
        triton_err(triton_log_default, "INVALID STATE: %s:%d: post call did not return AE_POSTED or AE_COMPLETE, trace exiting\n", #__fname, __location); \
        assert(__ae_postret == TRITON_SUCCESS); \
        return; \
    }

#define AE_MK_POST_STMT(__fname, __ret_type, __ctl_name, __location) \
    if(__ae_postret != TRITON_SUCCESS) \
    { \
        triton_err(triton_log_default, "INVALID STATE: %s:%d: post call did not return AE_POSTED or AE_COMPLETE, trace exiting\n", #__fname, __location); \
        assert(__ae_postret == TRITON_SUCCESS); \
        return __ae_postret; \
    }

#define AE_MK_PBREAK_DECLS() \
    triton_ret_t __ae_cancel_ret;

#define AE_MK_PBREAK_STMTS(__prefix, __fname, __location) \
{ \
    done_ctl->parent->hit_pbreak = 1; \
    triton_list_del(&done_ctl->link); \
    __ae_cancel_ret = ae_cancel_children(done_ctl->context, done_ctl->parent); \
    if(__ae_cancel_ret != TRITON_SUCCESS) \
    { \
        triton_err(triton_log_default, "INVALID STATE: %s:%d: ae_cancel_cancel did not return success\n", #__fname, __location); \
        assert(__ae_cancel_ret == TRITON_SUCCESS); \
    } \
}

#define AE_MK_PBRANCH_CB_START_DECLS(__prefix, __ctl_name) \
    int __ae_pwait_done; \
    struct __ctl_name *done_ctl;

#define AE_MK_PBRANCH_CB_START_STMTS(__prefix) \
{ \
    done_ctl = __prefix; \
    triton_mutex_lock(&done_ctl->parent->mutex); \
}

#define AE_MK_PBRANCH_DELETE_STMTS() \
    triton_list_del(&done_ctl->link);

#define AE_MK_PBRANCH_DONE_STMTS() \
{ \
    done_ctl->parent->completed++; \
    __ae_pwait_done = done_ctl->parent->allposted == 1 && done_ctl->parent->posted == done_ctl->parent->completed; \
    triton_mutex_unlock(&done_ctl->parent->mutex); \
    free(done_ctl); \
}

#define AE_MK_PBRANCH_CB_DONE_STMTS(__id) \
    if(!__ae_pwait_done) goto __ae_callback_end;

#define AE_MK_PBRANCH_POST_DONE_STMTS(__pbranch_id) \
    if(!__ae_pwait_done) goto __ae_##__pbranch_id##_end;

#define AE_MK_CB_DONE_STMTS() \
    goto __ae_callback_end;

#define AE_MK_CB_DONE_CTL_SET_STMTS(__prefix) \
    __prefix = done_ctl->parent;

#define AE_MK_PWAIT_INIT_STMTS(__pwait_params, __ctl) \
{ \
    __ctl->posted = 0; \
    __ctl->completed = 0; \
    __ctl->allposted = 0; \
    __ctl->hit_pbreak = 0; \
    triton_mutex_init(&__ctl->mutex, NULL); \
    __ctl->parent = NULL; \
    triton_list_init(&__ctl->children); \
    __ctl->in_pwait = 1; \
    __ctl->__pwait_params.shared_params = &__ctl->__pwait_params.shared; \
}

#define AE_MK_PWAIT_FINISH_STMTS(__ctl, __pwait_id) \
{ \
    int __ae_pwait_done; \
    triton_mutex_lock(&__ctl->mutex); \
    __ae_pwait_done = __ctl->posted == __ctl->completed; \
    __ctl->allposted = 1; \
    triton_mutex_unlock(&__ctl->mutex); \
    if(!__ae_pwait_done) goto __ae_pwait_##__pwait_id##_not_done; \
}

#define AE_MK_PWAIT_NOT_DONE_STMTS(__ctl, __pwait_id) \
    __ae_pwait_##__pwait_id##_not_done: {}

#define AE_MK_POST_FUN_INIT_STMTS(__ctl) \
{ \
    __ctl = malloc(sizeof(*__ctl)); \
    if(__ctl == NULL) \
    { \
        return TRITON_ERR_NOMEM; \
    } \
    __ctl->params = &__ctl->fields; \
    __ctl->user_ptr = user_ptr; \
    __ctl->callback = callback; \
    __ctl->hints = hints; \
    __ctl->context = context; \
    if(op_id) *op_id = ae_id_gen(0, (uint64_t)ctl); \
    triton_mutex_init(&__ctl->mutex, NULL); \
}

#define AE_MK_PBRANCH_POST_DECLS(__ctl_type, __ctl) \
    struct __ctl_type * __ctl;

#define AE_MK_PBRANCH_POST_STMTS(__pwait_ctl, __ctl, __parent_ctl, __ctl_type, __fname, __location, __pbranch_id) \
{ \
    __ctl = malloc(sizeof(*__ctl)); \
    if(__ctl == NULL) \
    { \
        triton_err(triton_log_default, "INVALID STATE: %s:%d: memory allocation for control structure failed!\n", #__fname, __location); \
        assert(__ctl != NULL); \
        goto __ae_##__pbranch_id##_end; \
    } \
    __ctl->parent = __parent_ctl; \
    __ctl->context = __parent_ctl->context; \
    __ctl->hints = __parent_ctl->hints; \
    __ctl->params = __parent_ctl->params; \
    memcpy(&__ctl->__pwait_ctl.private, &__parent_ctl->__pwait_ctl.private, sizeof(__ctl->__pwait_ctl.private)); \
    __ctl->__pwait_ctl.shared_params = &__parent_ctl->__pwait_ctl.shared; \
    triton_mutex_lock(&__ctl->parent->mutex); \
    __ctl->parent->posted++; \
    triton_list_link_clear(&__ctl->link); \
    triton_queue_enqueue(&__ctl->link, &__ctl->parent->children); \
    triton_mutex_unlock(&__ctl->parent->mutex); \
}

#define AE_MK_PARENT_POINTER_DECL(__parent) \
    struct __parent##_ctl *parent;

#define AE_MK_BLOCKING_PARAMS_FOR_STRUCT_DECLS() \
    ae_op_id_t current_op_id; \
    int cancelled; \
    triton_mutex_t mutex; \
    triton_list_t children; \
    struct triton_list_link link; \
    int posted; \
    int completed; \
    int allposted; \
    int hit_pbreak; \
    int in_pwait; \
    void *user_ptr; \
    ae_hints_t hints; \
    ae_context_t context;
    /* void (*callback) (void *user_ptr [, __ret_type __ae_ret]); */
    /* __ret_type __ae_ret */

#define AE_MK_BLOCKING_PARAMS_FOR_POST_DECLS() \
    void *user_ptr; \
    ae_hints_t hints; \
    ae_context_t context; \
    ae_op_id_t *op_id;
    /* void (*callback) (void *user_ptr [, __ret_type __ae_ret]); */

#define AE_MK_BLOCKING_PARAMS_FUN_PTR_DECLS() \
    void *user_ptr; \
    ae_hints_t hints; \
    ae_context_t context; \
    ae_op_id_t *op_id;
    /* void (*callback) (void *user_ptr [, __ret_type __ae_ret]); */


#define AE_MK_POST_FUN_DECLS(__ctl_type) \
    struct __ctl_type##_ctl *ctl; \
    triton_ret_t __ae_postret;

#define AE_MK_POST_FUN_FINISHED_STMTS() \
__ae_post_end: \
{ \
                   return __ae_postret; \
} \

#endif
