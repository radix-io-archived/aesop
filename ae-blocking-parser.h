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
    triton_ret_t __ae_postret __attribute__ ((unused));
    
#define AE_MK_POSTCB_STMT(__fname, __ret_type, __ctl_name, __location) \
    if(__ae_postret != TRITON_SUCCESS) \
    { \
        triton_err(triton_log_default, "INVALID STATE: %s:%d: post call did not return AE_POSTED or AE_COMPLETE, trace exiting\n", #__fname, __location); \
        triton_error_assert(__ae_postret); \
        return; \
    }

#define AE_MK_POST_STMT(__fname, __ret_type, __ctl_name, __location) \
    if(__ae_postret != TRITON_SUCCESS) \
    { \
        triton_err(triton_log_default, "INVALID STATE: %s:%d: post call did not return AE_POSTED or AE_COMPLETE, trace exiting\n", #__fname, __location); \
        triton_error_assert(__ae_postret); \
        return __ae_postret; \
    }

#define AE_MK_PBREAK_DECLS() \
    triton_ret_t __ae_cancel_ret;

#define AE_MK_PBREAK_STMTS(__prefix, __fname, __location) \
{ \
    done_ctl->parent->gen.hit_pbreak = 1; \
    triton_uint128_setzero(done_ctl->gen.current_op_id); \
    triton_list_del(&done_ctl->gen.link); \
}

#define AE_MK_PBRANCH_CB_START_DECLS(__prefix, __ctl_name) \
    int __ae_pwait_done; \
    struct __ctl_name *done_ctl;

#define AE_MK_PBRANCH_CB_START_STMTS(__prefix) \
{ \
    done_ctl = __prefix; \
    triton_mutex_lock(&done_ctl->parent->gen.mutex); \
}

#define AE_MK_PBRANCH_POST_START_DECLS(__prefix, __ctl_name) \
    int __ae_pwait_done; \
    struct __ctl_name *done_ctl;

#define AE_MK_PBRANCH_POST_START_STMTS(__prefix) \
{ \
    done_ctl = __prefix; \
    triton_mutex_lock(&done_ctl->parent->gen.mutex); \
}

#define AE_MK_PBRANCH_DELETE_STMTS() \
    triton_list_del(&done_ctl->gen.link);

#define AE_MK_PBRANCH_DONE_STMTS() \
{ \
    done_ctl->parent->gen.completed++; \
    __ae_pwait_done = done_ctl->parent->gen.allposted == 1 && done_ctl->parent->gen.posted == done_ctl->parent->gen.completed; \
    triton_mutex_unlock(&done_ctl->parent->gen.mutex); \
    ae_hints_destroy(done_ctl->gen.hints); \
    ae_ctl_destroy(done_ctl, &done_ctl->gen); \
}

#define AE_MK_PBRANCH_CB_DONE_STMTS(__id) \
    triton_mutex_lock(&ctl->gen.mutex); \
    __ae_pwait_done = ctl->gen.allposted == 1 && ctl->gen.posted == ctl->gen.completed; \
    triton_mutex_unlock(&ctl->gen.mutex); \
    if(!__ae_pwait_done) goto __ae_callback_end;

#define AE_MK_PBRANCH_POST_DONE_STMTS(__pbranch_id) \
    triton_mutex_lock(&ctl->gen.mutex); \
    __ae_pwait_done = ctl->gen.allposted == 1 && ctl->gen.posted == ctl->gen.completed; \
    triton_mutex_unlock(&ctl->gen.mutex); \
    if(!__ae_pwait_done) goto __ae_##__pbranch_id##_end;

#define AE_MK_CB_DECLS(__ctl, __ctl_type) \
    struct __ctl_type *__ctl;

#define AE_MK_CB_INIT_STMTS(__ctl, __ctl_type) \
    __ctl = (struct __ctl_type *) __ae_ptr; \
    triton_mutex_lock(&__ctl->gen.mutex); \
    triton_uint128_setzero(__ctl->gen.current_op_id); \
    triton_mutex_unlock(&__ctl->gen.mutex);

#define AE_MK_CB_DONE_STMTS() \
    goto __ae_callback_end;

#define AE_MK_CB_DONE_CTL_SET_STMTS(__prefix) \
    __prefix = done_ctl->parent;

#define AE_MK_PWAIT_INIT_STMTS(__pwait_params, __ctl, __pwait_name) \
{ \
    __ctl->parent = NULL; \
    __ctl->gen.in_pwait = 1; \
    __ctl->__pwait_params.shared_params = &__ctl->__pwait_params.shared; \
}

#define AE_MK_PWAIT_FINISH_STMTS(__ctl, __pwait_id) \
{ \
    int __ae_pwait_done; \
    triton_mutex_lock(&__ctl->gen.mutex); \
    __ae_pwait_done = __ctl->gen.posted == __ctl->gen.completed; \
    __ctl->gen.allposted = 1; \
    triton_mutex_unlock(&__ctl->gen.mutex); \
    if(!__ae_pwait_done) goto __ae_pwait_##__pwait_id##_not_done; \
}

#define AE_MK_PWAIT_NOT_DONE_STMTS(__ctl, __pwait_id) \
    __ae_pwait_##__pwait_id##_not_done: {}

#define AE_MK_POST_FUN_INIT_STMTS(__ctl, __fname) \
{ \
    __ctl = malloc(sizeof(*__ctl)); \
    if(__ctl == NULL) \
    { \
        return TRITON_ERR_NOMEM; \
    } \
    ae_ctl_init(&__ctl->gen, #__fname, __ae_hints, __ae_context, __ae_internal, __ae_user_ptr); \
    __ctl->params = &__ctl->fields; \
    __ctl->user_ptr = __ae_user_ptr; \
    __ctl->__ae_callback = __ae_callback; \
    if(__ae_op_id) *__ae_op_id = ae_id_gen(0, (intptr_t)ctl); \
}

#define AE_MK_PBRANCH_POST_DECLS(__ctl_type) \
    struct __ctl_type *parent_ctl;

#define AE_MK_PBRANCH_POST_STMTS(__pwait_ctl, __ctl_type, __fname, __location, __pbranch_id) \
{ \
    parent_ctl = ctl; \
    ctl = malloc(sizeof(*ctl)); \
    if(ctl == NULL) \
    { \
        triton_err(triton_log_default, "INVALID STATE: %s:%d: memory allocation for control structure failed!\n", #__fname, __location); \
        assert(ctl != NULL); \
        goto __ae_##__pbranch_id##_end; \
    } \
    ae_ctl_init(&ctl->gen, #__ctl_type ":" #__pbranch_id, NULL, parent_ctl->gen.context, 1, parent_ctl); \
    ctl->parent = parent_ctl; \
    ae_hints_dup(parent_ctl->gen.hints, &ctl->gen.hints); \
    ctl->params = parent_ctl->params; \
    memcpy(&ctl->__pwait_ctl.private, &parent_ctl->__pwait_ctl.private, sizeof(ctl->__pwait_ctl.private)); \
    ctl->__pwait_ctl.shared_params = &parent_ctl->__pwait_ctl.shared; \
    triton_mutex_lock(&ctl->parent->gen.mutex); \
    ctl->parent->gen.posted++; \
    triton_list_link_clear(&ctl->gen.link); \
    triton_queue_enqueue(&ctl->gen.link, &ctl->parent->gen.children); \
    triton_mutex_unlock(&ctl->parent->gen.mutex); \
}

#define AE_MK_PBRANCH_POST_END_STMTS(__pwait_ctl, __ctl_type, __fname, __location, __pbranch_id) \
{ \
    ctl = parent_ctl; \
}

#define AE_MK_LONE_PBRANCH_POST_DECLS(__ctl_type) \
    struct __ctl_type *parent_ctl;

#define AE_MK_LONE_PBRANCH_POST_STMTS(__ctl_type, __fname, __location, __pbranch_id) \
{ \
    parent_ctl = ctl; \
    ctl = malloc(sizeof(*ctl)); \
    if(ctl == NULL) \
    { \
        triton_err(triton_log_default, "INVALID STATE: %s:%d: memory allocation for control structure failed!\n", #__fname, __location); \
        assert(ctl != NULL); \
        goto __ae_##__pbranch_id##_end; \
    } \
    ae_ctl_init(&ctl->gen, #__ctl_type ":" #__pbranch_id, NULL, parent_ctl->gen.context, 1, parent_ctl); \
    ctl->parent = parent_ctl; \
    ae_hints_dup(parent_ctl->gen.hints, &ctl->gen.hints); \
    ctl->params = parent_ctl->params; \
    triton_list_link_clear(&ctl->gen.link); \
    ae_lone_pbranches_add(&ctl->gen); \
    triton_mutex_lock(&ctl->parent->gen.mutex); \
    ae_ctl_refinc(&ctl->parent->gen); \
    triton_mutex_unlock(&ctl->parent->gen.mutex); \
}

#define AE_MK_LONE_PBRANCH_POST_END_STMTS(__ctl_type, __fname, __location, __pbranch_id) \
{ \
    ctl = parent_ctl; \
}

#define AE_MK_LONE_PBRANCH_DONE_STMTS() \
{ \
    int prc; \
    ae_lone_pbranches_remove(&ctl->gen); \
    triton_mutex_lock(&ctl->parent->gen.mutex); \
    prc = ae_ctl_refdec(&ctl->parent->gen); \
    triton_mutex_unlock(&ctl->parent->gen.mutex); \
    if(prc == 0) ae_ctl_destroy(ctl->parent, &ctl->parent->gen); \
    ae_hints_destroy(ctl->gen.hints); \
    ae_ctl_destroy(ctl, &ctl->gen); \
}
#define AE_MK_PARENT_POINTER_DECL(__parent) \
    struct __parent##_ctl *parent;

#define AE_MK_BLOCKING_PARAMS_FOR_STRUCT_DECLS() \
    struct ae_ctl gen; \
    void *user_ptr;

#define AE_MK_BLOCKING_PARAMS_FOR_POST_DECLS() \
    void *__ae_user_ptr; \
    ae_hints_t *__ae_hints; \
    ae_context_t __ae_context; \
    ae_op_id_t *__ae_op_id; \
    int __ae_internal;
    /* void (*callback) (void *user_ptr [, __ret_type __ae_ret]); */

#define AE_MK_BLOCKING_PARAMS_FUN_PTR_DECLS() \
    void *__ae_user_ptr; \
    ae_hints_t *__ae_hints; \
    ae_context_t __ae_context; \
    ae_op_id_t *__ae_op_id; \
    int __ae_internal;
    /* void (*callback) (void *user_ptr [, __ret_type __ae_ret]); */


#define AE_MK_POST_FUN_DECLS(__ctl_type) \
    struct __ctl_type##_ctl *ctl; \
    triton_ret_t __ae_postret;

#define AE_MK_POST_FUN_FINISHED_STMTS() \
__ae_post_end: \
{ \
    return __ae_postret; \
} \

#define AE_MK_NULL_RETURN_CALLBACK() \
{ \
    void (* __ae_local_cb)(void *); \
    void *__ae_local_up; \
    int __ae_refcount; \
    __ae_local_cb = ctl->__ae_callback; \
    __ae_local_up = ctl->user_ptr; \
    triton_mutex_lock(&ctl->gen.mutex); \
    __ae_refcount = ae_ctl_refdec(&ctl->gen); \
    assert(__ae_refcount >= 0); \
    triton_mutex_unlock(&ctl->gen.mutex); \
    __ae_local_cb(__ae_local_up); \
    if(__ae_refcount == 0) { \
        ae_ctl_destroy(ctl, &ctl->gen); \
    } \
    goto __ae_callback_end; \
}

#define AE_MK_RETURN_CALLBACK(__ret_param_expr, __ret_type) \
{ \
    void (* __ae_local_cb)(void *, __ret_type); \
    void *__ae_local_up; \
    int __ae_refcount; \
    __ret_type __ae_ret; \
    __ae_local_cb = ctl->__ae_callback; \
    __ae_local_up = ctl->user_ptr; \
    __ae_ret = __ret_param_expr; \
    triton_mutex_lock(&ctl->gen.mutex); \
    __ae_refcount = ae_ctl_refdec(&ctl->gen); \
    assert(__ae_refcount >= 0); \
    triton_mutex_unlock(&ctl->gen.mutex); \
    __ae_local_cb(__ae_local_up, __ae_ret); \
    if(__ae_refcount == 0) { \
        ae_ctl_destroy(ctl, &ctl->gen); \
    } \
    goto __ae_callback_end; \
}

#define AE_MK_NULL_POST_RETURN_CALLBACK() \
{ \
    void (* __ae_local_cb)(void *); \
    void *__ae_local_up; \
    int __ae_refcount; \
    __ae_local_cb = ctl->__ae_callback; \
    __ae_local_up = ctl->user_ptr; \
    triton_mutex_lock(&ctl->gen.mutex); \
    __ae_refcount = ae_ctl_refdec(&ctl->gen); \
    triton_mutex_unlock(&ctl->gen.mutex); \
    assert(__ae_refcount >= 0); \
    __ae_local_cb(__ae_local_up); \
    if(__ae_refcount == 0) ae_ctl_destroy(ctl, &ctl->gen); \
    __ae_postret = TRITON_SUCCESS; \
    goto __ae_post_end; \
}

#define AE_MK_POST_RETURN_CALLBACK(__ret_param_expr, __ret_type) \
{ \
    void (* __ae_local_cb)(void *, __ret_type); \
    void *__ae_local_up; \
    int __ae_refcount; \
    __ret_type __ae_ret; \
    __ae_local_cb = ctl->__ae_callback; \
    __ae_local_up = ctl->user_ptr; \
    __ae_ret = __ret_param_expr; \
    triton_mutex_lock(&ctl->gen.mutex); \
    __ae_refcount = ae_ctl_refdec(&ctl->gen); \
    triton_mutex_unlock(&ctl->gen.mutex); \
    assert(__ae_refcount >= 0); \
    __ae_local_cb(__ae_local_up, __ae_ret); \
    if(__ae_refcount == 0) ae_ctl_destroy(ctl, &ctl->gen); \
    __ae_postret = TRITON_SUCCESS; \
    goto __ae_post_end; \
}

#endif
