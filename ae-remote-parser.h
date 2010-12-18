#ifndef __AE_REMOTE_PARSER_H__
#define __AE_REMOTE_PARSER_H__

#include "src/common/triton-types.h"
#include "src/common/triton-list.h"
#include "src/common/triton-error.h"
#include "src/common/triton-log.h"
#include "src/remote/remote.hae"
#include "src/remote/encoding.h"
#include "src/common/triton-init.h"
#include "src/aesop/hints.h"
#include "src/common/triton-debug.h"

extern triton_debug_mask_t encoding_dbg_mask;

/**
 * The remote parser generates encoding/decoding functions for every type (struct, typedef, etc.) with
 * the __remote specifier.  This header allows callouts to be defined, so that compile time configuration
 * changes (made through CPP #defines) can be made to the internals of the encoding/decoding functions.
 * The header defines a set of macros that get invoked by the parser to generate declarations or statements that
 * are placed in the encoding/decoding functions.  To get an idea of how this works, the parser
 * generates an encoding function that looks like this:
 *
 * static inline triton_ret_t aer_encode_<type>(ae_buffer_t buf, const char *field_name, void *vx)
 * {
 *     AE_MK_ENCODE_DECLS(<type>);
 *     AE_MK_ENCODE_STMTS_START(<type>);
 *
 *     AE_MK_ENCODE_TYPE(<subtype1>, <subfield1>);
 *     AE_MK_ENCODE_TYPE(<subtype2>, <subfield2>);
 *     ...
 *
 *     AE_MK_ENCODE_STMTS_END(<type>);
 * }
 *
 * For fields that are pointers, the AE_MK_ENCODE_TYPE_PTR macro is called instead.
 *
 * The macros below are defined generate the appropriate code for the above function.
 */

#define AER_MK_ENCODE_TYPE(__type__, __field__, __ptr__) \
{ \
    ret = aer_encode_##__type__(buf, #__field__, __ptr__ x->__field__); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
}

#define AER_MK_ENCODE_DECLS(__type__) \
    triton_ret_t ret = TRITON_SUCCESS; \
    __type__ x;

#define AER_MK_ENCODE_STMTS_START(__type__, __canon_type__) \
{ \
    x = (__type__)vx; \
    triton_debug(encoding_dbg_mask, "encoding:" #__canon_type__ ":%s\t cptr=%p\n", varname, triton_buffer_cptr(buf)); \
}

#define AER_MK_ENCODE_STMTS_END() \
{ \
    return ret; \
}

/**
 * Simlarly, the decode function looks like this:
 *
 * triton_ret_t aer_decode_<type>(ae_buffer_t buf, const char *field_name, void *vx)
 * {
 *     AER_MK_DECODE_DECLS(<type>);
 *     AER_MK_DECODE_STMTS_START(<type>);
 *
 *     AER_MK_DECODE_TYPE(<subtype1>, <subfield1>);
 *     AER_MK_DECODE_TYPE(<subtype2>, <subfield2>);
 *     ...
 *
 *     AER_MK_DECODE_STMTS_END(<type>);
 * }
 *
 * As before, for fields that are pointers, the AER_MK_DECODE_TYPE_PTR macro is called instead.
 */

#define AER_MK_DECODE_TYPE(__type__, __field__, __ptr__) \
{ \
    ret = aer_decode_##__type__(buf, NULL, __ptr__ x->__field__); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
}

#define AER_MK_DECODE_DECLS(__type__) \
    triton_ret_t ret = TRITON_SUCCESS; \
    __type__ x;

#define AER_MK_DECODE_STMTS_START(__type__, __canon_type__) \
{ \
    x = (__type__)vx; \
    triton_debug(encoding_dbg_mask, "decoding:" #__canon_type__ ":%s\t cptr=%p\n", (varname ? *varname : ""), triton_buffer_cptr(buf)); \
}

#define AER_MK_DECODE_STMTS_END() \
{ \
    return ret; \
}

/**
 * Simlarly, the size encode_size function looks like this:
 *
 * uint64_t aer_encode_size_<type>(const char *field_name, void *vx)
 * {
 *     AER_MK_SIZE_DECLS(<type>);
 *     AER_MK_SIZE_STMTS_START(<type>);
 *
 *     AER_MK_SIZE_TYPE(<subtype1>, <subfield1>);
 *     AER_MK_SIZE_TYPE(<subtype2>, <subfield2>);
 *     ...
 *
 *     AER_MK_SIZE_STMTS_END(<type>);
 * }
 *
 * As before, for fields that are pointers, the AER_MK_SIZE_TYPE_PTR macro is called instead.
 */

#define AER_MK_SIZE_TYPE(__type__, __field__, __ptr__) \
{ \
    size += aer_encode_size_##__type__ (#__field__, __ptr__ x->__field__); \
}

#define AER_MK_SIZE_DECLS(__type__) \
    uint64_t size; \
    __type__ x;

#define AER_MK_SIZE_STMTS_START(__type__) \
{ \
    size = 0; \
    x = (__type__)vx; \
}

#define AER_MK_SIZE_STMTS_END() \
{ \
    return size; \
}

/**
 * Only called on pointer fields to pre-initialize
 */
#define AER_MK_INIT_PTR_NULL(__type__, __field__) \
{ \
    x->__field__ = NULL; \
}

#define AER_MK_INIT_NULL_TYPE(__basetype__, __type__, __field__, __ptr__) \
{ \
    ret = aer_init_null_##__type__(__ptr__ x->__field__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_destroy_##__basetype__(x); \
        return ret; \
    } \
}

#define AER_MK_PTR_INIT_TYPE(__basetype__, __type__, __field__, __ptr__) \
{ \
    x->__field__ = __field__; \
}

#define AER_MK_INIT_TYPE(__basetype__, __type__, __field__, __ptr__) \
{ \
    ret = aer_copy_##__type__(__ptr__ x->__field__, __field__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_destroy_##__basetype__(x); \
        return ret; \
    } \
}

#define AER_MK_INIT_DECLS(__type__) \
    triton_ret_t ret = TRITON_SUCCESS; \
    __type__ x;

#define AER_MK_INIT_STMTS_START(__type__) \
{ \
    x = (__type__)vx; \
}

#define AER_MK_INIT_STMTS_END() \
{ \
    return ret; \
}

#define AER_MK_PTR_COPY_TYPE(__basetype__, __type__, __field__, __ptr__) \
{ \
    x->__field__ = v->__field__; \
}

#define AER_MK_COPY_TYPE(__basetype__, __type__, __field__, __ptr__) \
{ \
    ret = aer_copy_##__type__(__ptr__ x->__field__, __ptr__ v->__field__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_destroy_##__basetype__(x); \
        return ret; \
    } \
}

#define AER_MK_COPY_DECLS(__type__) \
    triton_ret_t ret = TRITON_SUCCESS; \
    __type__ x; \
    __type__ v;

#define AER_MK_COPY_STMTS_START(__type__) \
{ \
    x = (__type__)vx; \
    v = (__type__)vv; \
}

#define AER_MK_COPY_STMTS_END() \
{ \
    return ret; \
}

#define AER_MK_DESTROY_TYPE(__basetype__, __type__, __field__, __ptr__) \
{ \
    aer_destroy_##__type__(__ptr__ x->__field__); \
}

#define AER_MK_DESTROY_DECLS(__type__) \
    __type__ x;

#define AER_MK_DESTROY_STMTS_START(__type__) \
{ \
    x = (__type__)vx; \
}

#define AER_MK_DESTROY_STMTS_END() 

#define AER_MK_STUB_DECL(__ret__, __fname__, params...) \
    __blocking __ret__ remote_##__fname__(aer_remote_ctx_t ctx, triton_addr_t id, ##params);

#define AER_MK_STUB_DECLS(__fname__) \
    aer_message_t send_message; \
    aer_message_t recv_message; \
    triton_ret_t ret, sret; \
    uint64_t insize; \
    uint64_t op; \
    int s;

#define AER_MK_STUB_BLOCK(__fname__, __intype__, __inname__, __outtype__, __outname__, __ptr__) \
{ \
    s = triton_addr_is_self(id); \
    if(s) \
    { \
        ret = ae_hints_put(&ctl->gen.hints, "triton.remote.context", sizeof(ctx), &ctx); \
        if(ret != TRITON_SUCCESS) triton_log_error(triton_log_default, ret, "Failed to store remote context in hints\n"); \
        ret = ae_hints_put(&ctl->gen.hints, "triton.remote.from", sizeof(id), &id); \
        if(ret != TRITON_SUCCESS) triton_log_error(triton_log_default, ret, "Failed to store remote address in hints\n"); \
        return __fname__(__inname__, __outname__); \
    } \
    insize = aer_encode_size_##__intype__(#__inname__, __ptr__ __inname__); \
    ret = aer_message_init(ctx, &send_message, AER_MESSAGE_REQUEST, insize); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    op = __get_op_id_##__fname__(); \
    ret = aer_message_attr_set(ctx, &send_message, "aesop.remote.operation", &op); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    triton_debug(encoding_dbg_mask, "---------- encoding request header: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&send_message.buffer)); \
    ret = aer_message_encode(ctx, &send_message); \
    triton_debug(encoding_dbg_mask, "---------- encoding request header completed: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&send_message.buffer)); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    triton_debug(encoding_dbg_mask, "---------- encoding request: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&send_message.buffer)); \
    ret = aer_encode_##__intype__(&(send_message.buffer), #__inname__, __ptr__ __inname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        triton_debug(encoding_dbg_mask, "---------- encoding request completed: " #__fname__ " cptr=%p ----------\n", \
                     triton_buffer_cptr(&send_message.buffer)); \
        aer_message_destroy(ctx, &send_message); \
        return ret; \
    } \
    ret = aer_encoding_finish(&(send_message.buffer)); \
    triton_debug(encoding_dbg_mask, "---------- encoding request completed: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&send_message.buffer)); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(ctx, &send_message); \
        return ret; \
    } \
    ret = aer_message_init(ctx, &recv_message, AER_MESSAGE_RESPONSE, AE_REMOTE_MAX_RESPONSE_SIZE); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(ctx, &send_message); \
        return ret; \
    } \
    ret = aer_message_sendrecv(ctx, id, &send_message, &recv_message); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(ctx, &send_message); \
        aer_message_destroy(ctx, &recv_message); \
        return ret; \
    } \
    triton_debug(encoding_dbg_mask, "---------- decoding response header:" #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&recv_message.buffer)); \
    ret = aer_message_decode(ctx, &recv_message); \
    triton_debug(encoding_dbg_mask, "---------- decoding response header completed:" #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&recv_message.buffer)); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(ctx, &send_message); \
        aer_message_destroy(ctx, &recv_message); \
        return ret; \
    } \
    triton_debug(encoding_dbg_mask, "---------- decoding response: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&recv_message.buffer)); \
    triton_debug(encoding_dbg_mask, "decoding:return cptr=%p\n", triton_buffer_cptr(&recv_message.buffer)); \
    ret = aer_decode_triton_ret_t(&(recv_message.buffer), NULL, &sret); \
    if(ret != TRITON_SUCCESS) \
    { \
        triton_debug(encoding_dbg_mask, "---------- decoding response completed: " #__fname__ " cptr=%p ----------\n", \
                     triton_buffer_cptr(&recv_message.buffer)); \
        aer_message_destroy(ctx, &send_message); \
        aer_message_destroy(ctx, &recv_message); \
        return ret; \
    } \
    if(sret == TRITON_SUCCESS) \
    { \
        triton_debug(encoding_dbg_mask, "decoding:" #__outtype__ ":" #__outname__ " cptr=%p\n", \
                     triton_buffer_cptr(&recv_message.buffer)); \
        ret = aer_decode_##__outtype__(&(recv_message.buffer), NULL, __outname__); \
        triton_debug(encoding_dbg_mask, "---------- decoding response completed: " #__fname__ " cptr=%p ----------\n", \
                     triton_buffer_cptr(&recv_message.buffer)); \
        aer_message_destroy(ctx, &send_message); \
        aer_message_destroy(ctx, &recv_message); \
        if(ret != TRITON_SUCCESS) \
        { \
            return ret; \
        } \
    } \
    else \
    { \
        triton_debug(encoding_dbg_mask, "---------- decoding response completed: cptr=%p ----------\n", \
                     triton_buffer_cptr(&recv_message.buffer)); \
        aer_message_destroy(ctx, &send_message); \
        aer_message_destroy(ctx, &recv_message); \
    } \
    return sret; \
}

#define AER_MK_SERVICE_DECL(__fname__) \
    __blocking triton_ret_t __service_##__fname__( \
        aer_remote_ctx_t ctx, aer_message_t *send_message, aer_message_t *recv_message);

#define AER_MK_SERVICE_FNDEF(__fname__, \
                             __intype__, \
                             __incanontype__, \
                             __inname__, \
                             __outtype__, \
                             __outcanontype__, \
                             __outname__, \
                             __ptr__) \
__blocking triton_ret_t __service_##__fname__( \
   aer_remote_ctx_t ctx, aer_message_t *in_message, aer_message_t *out_message) \
{ \
    triton_ret_t ret, sret; \
    uint32_t outsize, retsize; \
    __intype__ __inname__; \
    __outtype__ __outname__; \
    triton_debug(encoding_dbg_mask, "---------- decoding request: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&in_message->buffer)); \
    ret = aer_decode_##__incanontype__(&(in_message->buffer), NULL, &__inname__); \
    triton_debug(encoding_dbg_mask, "---------- decoding request completed: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&in_message->buffer)); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    sret = __fname__(__ptr__ __inname__, &__outname__); \
    retsize = aer_encode_size_triton_ret_t("error", &sret); \
    outsize = 0; \
    if(sret == TRITON_SUCCESS) \
    { \
        outsize = aer_encode_size_##__outcanontype__(#__outname__, &__outname__); \
    } \
    ret = aer_message_init(ctx, out_message, AER_MESSAGE_RESPONSE, retsize+outsize); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    triton_debug(encoding_dbg_mask, "---------- encoding response header: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&out_message->buffer)); \
    ret = aer_message_encode(ctx, out_message); \
    triton_debug(encoding_dbg_mask, "---------- encoding response header completed: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&out_message->buffer)); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(ctx, out_message); \
        return ret; \
    } \
    triton_debug(encoding_dbg_mask, "---------- encoding response: " #__fname__ " cptr=%p ----------\n", \
                 triton_buffer_cptr(&out_message->buffer)); \
    ret = aer_encode_triton_ret_t(&(out_message->buffer), "error", &sret); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(ctx, out_message); \
        return ret; \
    } \
    if(sret == TRITON_SUCCESS) \
    { \
        ret = aer_encode_##__outcanontype__(&(out_message->buffer), #__outname__, &__outname__); \
        if(ret != TRITON_SUCCESS) \
        { \
            triton_debug(encoding_dbg_mask, "---------- encoding response completed: " #__fname__ " cptr=%p ----------\n", \
                         triton_buffer_cptr(&out_message->buffer)); \
            aer_message_destroy(ctx, out_message); \
            return ret; \
        } \
        ret = aer_encoding_finish(&(out_message->buffer)); \
        triton_debug(encoding_dbg_mask, "---------- encoding request completed: " #__fname__ " cptr=%p ----------\n", \
                     triton_buffer_cptr(&out_message->buffer)); \
        if(ret != TRITON_SUCCESS) \
        { \
            aer_message_destroy(ctx, out_message); \
            return ret; \
        } \
    } \
    else \
    { \
        ret = aer_encoding_finish(&(out_message->buffer)); \
        triton_debug(encoding_dbg_mask, "---------- encoding request completed: " #__fname__ " cptr=%p ----------\n", \
                     triton_buffer_cptr(&out_message->buffer)); \
        if(ret != TRITON_SUCCESS) \
        { \
            aer_message_destroy(ctx, out_message); \
            return ret; \
        } \
    } \
    return ret; \
}

#define AER_MK_OP_STMTS(__fname__) \
{ \
    if(__op_id_##__fname__) \
    { \
        return __op_id_##__fname__; \
    } \
    __op_id_##__fname__ = aer_service_unique_id(#__fname__); \
    return __op_id_##__fname__; \
}

#define AER_MK_REG_DECLS(__service_name__) \
    triton_ret_t ret;

#define AER_MK_REG_BLOCK(__fname__) \
{ \
    ret = aer_service_register(__get_op_id_##__fname__(), #__fname__, __service_##__fname__); \
    if(ret != TRITON_SUCCESS) return ret; \
}

#define AER_MK_REG_START(__service_name__) \
    ret = TRITON_SUCCESS;

#define AER_MK_REG_END(__service_name__) \
    return ret;

#define AER_MK_STATIC_OP_DECL(__fname__) \
    static uint64_t __op_id_##__fname__ = 0;

#define AER_MK_REG_CTOR_FNDEF(__service_name__) \
static __attribute__((constructor)) void aer_remote_##__service_name__##_init_register(void) \
{ \
    triton_init_register("aesop.remote." #__service_name__, aer_remote_register_##__service_name__, NULL, NULL, "aesop.remote.service"); \
}

#define AER_MK_STRUCT_DECLS(__name__, params...) \
    triton_ret_t aer_init_##__name__(void * vx, ##params); \
    triton_ret_t aer_copy_##__name__(void * vx, void *vv); \
    triton_ret_t aer_destroy_##__name__(void * vx);

#endif
