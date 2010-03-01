#ifndef __AE_REMOTE_PARSER_H__
#define __AE_REMOTE_PARSER_H__

#include "src/common/triton-types.h"
#include "src/common/triton-list.h"
#include "src/common/triton-error.h"
#include "src/common/triton-log.h"
#include "src/remote/remote.hae"

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

#define AER_MK_ENCODE_TYPE_PTR(__type__, __field__) \
{ \
    ret = aer_encode_##__type__(buf, #__field__, x->__field__); \
    if(ret != 0) \
    { \
        return ret; \
    } \
}

#define AER_MK_ENCODE_TYPE(__type__, __field__) \
{ \
    ret = aer_encode_##__type__(buf, #__field__, &x->__field__); \
    if(ret != 0) \
    { \
        return ret; \
    } \
}

#define AER_MK_ENCODE_DECLS(__type__) \
    triton_ret_t ret; \
    __type__ x;

#define AER_MK_ENCODE_STMTS_START(__type__) \
{ \
    x = (__type__)vx; \
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

#define AER_MK_DECODE_TYPE_PTR(__type__, __field__) \
{ \
    ret = aer_decode_##__type__(buf, NULL, x->__field__); \
}

#define AER_MK_DECODE_TYPE(__type__, __field__) \
{ \
    ret = aer_decode_##__type__(buf, NULL, &x->__field__); \
}

#define AER_MK_DECODE_DECLS(__type__) \
    triton_ret_t ret; \
    __type__ x;

#define AER_MK_DECODE_STMTS_START(__type__) \
{ \
    x = (__type__)vx; \
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

#define AER_MK_SIZE_TYPE_PTR(__type__, __field__) \
{ \
    size += aer_encode_size_##__type__ (#__field__, x->__field__); \
}

#define AER_MK_SIZE_TYPE(__type__, __field__) \
{ \
    size += aer_encode_size_##__type__ (#__field__, &x->__field__); \
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

#define AER_MK_INIT_PTR_TYPE(__basetype__, __type__, __field__) \
{ \
    x->__field__ = malloc(sizeof(*(x->__field__))); \
    if(!x->__field__) \
    { \
        aer_destroy_##__basetype__(x); \
        return TRITON_ERR_ENOMEM; \
    } \
    ret = aer_init_##__type__(x->__field__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_destroy_##__basetype__(x); \
        return ret; \
    } \
}

#define AER_MK_INIT_TYPE(__basetype__, __type__, __field__) \
{ \
    ret = aer_init_##__type__(&x->__field__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_destroy_##__basetype__(x); \
        return ret; \
    } \
}

#define AER_MK_INIT_DECLS(__type__) \
    triton_ret_t ret; \
    __type__ x;

#define AER_MK_INIT_STMTS_START(__type__) \
{ \
    x = (__type__)vx; \
}

#define AER_MK_INIT_STMTS_END() \
{ \
    return ret; \
}

#define AER_MK_DESTROY_PTR_TYPE(__basetype__, __type__, __field__) \
{ \
    if(x->__field__ != NULL) \
    { \
        aer_destroy_##__type__(x->__field__); \
        free(x->__field__); \
        x->__field__ = NULL; \
    } \
}

#define AER_MK_DESTROY_TYPE(__basetype__, __type__, __field__) \
{ \
    aer_destroy_##__type__(&x->__field__); \
}

#define AER_MK_DESTROY_DECLS(__type__) \
    __type__ x;

#define AER_MK_DESTROY_STMTS_START(__type__) \
{ \
    x = (__type__)vx; \
}

#define AER_MK_DESTROY_STMTS_END() 

#define AER_MK_STUB_DECL(__ret__, __fname__, params...) \
    __blocking __ret__ remote_##__fname__(triton_node_t id, ##params);

#define AER_MK_STUB_DECLS(__fname__) \
    aer_message_t send_message; \
    aer_message_t recv_message; \
    triton_ret_t ret; \
    uint64_t insize;

#define AER_MK_STUB_BLOCK(__fname__, __intype__, __inname__, __outtype__, __outname__) \
{ \
    ret = aer_message_init(&send_message, AE_REMOTE_MAX_REQUEST_SIZE, __get_op_id_##__fname__()); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    ret = aer_encode_##__intype__(&(send_message.buffer), #__inname__, &__inname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(&send_message); \
        return ret; \
    } \
    ret = aer_message_init(&recv_message, AE_REMOTE_MAX_RESPONSE_SIZE, __get_op_id_##__fname__()); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(&send_message); \
        return ret; \
    } \
    ret = aer_message_sendrecv(id, &send_message, &recv_message); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(&send_message); \
        aer_message_destroy(&recv_message); \
        return ret; \
    } \
    ret = aer_decode_##__outtype__(&(recv_message.buffer), NULL, __outname__); \
    aer_message_destroy(&send_message); \
    aer_message_destroy(&recv_message); \
    return ret; \
}

#define AER_MK_STUB_PTR_BLOCK(__fname__, __intype__, __inname__, __outtype__, __outname__) \
{ \
    ret = aer_message_init(&send_message); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    ret = aer_message_set_op(&send_message, __get_op_id_##__fname__()); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(&send_message); \
        return ret; \
    } \
    ret = aer_encode_##__intype__(&(send_message.buffer), #__inname__, __inname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(&send_message); \
        return ret; \
    } \
    ret = aer_message_init(&recv_message); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(&send_message); \
        return ret; \
    } \
    ret = aer_message_sendrecv(id, &send_message, &recv_message); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(&send_message); \
        aer_message_destroy(&recv_message); \
        return ret; \
    } \
    ret = aer_decode_##__outtype__(&(recv_message.buffer), NULL, __outname__); \
    aer_message_destroy(&send_message); \
    aer_message_destroy(&recv_message); \
    return ret; \
}

#define AER_MK_SERVICE_DECL(__fname__) \
    __blocking triton_ret_t __service_##__fname__(aer_message_t *send_message, aer_message_t *recv_message);

#define AER_MK_SERVICE_FNDEF(__fname__, __intype__, __inname__, __outtype__, __outname__) \
__blocking triton_ret_t __service_##__fname__(aer_message_t *send_message, aer_message_t *recv_message) \
{ \
    triton_ret_t ret; \
    __intype__ __inname__; \
    __outtype__ __outname__; \
    ret = aer_decode_##__intype__(&(recv_message->buffer), NULL, &__inname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    ret = __fname__(__inname__, &__outname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    ret = aer_encode_##__outtype__(&(send_message->buffer), #__outname__, &__outname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(send_message); \
        return ret; \
    } \
    return ret; \
}

#define AER_MK_SERVICE_FNDEF_INPTR(__fname__, __intype__, __inname__, __outtype__, __outname__) \
__blocking triton_ret_t __service_##__fname__(aer_message_t *send_message, aer_message_t *recv_message) \
{ \
    triton_ret_t ret; \
    __intype__ __inname__; \
    __outtype__ __outname__; \
    ret = aer_decode_##__intype__(&(recv_message->buffer), NULL, &__inname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    ret = __fname__(&__inname__, &__outname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        return ret; \
    } \
    ret = aer_encode_##__outtype__(&(send_message->buffer), #__outname__, &__outname__); \
    if(ret != TRITON_SUCCESS) \
    { \
        aer_message_destroy(send_message); \
        return ret; \
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

#define AER_MK_REG_START(__service_name__)

#define AER_MK_REG_END(__service_name__) \
    return ret;

#define AER_MK_STATIC_OP_DECL(__fname__) \
    static uint64_t __op_id_##__fname__ = 0;

#endif
