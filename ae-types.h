#ifndef AE_TYPES_H
#define AE_TYPES_H

/**
 * Aesop uses Triton types (uint128 in particular).  Replace
 * the following #defines with your own impl if you don't
 * want to use Triton.
 */

#include "src/common/triton-string.h"
#include "src/common/triton-uint128.h"

#define ae_op_id_t uint128_t
#define ae_op_id_clear triton_uint128_setzero
#define ae_op_id_iszero triton_uint128_iszero
#define ae_op_id_set triton_uint128_set

#define ae_op_id_equal(id1, id2) !memcmp(&id1, &id2, sizeof(ae_op_id_t))

#define ae_string_t triton_string_t

#endif
