/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#ifndef AE_TYPES_H
#define AE_TYPES_H

/**
 * Aesop uses Triton types (uint128 in particular).  Replace
 * the following #defines with your own impl if you don't
 * want to use Triton.
 */

#include <triton-uint128.h>

#define ae_op_id_t uint128_t
#define ae_op_id_clear triton_uint128_setzero
#define ae_op_id_iszero triton_uint128_iszero
#define ae_op_id_set triton_uint128_set

#define ae_op_id_equal(id1, id2) !memcmp(&id1, &id2, sizeof(ae_op_id_t))

#endif

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
