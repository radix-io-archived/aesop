#ifndef AE_ERROR_H
#define AE_ERROR_H

/* Use triton errors for aesop errors.  To use aesop without
 * triton, replace the following #defines with your own.
 */
#include "src/common/triton-error.h"

#define ae_ret_t triton_ret_t

#define AE_SUCCESS TRITON_SUCCESS
#define AE_IMMEDIATE_COMPLETION TRITON_IMMEDIATE_COMPLETION
#define AE_NOMEM TRITON_ERR_NOMEM
#define AE_TIMEDOUT TRITON_ERR_TIMEDOUT
#define AE_NOSYS TRITON_ERR_NOSYS

#define aesop_error_assert(__ret) triton_error_assert(__ret)

#define AE_ERR_NOT_FOUND (-1)  /* object or entity not found */
#define AE_ERR_INVALID (-2)    /* invalid argument */
#define AE_ERR_SYSTEM (-3)     /* critical system error (out of memory, etc.) */

#endif
