#ifndef AE_DEBUG_H
#define AE_DEBUG_H

/* Use triton debug interface for aesop debugging.  To use aesop without
 * triton, replace the following #defines with your own.
 */
#include "src/common/triton-debug.h"

extern triton_debug_mask_t ae_debug_blocking_funs;

#define ae_debug_blocking(__format, ...) \
    triton_debug(ae_debug_blocking_funs, __format , ## __VA_ARGS__)

extern triton_debug_mask_t ae_debug_pbranch_state;

#define ae_debug_pbranch(__format, ...) \
    triton_debug(ae_debug_pbranch_state, __format , ## __VA_ARGS__)

extern triton_debug_mask_t ae_debug_cancel_mask;

#define ae_debug_cancel(__format, ...) \
    triton_debug(ae_debug_cancel_mask, __format , ## __VA_ARGS__)


#endif

