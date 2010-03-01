#ifndef __AESOP_H__
#define __AESOP_H__

#include "src/common/triton-base.h"
#include "src/common/triton-error.h"
#include "src/common/triton-thread.h"
#include "src/common/triton-list.h"
#include "src/common/triton-log.h"
#include "src/common/triton-debug.h"

#include "src/aesop/resource.h"
#include "src/aesop/op.h"
#include "src/aesop/opcache.h"

extern triton_debug_mask_t ae_debug_blocking_funs;

triton_ret_t aesop_init(void);
void aesop_finalize(void);

#endif
