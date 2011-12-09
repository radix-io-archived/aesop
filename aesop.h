#ifndef __AESOP_H__
#define __AESOP_H__

#include "src/aesop/ae-base.h"
#include "src/aesop/ae-error.h"
#include "src/aesop/ae-thread.h"
#include "src/aesop/ae-list.h"
#include "src/aesop/ae-log.h"
#include "src/aesop/ae-debug.h"
#include "src/aesop/ae-init.h"

#include "src/aesop/resource.h"
#include "src/aesop/op.h"
#include "src/aesop/opcache.h"

ae_ret_t aesop_init(void);
void aesop_finalize(void);

int aesop_set_config(const char* key, const char* value);

#endif
