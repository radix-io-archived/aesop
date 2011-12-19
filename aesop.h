#ifndef __AESOP_H__
#define __AESOP_H__

#include "ae-base.h"
#include "ae-error.h"
#include "ae-thread.h"
#include "ae-list.h"
#include "ae-log.h"
#include "ae-debug.h"

#include "resource.h"
#include "op.h"
#include "opcache.h"


int aesop_init(const char *resource_list);
void aesop_finalize(void);

int aesop_set_config(const char* key, const char* value);
int aesop_set_debugging(const char* resource, int value);

#endif
