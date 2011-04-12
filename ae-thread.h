#ifndef AE_THREAD_H
#define AE_THREAD_H

/**
 * Aesop uses Triton thread interfaces by default.  If
 * you want to use Aesop without Triton, replace the
 * following macros with your own.
 */
#include "src/common/triton-thread.h"

typedef triton_mutex_t ae_mutex_t;
#define ae_mutex_init triton_mutex_init
#define ae_mutex_lock triton_mutex_lock
#define ae_mutex_unlock triton_mutex_unlock

#endif
