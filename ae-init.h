#ifndef AE_INIT_H
#define AE_INIT_H

/**
 * Aesop uses the Triton init interfaces.  Replace the below
 * macros with your own if you want to use Aesop without Triton.
 */

#include "src/common/triton-init.h"

#define aesop_module_init(...) triton_init(__VA_ARGS__)
#define aesop_module_finalize triton_finalize

#endif
