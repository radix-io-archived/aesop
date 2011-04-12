#ifndef AE_LOG_H
#define AE_LOG_H

/* Use triton log interface for aesop logging.  To use aesop without
 * triton, replace the following #defines with your own.
 */
#include "src/common/triton-log.h"

#define aesop_err(...) triton_err(triton_log_default, __VA_ARGS__)

#endif
