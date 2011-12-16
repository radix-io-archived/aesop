#ifndef AE_ERROR_H
#define AE_ERROR_H

#include <assert.h>

/* pre-defined error codes used by aesop */

#define aesop_error_assert(__ret) assert(__ret == AE_SUCCESS || __ret == AE_IMMEDIATE_COMPLETION)

#define AE_SUCCESS 0           /* success */
/* not an error, just means that function completed without blocking */
#define AE_IMMEDIATE_COMPLETION 1  

#define AE_ERR_NOT_FOUND (-1)  /* object or entity not found */
#define AE_ERR_INVALID (-2)    /* invalid argument */
#define AE_ERR_SYSTEM (-3)     /* critical system error (out of memory, etc.) */
#define AE_ERR_EXIST (-4)      /* object or entity already exists */
#define AE_ERR_TIMEDOUT (-5)   /* timed out */
#define AE_ERR_OVERFLOW (-6)   /* overflow of some resource limitation */

#endif
