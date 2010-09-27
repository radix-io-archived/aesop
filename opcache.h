
#ifndef __OPCACHE_H__
#define __OPCACHE_H__

#include "src/aesop/op.h"
#include "src/common/triton-error.h"

typedef struct ae_opcache *ae_opcache_t;

#define AE_OPCACHE_INIT(_optype, _member, _size, _name) \
    ae_opcache_init(sizeof(_optype), (unsigned long)(&((_optype *)0)->_member), _size, (_name))

/* complete an operation by returning it to opcache and invoking callback */
/* note that __error_code is copied on purpose for safety in case it is a
 * member of the __op structure.
 */
#define ae_opcache_complete_op(__opcache, __op, __ret_type, __error_code) do { \
    void (*__callback)(void *, __ret_type) = (__op)->callback; \
    void* __user_ptr = (__op)->user_ptr; \
    __ret_type __saved_error_code = __error_code; \
    ae_opcache_put(__opcache, __op); \
    __callback(__user_ptr, __saved_error_code); \
    } while(0)

triton_ret_t ae_opcache_init(int typesize, int member_offset, int init_size, ae_opcache_t *cache);
   
triton_ret_t ae_opcache_double_size(ae_opcache_t cache);

void ae_opcache_destroy(ae_opcache_t cache);

struct ae_op *ae_opcache_get(ae_opcache_t cache);

void ae_opcache_put(ae_opcache_t cache, struct ae_op *op);

/**
 * Size of the opcache array.  This is the total size of the cache.  As
 * more in-use ops are pulled from the cache (ae_opcache_get), the cache
 * will double in size on demand.  
 */
int ae_opcache_size(ae_opcache_t cache);

/**
 * Count of the entries used in the arrays of the cache.  Once ops are given
 * back to the cache, they get put on a free list, so this count only
 * gets larger, and does not reflect the actual amount of current ops available
 * in the cache without needing to double the size.
 */
int ae_opcache_count(ae_opcache_t cache);

struct ae_op *ae_opcache_lookup(ae_opcache_t cache, int id);

#endif

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
