#ifndef __OPCACHE_H__
#define __OPCACHE_H__

#include "op.h"
#include "ae-error.h"

typedef struct ae_opcache *ae_opcache_t;

#define AE_OPCACHE_INIT(_optype, _member, _size, _name) \
    ae_opcache_init(sizeof(_optype), (unsigned long)(&((_optype *)0)->_member), _size, (_name))

/* complete an operation by returning it to opcache and invoking callback */
/* note that __error_code is copied on purpose for safety in case it is a
 * member of the __op structure.
 *
 * It releases the entry by calling opcache_put
 *
 * ae_opcache_complete_op is thread-safe with respect to the opcache.
 */
#define ae_opcache_complete_op(__opcache, __op, __ret_type, __error_code) do { \
    void (*__callback)(void *, __ret_type) = (__op)->callback; \
    void* __user_ptr = (__op)->user_ptr; \
    __ret_type __saved_error_code = __error_code; \
    __callback(__user_ptr, __saved_error_code); \
    ae_opcache_put(__opcache, __op); \
    } while(0)

void ae_opcache_complete_op_threaded(ae_opcache_t cache, struct ae_op* op);

void ae_opcache_complete_op_threaded_with_affinity(ae_opcache_t cache, struct ae_op* op, void* affinity_data, int affinity_data_size);

/**
 * Create an opcache.
 * init_size is a hint and may be ignored.
 * \return 0 on success, -1 on failure
 */
int ae_opcache_init(int typesize, int member_offset, int init_size,
      ae_opcache_t * cache);
   
/**
 * Activates a thread pool to run callbacks for the opcache
 * \return 0 on success, -1 on failure
 */
int ae_opcache_set_threads(ae_opcache_t cache, 
    void(*completion_fn)(ae_opcache_t opcache, struct ae_op* op), 
    int num_threads);

/**
 * Activates a thread pool to run callbacks for the opcache; allows users of
 * thread pool to use additional data to pin related operations to the same
 * thread
 * \return 0 on success, -1 on failure
 */
int ae_opcache_set_threads_with_affinity(ae_opcache_t cache, 
    void(*completion_fn)(ae_opcache_t opcache, struct ae_op* op), 
    int num_threads);

/**
 * Destroy the given opcache.
 * Note that all entries obtained from this cache are released and
 * invalidated.
 */
void ae_opcache_destroy(ae_opcache_t cache);

/**
 * Obtain an ae_op entry. The entry will have a valid 
 * op->cache_id.
 *
 * ae_opcache_get is thread-safe.
 */
struct ae_op *ae_opcache_get(ae_opcache_t cache);


/**
 * Return ae_op entry to the cache
 *
 * ae_opcache_put is thread-safe.
 */
void ae_opcache_put(ae_opcache_t cache, struct ae_op *op);

#endif

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
