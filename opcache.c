
#include <errno.h>
#include "src/aesop/aesop.h"
#include "src/aesop/opcache.h"
#include "src/common/triton-error.h"

#define TRITON_OPCACHE_ARRAY_COUNT 32
#define TRITON_OPCACHE_MAX_INDEX (0xFFFFFF)

struct ae_opcache
{
    void *array[TRITON_OPCACHE_ARRAY_COUNT];
    int array_count;
    int size;
    int count;
    triton_mutex_t mutex;
    ae_ops_t free_list;
    int typesize;
    int member_offset;
};

triton_ret_t ae_opcache_init(int typesize, int member_offset, int init_size, ae_opcache_t *cache)
{
    struct ae_opcache *c;
    c = malloc(sizeof(*c));
    if(!c)
    {
        return TRITON_ERR_NOMEM;
    }
    c->size = init_size;
    c->array_count = 1;
    c->array[0] = malloc(typesize * init_size);
    if(!c->array[0])
    {
        free(c);
        return TRITON_ERR_NOMEM;
    }
    c->typesize = typesize;
    c->member_offset = member_offset;
    c->count = 0;
    triton_mutex_init(&c->mutex, NULL);
    ae_ops_init(&(c->free_list));
    *cache = c;
    return TRITON_SUCCESS;
}
    
static triton_ret_t ae_opcache_double(ae_opcache_t cache)
{
    int i;
    cache->array[cache->array_count] = malloc(cache->typesize * cache->size);
    if(!cache->array[cache->array_count])
    {
        for(i = 0; i < cache->array_count; ++i)
        {
            free(cache->array[i]);
        }
        free(cache);
        triton_mutex_unlock(&cache->mutex);
        return TRITON_ERR_NOMEM;
    }
    cache->array_count++;
    cache->size *= 2;
    return TRITON_SUCCESS;
}

triton_ret_t ae_opcache_double_size(ae_opcache_t cache)
{
    triton_ret_t ret;
    triton_mutex_lock(&cache->mutex);
    ret = ae_opcache_double(cache);
    triton_mutex_unlock(&cache->mutex);
    return ret;
}

void ae_opcache_destroy(ae_opcache_t cache)
{
    int i;
    triton_mutex_lock(&cache->mutex);
    for(i = 0; i < cache->array_count; ++i)
    {
        free(cache->array[i]);
    }
    triton_mutex_unlock(&cache->mutex);
    free(cache);
    return;
}

inline int ae_opcache_size(ae_opcache_t cache)
{
    return cache->size;
}

inline int ae_opcache_count(ae_opcache_t cache)
{
    int count;

    triton_mutex_lock(&cache->mutex);
    count = cache->count;
    triton_mutex_unlock(&cache->mutex);

    return count;
}

struct ae_op *ae_opcache_get(ae_opcache_t cache)
{
    int aind, count;
    struct ae_op *op;

    triton_mutex_lock(&cache->mutex);
    if(ae_ops_empty(&cache->free_list))
    {
        if(cache->count == cache->size)
        {
            ae_opcache_double(cache);
        }

        aind = cache->array_count - 1;
        count = (cache->array_count > 1) ?
            cache->count - (cache->size / 2) : cache->count;

        op = (struct ae_op *)(((char *)cache->array[aind]) +
                              (count * cache->typesize) +
                              cache->member_offset);
        ae_op_clear(op);
        op->cache_id = (aind << 25) | count;
        ++cache->count;
    }
    else
    {
        op = ae_ops_dequeue(&cache->free_list);
    }
    triton_mutex_unlock(&cache->mutex);
    return op;
}

void ae_opcache_put(ae_opcache_t cache, struct ae_op *op)
{
    triton_mutex_lock(&cache->mutex);
    ae_ops_enqueue(op, &cache->free_list);
    triton_mutex_unlock(&cache->mutex);

    return;
}

struct ae_op *ae_opcache_lookup(ae_opcache_t cache, int id)
{
    int aind, count;

    /* we get the array index from the top 6 bits */
    aind = (id >> 25);
    count = id & 0xFFFFFF;

    return (struct ae_op *)(((char *)cache->array[aind]) + 
                            (count * cache->typesize) +
                            cache->member_offset);
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
