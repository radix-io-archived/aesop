#include "src/aesop/aesop.h"
#include "src/aesop/opcache.h"
#include "jenkins-hash.h"

#include <errno.h>
#include <opa_primitives.h>

/* Don't try to do manual memory management */
#define TRITON_OPCACHE_MALLOC

#define TRITON_OPCACHE_ARRAY_COUNT 32
/* UNUSED #define TRITON_OPCACHE_MAX_INDEX (0xFFFFFF) */

/* number of items that must be in the queue per thread before we wake up
 * another thread 
 * TODO: should be configurable
 */
#define THREAD_WORK_THRESHOLD 4

struct thread_data
{
    ae_ops_t thread_queue;
    int active;
    triton_mutex_t mutex;
    triton_cond_t cond;
    void* parent;
};

struct ae_opcache
{
#ifndef TRITON_OPCACHE_MALLOC
    void *array[TRITON_OPCACHE_ARRAY_COUNT];
    int array_count;
    int size;
    int count;
    ae_ops_t free_list;
#endif
    triton_mutex_t mutex;
    triton_cond_t cond;
    OPA_int_t num_ops_in_use;
    void (*completion_fn)(struct ae_opcache* opcache, struct ae_op* op);
    OPA_int_t num_threads;
    int num_threads_active;
    pthread_t* tids;
    ae_ops_t thread_queue;
    int typesize;
    int member_offset;
    struct thread_data* thread_data_array;
};

static void* thread_pool_fn(void* foo);
static void* thread_pool_fn_with_affinity(void* foo);

static void* thread_pool_fn_with_affinity(void* foo)
{
    struct thread_data* tdata = (struct thread_data*)foo;
    ae_opcache_t cache = (ae_opcache_t)tdata->parent;
    struct ae_op* op;

    while(OPA_load_int(&cache->num_threads) > 0)
    {
        triton_mutex_lock(&tdata->mutex);
        while((op = ae_ops_dequeue(&tdata->thread_queue)) == NULL)
        {
            tdata->active = 0;
            pthread_cond_wait(&tdata->cond, &tdata->mutex);
            tdata->active = 1;
        }
        triton_mutex_unlock(&tdata->mutex);
        cache->completion_fn(cache, op);
    }
    return(NULL);
}


static void* thread_pool_fn(void* foo)
{
    ae_opcache_t cache = (ae_opcache_t)foo;
    struct ae_op* op;

    while(OPA_load_int(&cache->num_threads) > 0)
    {
        triton_mutex_lock(&cache->mutex);
        while((op = ae_ops_dequeue(&cache->thread_queue)) == NULL)
        {
            cache->num_threads_active--;
            pthread_cond_wait(&cache->cond, &cache->mutex);
            cache->num_threads_active++;
        }

        if(ae_ops_count(&cache->thread_queue)/cache->num_threads_active >
            THREAD_WORK_THRESHOLD &&
            cache->num_threads_active < OPA_load_int(&cache->num_threads))
        {
            /* there is enough work available that we could make use of
             * another thread
             */
            triton_cond_signal(&cache->cond);
        }
        triton_mutex_unlock(&cache->mutex);
        cache->completion_fn(cache, op);
    }
    return(NULL);
}

int ae_opcache_set_threads_with_affinity(ae_opcache_t cache, 
    void(*completion_fn)(ae_opcache_t opcache, struct ae_op* op), 
    int num_threads)
{
    int i;
    int ret;

    OPA_store_int (&cache->num_threads, num_threads);
    cache->num_threads_active = num_threads;
    cache->tids = (pthread_t*)malloc(num_threads*sizeof(pthread_t));
    if(!cache->tids)
        return(-1);
    cache->completion_fn = completion_fn;

    cache->thread_data_array = (struct thread_data*)malloc(num_threads*sizeof(*cache->thread_data_array));
    if(!cache->thread_data_array)
        return(-1);

    for(i=0; i<num_threads; i++)
    {
        triton_mutex_init(&cache->thread_data_array[i].mutex, NULL);
        triton_cond_init(&cache->thread_data_array[i].cond, NULL);
        ae_ops_init(&cache->thread_data_array[i].thread_queue);
        cache->thread_data_array[i].parent = cache;
        cache->thread_data_array[i].active = 1;
        
        ret = pthread_create(&cache->tids[i], NULL, thread_pool_fn_with_affinity, &cache->thread_data_array[i]);
        if(ret != 0)
        {
            return(-1);
        }
    }
    
    return(0);
}


int ae_opcache_set_threads(ae_opcache_t cache, 
    void(*completion_fn)(ae_opcache_t opcache, struct ae_op* op), 
    int num_threads)
{
    int i;
    int ret;

    OPA_store_int (&cache->num_threads, num_threads);
    cache->num_threads_active = num_threads;
    cache->tids = (pthread_t*)malloc(num_threads*sizeof(pthread_t));
    if(!cache->tids)
        return(-1);
    cache->completion_fn = completion_fn;

    for(i=0; i<num_threads; i++)
    {
        ret = pthread_create(&cache->tids[i], NULL, thread_pool_fn, cache);
        if(ret != 0)
        {
            return(-1);
        }
    }
    
    return(0);
}

void ae_opcache_complete_op_threaded_with_affinity(ae_opcache_t cache, struct ae_op* op, void* affinity_data, int affinity_data_size)
{
    uint32_t pc = 0, pb = 0;
    int thread_index = 0;
    int num_threads = OPA_load_int(&cache->num_threads);

    assert(affinity_data); /* we could pick thread randomly if not set... */

    if(num_threads == 0)
    {
        cache->completion_fn(cache, op);
        return;
    }

    /* hash the affinity data to get a consistent thread selection */
    bj_hashlittle2(affinity_data, affinity_data_size, &pc, &pb);
    thread_index = pc % num_threads;

    triton_mutex_lock(&cache->thread_data_array[thread_index].mutex);
    ae_ops_enqueue(op, &cache->thread_data_array[thread_index].thread_queue);
    if(!cache->thread_data_array[thread_index].active)
    {
        triton_cond_signal(&cache->thread_data_array[thread_index].cond);
    }
    triton_mutex_unlock(&cache->thread_data_array[thread_index].mutex);

    return;
}

void ae_opcache_complete_op_threaded(ae_opcache_t cache, struct ae_op* op)
{
    if(OPA_load_int(&cache->num_ops_in_use) > 1)
    {
        triton_mutex_lock(&cache->mutex);
        ae_ops_enqueue(op, &cache->thread_queue);
        assert(cache->num_threads_active > -1);
        if(cache->num_threads_active == 0)
        {
            /* we need to wake up at least one servicing thread */
            triton_cond_signal(&cache->cond);
        }
        triton_mutex_unlock(&cache->mutex);
    } else 
    {
        //triton_mutex_unlock(&cache->mutex);
        cache->completion_fn(cache, op);
    } 
    return;
}


int ae_opcache_init(int typesize, int member_offset, int init_size, ae_opcache_t *cache)
{
    struct ae_opcache *c;
    c = malloc(sizeof(*c));
    if(!c)
    {
        return(-1);
    }

#ifndef TRITON_OPCACHE_MALLOC
    c->size = init_size;
    c->array_count = 1;
    c->array[0] = malloc(typesize * init_size);
    if(!c->array[0])
    {
        free(c);
        return(-1);
    }
    c->count = 0;
    ae_ops_init(&(c->free_list));
#endif

    triton_mutex_init(&c->mutex, NULL);
    triton_cond_init(&c->cond, NULL);
    ae_ops_init(&c->thread_queue);
    OPA_store_int (&c->num_ops_in_use, 0);
    OPA_store_int (&c->num_threads, 0);
    c->num_threads_active = 0;
    c->typesize = typesize;
    c->member_offset = member_offset;
    *cache = c;
    return(0);
}
    
#ifndef TRITON_OPCACHE_MALLOC
static int ae_opcache_double(ae_opcache_t cache)
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
        return(-1);
    }
    cache->array_count++;
    cache->size *= 2;
    return(0);
}
#endif

void ae_opcache_destroy(ae_opcache_t cache)
{
    int i;
    int nthreads;
#ifndef TRITON_OPCACHE_MALLOC
    triton_mutex_lock(&cache->mutex);
    for(i = 0; i < cache->array_count; ++i)
    {
        free(cache->array[i]);
    }
    triton_mutex_unlock(&cache->mutex);
#endif
    nthreads = OPA_swap_int (&cache->num_threads, 0);
    for(i=0; i<nthreads; i++)
    {
        pthread_join(cache->tids[i], NULL);
    }
    cache->num_threads_active = 0;
    if(nthreads > 0)
        free(cache->tids);
    triton_mutex_destroy(&cache->mutex);
    triton_cond_destroy(&cache->cond);
    ae_ops_destroy(&cache->thread_queue);
    free(cache);
    return;
}

#if 0
static int ae_opcache_size(ae_opcache_t cache)
{
    return cache->size;
}

static int ae_opcache_count(ae_opcache_t cache)
{
    int count;

    triton_mutex_lock(&cache->mutex);
    count = cache->count;
    triton_mutex_unlock(&cache->mutex);

    return count;
}
#endif

struct ae_op *ae_opcache_get(ae_opcache_t cache)
{
    struct ae_op *op;
#ifdef TRITON_OPCACHE_MALLOC
    op = (struct ae_op *) ( (char*) malloc (cache->typesize) +
          cache->member_offset);

    if (!op)
       return op;

    ae_op_clear(op);
    if(OPA_load_int (&cache->num_threads) > 0)
    {
        OPA_incr_int (&cache->num_ops_in_use);
    }
#else
    int aind, count;

    triton_mutex_lock(&cache->mutex);
    if(ae_ops_empty(&cache->free_list))
    {
        if(cache->count == cache->size)
        {
            if (ae_opcache_double(cache) != 0)
            {
               triton_mutex_unlock (&cache->mutex);
               return 0;
            }
        }

        aind = cache->array_count - 1;
        count = (cache->array_count > 1) ?
            cache->count - (cache->size / 2) : cache->count;

        op = (struct ae_op *)(((char *)cache->array[aind]) +
                              (count * cache->typesize) +
                              cache->member_offset);
        ae_op_clear(op);

        ++cache->count;
    }
    else
    {
        op = ae_ops_dequeue(&cache->free_list);
    }
    OPA_incr_int (&cache->num_op_in_use);
    triton_mutex_unlock(&cache->mutex);
    /* should always succeed since we're not allocating mem */
    assert(op);
#endif
    return op;
}

void ae_opcache_put(ae_opcache_t cache, struct ae_op *op)
{
#ifdef TRITON_OPCACHE_MALLOC
    free ((char*) op - cache->member_offset);
    if(OPA_load_int (&cache->num_threads) > 0)
    {
        OPA_decr_int(&cache->num_ops_in_use);
    }
#else
    triton_mutex_lock(&cache->mutex);
    ae_ops_enqueue(op, &cache->free_list);
    OPA_decr_int(&cache->num_ops_in_use);
    triton_mutex_unlock(&cache->mutex);
#endif

    return;
}

/*
struct ae_op * ae_opcache_lookup(ae_opcache_t cache, cache_id_t id)
{
#ifdef TRITON_OPCACHE_MALLOC
   return (struct ae_op *) id;
#else
    int aind, count;

    // the code below assumes we have 32 bits available
    assert (sizeof (cache_id_t) >= 4);

    // we get the array index from the top 6 bits
    aind = (id >> 25);
    count = id & 0xFFFFFF;

    return (struct ae_op *)(((char *)cache->array[aind]) + 
                            (count * cache->typesize) +
                            cache->member_offset);
#endif
}
*/

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
