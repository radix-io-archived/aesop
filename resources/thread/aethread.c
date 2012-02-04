#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <errno.h>
#include <unistd.h>
#include "aesop.h"
#include "op.h"
#include "opcache.h"
#include "resources/thread/aethread.h"

struct aethread_op
{
    ae_op_id_t op_id;
    struct ae_op op;
};

struct aethread_group
{
    int pool_size;
    int pool_active;
    int pool_started;
    pthread_t* pool_tids;
    pthread_cond_t pool_cond;
    pthread_mutex_t pool_mutex;
    ae_ops_t oplist;
};
/* TODO: make configurable, or better yet dynamic based a runtime
 * calculation that compares typical op service times against how long it
 * takes to wake up a thread.
 */
#define THREAD_WORK_THRESHOLD 1 
static int aethread_resource_id;
#define AETHREAD_DEFAULT_OPCACHE_SIZE 1024
static ae_opcache_t aethread_opcache;

static void* thread_pool_fn(void* foo);

void aethread_destroy_group(struct aethread_group* group)
{
    int i;

    pthread_mutex_lock(&group->pool_mutex);
    group->pool_started = 0;
    pthread_cond_broadcast(&group->pool_cond);
    pthread_mutex_unlock(&group->pool_mutex);

    for(i=0; i<group->pool_size; i++)
    {
        pthread_join(group->pool_tids[i], NULL);
    }

    free(group->pool_tids);
    
    return;
}

struct aethread_group* aethread_create_group_pool(int size)
{
    struct aethread_group* group;
    int i;
    int ret;

    group = malloc(sizeof(*group));
    if(!group)
    {
        return(NULL);
    }
    memset(group, 0, sizeof(*group));

    group->pool_size = size;
    group->pool_tids = malloc(size*sizeof(*group->pool_tids));
    if(!group->pool_tids)
    {
        free(group);
        return(NULL);
    }
    pthread_cond_init(&group->pool_cond, NULL);
    pthread_mutex_init(&group->pool_mutex, NULL);
    ae_ops_init(&group->oplist);

    group->pool_active = size;
    group->pool_started = 1;
    for(i=0; i<size; i++)
    {
        ret = pthread_create(&group->pool_tids[i], NULL, thread_pool_fn, group);
        assert(ret == 0); /* TODO: err handling */
    }

    return(group);
}

static void* thread_pool_fn(void* foo)
{
    struct aethread_group* group = (struct aethread_group*)foo;
    struct ae_op* op = NULL;
    void* user_ptr;
    void (*callback)(void*);

    while(1)
    {
        pthread_mutex_lock(&group->pool_mutex);
        while(ae_ops_empty(&group->oplist) && group->pool_started)
        {
            group->pool_active--;
            pthread_cond_wait(&group->pool_cond, &group->pool_mutex);
            group->pool_active++;
        }
        if(!group->pool_started)
        {
            triton_mutex_unlock(&group->pool_mutex);
            pthread_exit(NULL);
        }
        op = ae_ops_dequeue(&group->oplist);

        if(ae_ops_count(&group->oplist)/group->pool_active >=
            THREAD_WORK_THRESHOLD &&
            group->pool_active < group->pool_size)
        {
            /* there is enough work available that we could make use of
             * another thread
             */
            pthread_cond_signal(&group->pool_cond);
        }

        pthread_mutex_unlock(&group->pool_mutex);    
        
        /* no work; just run callback in the context of this thread */
        /* trigger completion of the operation */
        /* TODO: follow up on this.  Can't use the usual complete_op macro
         * because there is no return code
         */
        user_ptr = op->user_ptr;
        callback = op->callback;
        callback(user_ptr);
        ae_opcache_put(aethread_opcache, op);
    }

    return(NULL);
}

int aethread_hint(void (*__ae_callback)(void *ptr), \
                         void *__ae_user_ptr,                              \
                         ae_hints_t *__ae_hints,                           \
                         ae_context_t __ae_ctx,                            \
                         ae_op_id_t *__ae_op_id,                           \
                         int __ae_internal,                                \
                         struct aethread_group* group)
{
    struct ae_op *op;
    struct aethread_op *a_op;
    pthread_t tid;
    pthread_attr_t attr;
    int ret;
 
    if(!aethread_opcache)
    {
        fprintf(stderr, "Error: thread resource not initialized.\n");
        assert(0);
    }

    op = ae_opcache_get(aethread_opcache);
    ae_op_fill(op);

    a_op = ae_op_entry(op, struct aethread_op, op);
    a_op->op_id = ae_id_gen(aethread_resource_id, (intptr_t) op);

    *__ae_op_id = a_op->op_id;

    pthread_mutex_lock(&group->pool_mutex);
    ae_ops_enqueue(op, &group->oplist);
    if(group->pool_active == 0)
    {
        /* need to wake up at least one servicing thread */
        pthread_cond_signal(&group->pool_cond);
    }
    pthread_mutex_unlock(&group->pool_mutex);

    return AE_SUCCESS;
}

static int triton_aethread_poll(ae_context_t context)
{
    return AE_SUCCESS;
}

static int triton_aethread_cancel(ae_context_t triton_ctx, ae_op_id_t op_id)
{
    return AE_SUCCESS;
}

struct ae_resource triton_aethread_resource =
{
    .resource_name = "thread",
    .poll_context = triton_aethread_poll,
    .cancel = triton_aethread_cancel,
    .config_array = NULL
};

__attribute__((constructor)) void triton_aethread_init_register(void);

__attribute__((constructor)) void triton_aethread_init_register(void)
{
    ae_resource_init_register("thread", aethread_init, aethread_finalize);
}

int aethread_init(void)
{
    int ret;
    
    ret = AE_OPCACHE_INIT(struct aethread_op, op, AETHREAD_DEFAULT_OPCACHE_SIZE, &aethread_opcache);
    if(ret != 0)
    {
        return AE_ERR_SYSTEM;
    }

    return ae_resource_register(&triton_aethread_resource, &aethread_resource_id);
}

void aethread_finalize(void)
{
    ae_resource_unregister(aethread_resource_id);

    ae_opcache_destroy(aethread_opcache);
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
