/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <errno.h>
#include <unistd.h>
#include "aesop.h"
#include "op.h"
#include "opcache.h"
#include "resources/thread/aethread.h"
#include "resource.h"

struct aethread_op
{
    ae_op_id_t op_id;
    struct ae_op op;
    int ret;
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
    triton_list_link_t link;
};

static triton_mutex_t aethread_cancel_lock = TRITON_MUTEX_INITIALIZER;
static ae_ops_t       aethread_cancel_queue;

/* TODO: make configurable, or better yet dynamic based a runtime
 * calculation that compares typical op service times against how long it
 * takes to wake up a thread.
 */
#define THREAD_WORK_THRESHOLD 1 
static int aethread_resource_id;
#define AETHREAD_DEFAULT_OPCACHE_SIZE 1024
static ae_opcache_t aethread_opcache;

static triton_mutex_t module_lock = TRITON_MUTEX_INITIALIZER;
static int module_refcount = 0;

static void* thread_pool_fn(void* foo);

/* list of active groups.  We have to track them all so that we can look
 * through the groups to find operations to be cancelled.
 */
static triton_list_t group_list = TRITON_LIST_STATIC_INITIALIZER(group_list);
static pthread_mutex_t group_mutex = PTHREAD_MUTEX_INITIALIZER;

void aethread_destroy_group(struct aethread_group* group)
{
    int i;

    pthread_mutex_lock(&group_mutex);
    triton_list_del(&group->link);
    pthread_mutex_unlock(&group_mutex);

    /* TODO: should we do something to handle operations still in the queue
     * for this group?
     */

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

    assert(module_refcount);

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

    pthread_mutex_lock(&group_mutex);
    triton_list_add_back(&group->link, &group_list);
    pthread_mutex_unlock(&group_mutex);

    return(group);
}

static void* thread_pool_fn(void* foo)
{
    struct aethread_group* group = (struct aethread_group*)foo;
    struct ae_op* op = NULL;

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

        ae_opcache_complete_op(aethread_opcache, op, int, 0);
    }

    return(NULL);
}

ae_define_post(int, aethread_hint, struct aethread_group *group)
{
    struct ae_op *op;
    struct aethread_op *a_op;
 
    assert(module_refcount && group);

    if(!aethread_opcache)
    {
        fprintf(stderr, "Error: thread resource not module_refcount.\n");
        assert(0);
    }

    if (ae_resource_is_cancelled ())
    {
       *__ae_retval = AE_ERR_CANCELLED;
       return AE_IMMEDIATE_COMPLETION;
    }

    op = ae_opcache_get(aethread_opcache);
    ae_op_fill(op);

    a_op = ae_op_entry(op, struct aethread_op, op);
    a_op->op_id = ae_id_gen(aethread_resource_id, (intptr_t) op);
    a_op->ret = 0;

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

static int triton_aethread_poll(ae_context_t context, void * data)
{
   triton_mutex_lock (&aethread_cancel_lock);
   while (!ae_ops_empty (&aethread_cancel_queue))
   {
      ae_op_t * op = ae_ops_dequeue (&aethread_cancel_queue);

      triton_mutex_unlock (&aethread_cancel_lock);
      ae_opcache_complete_op(aethread_opcache, op, int, AE_ERR_CANCELLED);
      triton_mutex_lock (&aethread_cancel_lock);
   }
   triton_mutex_unlock (&aethread_cancel_lock);
    return AE_SUCCESS;
}

static int triton_aethread_cancel(ae_context_t triton_ctx, ae_op_id_t op_id)
{
    int resource_id;
    struct ae_op *op;
    struct aethread_op *aop;
    struct aethread_group *group, *tmpgroup;
    int found = 0;
        
    triton_mutex_lock(&group_mutex);

    op = intptr2op (ae_id_lookup(op_id, &resource_id));
    assert(resource_id == aethread_resource_id);

    /* search to see if this operation is still in a queue waiting for
     * service.  If so, cancel it.  If not, let it complete naturally.
     */
    triton_list_for_each_entry(group, tmpgroup, &group_list, struct aethread_group, link)
    {
        pthread_mutex_lock(&group->pool_mutex);
        if(ae_ops_exists(&group->oplist, &op->link))
        {
            /* found it */
            triton_list_del(&op->link);
            aop = ae_op_entry(op, struct aethread_op, op);
            aop->ret = AE_ERR_CANCELLED;

            triton_mutex_lock (&aethread_cancel_lock);
            ae_ops_enqueue(op, &aethread_cancel_queue);
            triton_mutex_unlock (&aethread_cancel_lock);

            found = 1;
        }
        pthread_mutex_unlock(&group->pool_mutex);
        if(found)
            break;
    }

    triton_mutex_unlock(&group_mutex);

    if (found)
       ae_resource_request_poll(triton_ctx, aethread_resource_id);

    /**
     * TODO: this needs to be fixed! Return code should depend on result of
     * cancel.
     */
    return AE_SUCCESS;
}

struct ae_resource triton_aethread_resource =
{
    .resource_name = "thread",
    .poll_context = triton_aethread_poll,
    .cancel = triton_aethread_cancel,
    .config_array = NULL
};

int aethread_init(void)
{
    int ret;

    triton_mutex_lock(&module_lock);

    if(!module_refcount)
    {

        ae_ops_init (&aethread_cancel_queue);
        
        ret = AE_OPCACHE_INIT(struct aethread_op, op, AETHREAD_DEFAULT_OPCACHE_SIZE, &aethread_opcache);
        if(ret != 0)
        {
            triton_mutex_unlock(&module_lock);
            return AE_ERR_SYSTEM;
        }

        ret = ae_resource_register(&triton_aethread_resource, 
            &aethread_resource_id);
        if(ret != 0)
        {
            triton_mutex_unlock(&module_lock);
            return ret;
        }
   }
   module_refcount++;
   triton_mutex_unlock(&module_lock);

   return AE_SUCCESS;
}

void aethread_finalize(void)
{
    triton_mutex_lock(&module_lock);
    module_refcount--;

    if(!module_refcount)
    {
        ae_resource_unregister(aethread_resource_id);
        ae_opcache_destroy(aethread_opcache);
        ae_ops_destroy (&aethread_cancel_queue);
    }
    triton_mutex_unlock(&module_lock);
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
