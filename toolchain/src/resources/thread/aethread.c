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
#include "src/resources/thread/aethread.h"
#include "resource.h"

/* represents each independent operation submitted to the thread pool */
struct aethread_op
{
    ae_op_id_t op_id;  /* aesop operation identifier */
    struct ae_op op;   /* aesop operation struct */
    int ret;           /* return code for the operation */
};

/* a group represents a particular pool of threads */
struct aethread_group
{
    int pool_size;           /* total number of threads in pool */
    int pool_active_threads; /* number of threads that are currently active */
    int pool_running;        /* is the pool currently runing or not (1 or 0) */
    pthread_t *pool_tids;    /* thread id for each thread in pool */
    pthread_cond_t pool_cond;
    pthread_mutex_t pool_mutex;
    ae_ops_t oplist;         /* queue of operations to service */
    triton_list_link_t link; /* link in list of all pools */
};

static triton_mutex_t aethread_cancel_lock = TRITON_MUTEX_INITIALIZER;
static ae_ops_t aethread_cancel_queue;

/* TODO: make configurable, or better yet dynamic based a runtime
 * calculation that compares typical op service times against how long it
 * takes to wake up a thread.
 */
#define THREAD_WORK_THRESHOLD 1
static int aethread_resource_id;

static triton_mutex_t module_lock = TRITON_MUTEX_INITIALIZER;
static int module_refcount = 0;

static void *thread_pool_fn(
    void *foo);
static void group_cleanup(struct aethread_group *group);

/* list of active groups.  We have to track them all so that we can look
 * through the groups to find operations to be cancelled.
 */
static triton_list_t group_list = TRITON_LIST_STATIC_INITIALIZER(group_list);
static pthread_mutex_t group_mutex = PTHREAD_MUTEX_INITIALIZER;

void aethread_destroy_group(
    struct aethread_group *group)
{
    /* remove group from list */
    pthread_mutex_lock(&group_mutex);
    triton_list_del(&group->link);
    pthread_mutex_unlock(&group_mutex);

    /* tell threads to shut down */
    pthread_mutex_lock(&group->pool_mutex);
    assert(ae_ops_count(&group->oplist) == 0);
    group->pool_running = 0;
    pthread_cond_broadcast(&group->pool_cond);
    pthread_mutex_unlock(&group->pool_mutex);

    /* NOTE: we don't deallocate the group here.  The last thread to exit
     * will take care of that.  The reason that we don't do it here is
     * because an aesop callback may trigger destroy_group() itself, making
     * it impossible to wait for running threads to exit in the context of
     * this function.
     */
    return;
}

static void group_cleanup(
    struct aethread_group *group)
{
    pthread_cond_destroy(&group->pool_cond);
    pthread_mutex_destroy(&group->pool_mutex);
    ae_ops_destroy(&group->oplist);
    free(group->pool_tids);
    free(group);

    return;
}


struct aethread_group *aethread_create_group_pool(
    int size)
{
    struct aethread_group *group;
    int i;
    int ret;
    pthread_attr_t attr;

    assert(module_refcount);

    group = malloc(sizeof(*group));
    if (!group)
    {
        return (NULL);
    }
    memset(group, 0, sizeof(*group));

    group->pool_size = size;
    group->pool_tids = malloc(size * sizeof(*group->pool_tids));
    if (!group->pool_tids)
    {
        free(group);
        return (NULL);
    }
    pthread_cond_init(&group->pool_cond, NULL);
    pthread_mutex_init(&group->pool_mutex, NULL);
    ae_ops_init(&group->oplist);

    group->pool_active_threads = size;
    group->pool_running = 1;

    /* Create threads in detached mode. This allows us to control the timing
     * of when threads exit vs. when the call to destroy_group is executed.
     * destroy_group() cannot safely wait for threads to complete in some
     * scenarios.
     */
    ret = pthread_attr_init(&attr);
    assert(ret == 0);  /* TODO: error handling */
    ret = pthread_attr_setdetachstate(&attr,
        PTHREAD_CREATE_DETACHED);
    assert(ret == 0);  /* TODO: error handling */
    for (i = 0; i < size; i++)
    {
        ret = pthread_create(&group->pool_tids[i], &attr, thread_pool_fn, group);
        assert(ret == 0);       /* TODO: err handling */
    }

    pthread_mutex_lock(&group_mutex);
    triton_list_add_back(&group->link, &group_list);
    pthread_mutex_unlock(&group_mutex);

    return (group);
}

static void *thread_pool_fn(
    void *foo)
{
    struct aethread_group *group = (struct aethread_group *) foo;
    struct ae_op *op = NULL;
    int normal_completion;
    int size;

    while (1)
    {
        pthread_mutex_lock(&group->pool_mutex);
        while (ae_ops_empty(&group->oplist) && group->pool_running)
        {
            group->pool_active_threads--;
            pthread_cond_wait(&group->pool_cond, &group->pool_mutex);
            group->pool_active_threads++;
        }
        if (!group->pool_running)
        {
            group->pool_size--;
            size = group->pool_size;
            triton_mutex_unlock(&group->pool_mutex);
            /* the last thread to exit cleans up memory associated with the
             * group
             */
            if(size == 0)
                group_cleanup(group);
            pthread_exit(NULL);
        }
        op = ae_ops_dequeue(&group->oplist);

        if (ae_ops_count(&group->oplist) / group->pool_active_threads >=
            THREAD_WORK_THRESHOLD && group->pool_active_threads < group->pool_size)
        {
            /* there is enough work available that we could make use of
             * another thread
             */
            pthread_cond_signal(&group->pool_cond);
        }

        pthread_mutex_unlock(&group->pool_mutex);

        /* no work; just run callback in the context of this thread */
        /* trigger completion of the operation */
        
        normal_completion = ae_op_complete(op);
        if(normal_completion)
        {
            ae_op_execute(op, int, 0);
            free(ae_op_entry(op, struct aethread_op, op));
        }
        else
        {
            /* A return code of 0 here would indicate that a cancel() is
             * already in progress for this operation.
             *
             * In theory we need to skip completing the operation in this
             * case and allow cancel to complete it.  However, there are
             * mutex locks preventing both this function and cancel from
             * finding the same operation.  Both code paths hold a resource
             * mutex while searching for the operation and removing it from the
             * queue.  So we should still complete the operation here?
             *
             * TODO: need to figure out how to construct a test case that
             * triggers this specific scenario.
             */
            assert(0);

            ae_op_execute(op, int, 0);
            free(ae_op_entry(op, struct aethread_op, op));
        }
    }

    return (NULL);
}

ae_define_post(int, aethread_hint, struct aethread_group * group)
{
    struct ae_op *op;
    struct aethread_op *a_op;

    assert(module_refcount && group);

    if (ae_resource_is_cancelled())
    {
        *__ae_retval = AE_ERR_CANCELLED;
        return AE_IMMEDIATE_COMPLETION;
    }

    a_op = malloc(sizeof(*a_op));
    if(!a_op)
    {
        return AE_ERR_SYSTEM;
    }
    op = &a_op->op;
    ae_op_fill(op);
    ae_ops_link_init(op);

    a_op = ae_op_entry(op, struct aethread_op, op);
    a_op->op_id = ae_id_gen(aethread_resource_id, (intptr_t) op);
    a_op->ret = 0;

    *__ae_op_id = a_op->op_id;

    pthread_mutex_lock(&group->pool_mutex);
    ae_ops_enqueue(op, &group->oplist);
    if (group->pool_active_threads == 0)
    {
        /* need to wake up at least one servicing thread */
        pthread_cond_signal(&group->pool_cond);
    }
    pthread_mutex_unlock(&group->pool_mutex);

    return AE_SUCCESS;
}

static int triton_aethread_poll(
    void *data)
{
    triton_mutex_lock(&aethread_cancel_lock);
    while (!ae_ops_empty(&aethread_cancel_queue))
    {
        ae_op_t *op = ae_ops_dequeue(&aethread_cancel_queue);

        triton_mutex_unlock(&aethread_cancel_lock);
        ae_op_execute(op, int, AE_ERR_CANCELLED);
        free(ae_op_entry(op, struct aethread_op, op));
        triton_mutex_lock(&aethread_cancel_lock);
    }
    triton_mutex_unlock(&aethread_cancel_lock);
    return AE_SUCCESS;
}

static int triton_aethread_cancel(
    ae_op_id_t op_id)
{
    int resource_id;
    struct ae_op *op;
    struct aethread_op *aop;
    struct aethread_group *group, *tmpgroup;
    int found = 0;

    triton_mutex_lock(&group_mutex);

    op = intptr2op(ae_id_lookup(op_id, &resource_id));
    assert(resource_id == aethread_resource_id);

    /* search to see if this operation is still in a queue waiting for
     * service.  If so, cancel it.  If not, let it complete naturally.
     */
    triton_list_for_each_entry(group, tmpgroup, &group_list,
                               struct aethread_group,
                               link)
    {
        pthread_mutex_lock(&group->pool_mutex);
        if (ae_ops_exists(&group->oplist, &op->link))
        {
            /* found it */
            triton_list_del(&op->link);
            aop = ae_op_entry(op, struct aethread_op,
                              op);
            aop->ret = AE_ERR_CANCELLED;

            triton_mutex_lock(&aethread_cancel_lock);
            ae_ops_enqueue(op, &aethread_cancel_queue);
            triton_mutex_unlock(&aethread_cancel_lock);

            found = 1;
        }
        pthread_mutex_unlock(&group->pool_mutex);
        if (found)
            break;
    }

    triton_mutex_unlock(&group_mutex);

    if (found)
    {
        ae_resource_request_poll(aethread_resource_id);
        return (AE_SUCCESS);
    }

    /* If we were not able to cancel the operation then return an error */
    return (AE_ERR_OTHER);
}

struct ae_resource triton_aethread_resource = {
    .resource_name = "thread",
    .poll = triton_aethread_poll,
    .cancel = triton_aethread_cancel,
};

int aethread_init(
    void)
{
    int ret;

    triton_mutex_lock(&module_lock);

    if (!module_refcount)
    {

        ae_ops_init(&aethread_cancel_queue);

        ret = ae_resource_register(&triton_aethread_resource,
                                   &aethread_resource_id);
        if (ret != 0)
        {
            triton_mutex_unlock(&module_lock);
            return ret;
        }
    }
    module_refcount++;
    triton_mutex_unlock(&module_lock);

    return AE_SUCCESS;
}

void aethread_finalize(
    void)
{
    triton_mutex_lock(&module_lock);
    module_refcount--;

    if (!module_refcount)
    {
        ae_resource_unregister(aethread_resource_id);
        ae_ops_destroy(&aethread_cancel_queue);
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
