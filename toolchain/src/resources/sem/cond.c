/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

/* This is an implementation of a condition variable API for Aesop.  
 *
 * TODO: this implementation duplicates much of the logic of the sem
 * resource.  The two need to be merged or one built atop the other.  See
 * trac #69
 */

#include <errno.h>
#include <aesop/aesop.h>
#include <aesop/resource.h>
#include "src/resources/sem/cond.h"

struct aesop_cond_s
{
    ae_ops_t opqueue;
};

static triton_mutex_t cond_done_mutex;
static ae_ops_t cond_done_queue;
static int aesop_cond_resource_id;
static triton_mutex_t module_lock = TRITON_MUTEX_INITIALIZER;
static int module_refcount = 0;

int aesop_cond_init(aesop_cond_t *cond)
{
    struct aesop_cond_s *s;
    s = malloc(sizeof(*s));
    if(s == NULL)
    {
        return AE_ERR_NOMEM;
    }
    ae_ops_init(&s->opqueue);
    *cond = s;
    return AE_SUCCESS;
}

void aesop_cond_destroy(aesop_cond_t cond)
{
    assert(ae_ops_empty(&cond->opqueue));
    free(cond);
}
      
ae_define_post(int, aesop_cond_obj_wait, aesop_cond_t cond, triton_mutex_t *mutex, aesop_cond_id_t *result)
{

    if(ae_resource_is_cancelled())
    {
        *__ae_retval = AE_ERR_CANCELLED;
        return AE_IMMEDIATE_COMPLETION;
    }

    if(result == NULL)
    {
        result = malloc(sizeof(*result));
        if(!result)
        {
            return(AE_ERR_NOMEM);
        }
	result->internal = 1;
    }
    else
    {
	result->internal = 0;
    }

    assert(triton_mutex_trylock(mutex) == EBUSY);

    ae_op_clear(&result->op);
    ae_op_fill(&result->op);
    result->mutex = mutex;
    result->op_id = ae_id_gen(aesop_cond_resource_id, (uint64_t)result);
    result->state = AESOP_COND_STATE_SCHEDULED;

    ae_ops_enqueue(&result->op, &cond->opqueue);
    triton_mutex_unlock(result->mutex);

    *__ae_op_id = result->op_id;
    return AE_SUCCESS;
}

void aesop_cond_notify_all(aesop_cond_t cond)
{
    struct ae_op *op;
    aesop_cond_id_t *result;
    int found_ops = 0;

    while(!ae_ops_empty(&cond->opqueue))
    {
        found_ops = 1;

        /* remove op from conduling queue */
        op = ae_ops_dequeue(&cond->opqueue);
	result = ae_op_entry(op, struct aesop_cond_id_s, op);
        result->state = AESOP_COND_STATE_DONE;

        /* add op to done queue */
        triton_mutex_lock(&cond_done_mutex);
        ae_ops_enqueue(op, &cond_done_queue);
        triton_mutex_unlock(&cond_done_mutex);
    }

    if(found_ops)
    {
        ae_resource_request_poll(aesop_cond_resource_id);
    }

    return;
}
        
void aesop_cond_notify_next(aesop_cond_t cond)
{
    struct ae_op *op;
    aesop_cond_id_t *result;
    if(!ae_ops_empty(&cond->opqueue))
    {
        op = ae_ops_dequeue(&cond->opqueue);
	result = ae_op_entry(op, struct aesop_cond_id_s, op);
        result->state = AESOP_COND_STATE_DONE;

        /* add op to done queue */
        triton_mutex_lock(&cond_done_mutex);
        ae_ops_enqueue(op, &cond_done_queue);
        ae_resource_request_poll(aesop_cond_resource_id);
        triton_mutex_unlock(&cond_done_mutex);
    }
    return;
}

void aesop_cond_notify_specific(aesop_cond_t cond, aesop_cond_id_t *id)
{
    ae_ops_del(&id->op);
    id->state = AESOP_COND_STATE_DONE;

    triton_mutex_lock(&cond_done_mutex);
    ae_ops_enqueue(&id->op, &cond_done_queue);
    ae_resource_request_poll(aesop_cond_resource_id);
    triton_mutex_unlock(&cond_done_mutex);

    return;
}

static int aesop_cond_cancel(ae_op_id_t op_id)
{
    int resource_id;
    aesop_cond_id_t *rid;
    intptr_t tmp_ptr;
    int ret;

    tmp_ptr = ae_id_lookup(op_id, &resource_id);
    rid = (aesop_cond_id_t *)tmp_ptr;

    assert(resource_id == aesop_cond_resource_id);

    triton_mutex_lock(rid->mutex);
    if(rid->state == AESOP_COND_STATE_SCHEDULED)
    {
        ae_ops_del(&rid->op);
        rid->state = AESOP_COND_STATE_CANCELED;
        triton_mutex_lock(&cond_done_mutex);
        ae_ops_enqueue(&rid->op, &cond_done_queue);
        ae_resource_request_poll(aesop_cond_resource_id);
        triton_mutex_unlock(&cond_done_mutex);
        ret = AE_SUCCESS;
    }
    else
    {
        ret = AE_ERR_OTHER;
    }
    triton_mutex_unlock(rid->mutex);
    return ret;
}

static int aesop_cond_poll(void *arg)
{
    struct ae_op *op;
    struct aesop_cond_id_s *result;
    int internal;
    int normal_completion;

    triton_mutex_lock(&cond_done_mutex);
    while(ae_ops_count(&cond_done_queue) > 0)
    {
        int ret;
        void (*callback)(void *, int);
        void *user_ptr; 

        op = ae_ops_dequeue(&cond_done_queue);
        triton_mutex_unlock(&cond_done_mutex);
	result = ae_op_entry(op, struct aesop_cond_id_s, op);
        triton_mutex_lock(result->mutex);

        internal = result->internal;
        ret = result->state == AESOP_COND_STATE_CANCELED ? AE_ERR_CANCELLED : AE_SUCCESS;

        normal_completion = ae_op_complete(op);
        /* TODO: see aethread.c comments; it isn't clear how to get
         * ae_op_complete to return 0.
         */
        assert(normal_completion); 
        if(internal)
        {
            ae_op_execute(op, int, ret);
            free(result);
        }
        else
        {
            callback = op->callback;
            user_ptr = op->user_ptr;
            /* NOTE: the callback in this case might free the control
             * structure that holds the op, so we can't touch it any more
             * after calling the callback.
             */
	    ae_op_clear(op);
            callback(user_ptr, ret);
        }

        triton_mutex_lock(&cond_done_mutex);
    }
    triton_mutex_unlock(&cond_done_mutex);

    return AE_SUCCESS;
}

static struct ae_resource aesop_cond_resource = 
{
    .resource_name = "cond",
    .poll = aesop_cond_poll,
    .cancel = aesop_cond_cancel
};

int aesop_cond_resource_init(void)
{
    int ret;

    triton_mutex_lock(&module_lock);

    if(!module_refcount)
    {
        ae_ops_init(&cond_done_queue);
        triton_mutex_init(&cond_done_mutex, NULL);

        ret = ae_resource_register(&aesop_cond_resource, &aesop_cond_resource_id);
        if(ret!= AE_SUCCESS)
        {
            triton_mutex_unlock(&module_lock);
            return(AE_ERR_UNKNOWN);
        }
    }
    module_refcount++;
    triton_mutex_unlock(&module_lock);

    return(AE_SUCCESS);
}

void aesop_cond_resource_finalize(void)
{
    triton_mutex_lock(&module_lock);
    module_refcount--;

    if(!module_refcount)
    {
        ae_resource_unregister(aesop_cond_resource_id);
    }
    triton_mutex_unlock(&module_lock);
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
