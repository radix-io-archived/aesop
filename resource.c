
#include "src/common/triton-error.h"
#include "src/aesop/resource.h"

struct ae_resource_entry
{
    int id;
    struct ae_resource *resource;
};

static int ae_resource_count = 0;
static struct ae_resource_entry ae_resource_entries[AE_MAX_RESOURCES];

#define AE_RESOURCE_IDX2ID(reindex) (reindex+16)
#define AE_RESOURCE_ID2IDX(rid) (rid-16)

struct ae_context
{
    int id;
    int resource_count;
    int *resource_ids;
};

static int ae_context_count = 0;
static struct ae_context ae_context_entries[AE_MAX_CONTEXTS];

triton_ret_t ae_resource_register(struct ae_resource *resource, int *newid)
{
    int reindex = ae_resource_count;

    if(ae_resource_count == AE_MAX_RESOURCES)
    {
	return TRITON_ERR_INVAL;
    }

    ae_resource_entries[reindex].id = AE_RESOURCE_IDX2ID(reindex);
    ae_resource_entries[reindex].resource = resource;
    ae_resource_count++;
    *newid = AE_RESOURCE_IDX2ID(reindex);
    return TRITON_SUCCESS;
}

void ae_resource_unregister(int id)
{
    int idx = AE_RESOURCE_ID2IDX(id);

    if(idx != (ae_resource_count -1))
    {
	/* this is not the last resource in the table; shift everything down */
	memmove(&ae_resource_entries[idx], &ae_resource_entries[idx+1], 
		(sizeof(struct ae_resource_entry) * (ae_resource_count-idx-1)));
    }
    ae_resource_count--;
}

/**
 * ae_cancel_op tries to cancel the operation with the given id.  This
 * function returns TRITON_SUCCESS if the operation was successfully cancelled,
 * the callback for the operation is responsible for returning TRITON_ERR_CANCELLED
 * in the callback or somehow notifying through the callback that the operation was
 * cancelled.
 */ 
triton_ret_t ae_cancel_op(ae_context_t context, ae_op_id_t op_id)
{
    struct ae_ctl *ctl;
    int resource_id, ridx;
    triton_ret_t ret, saved;

    ae_id_lookup(op_id, &resource_id);

    if(resource_id == 0)
    {
	ctl = (struct ae_ctl *)(intptr_t)ae_id_lookup(op_id, NULL);
	triton_mutex_lock(&ctl->mutex);
	if(ctl->cancelled)
	{
	    /* already cancelled! */
	    triton_mutex_unlock(&ctl->mutex);
	    return TRITON_ERR_INVAL;
	}

	ctl->cancelled = 1;
	if(ctl->in_pwait)
	{
            ret = ae_cancel_children(context, ctl);
	}
	else
	{
	    ret = ae_cancel_op(context, ctl->current_op_id);
	}
	triton_mutex_unlock(&ctl->mutex);
	return ret;
    }
    else
    {
        ridx = AE_RESOURCE_ID2IDX(resource_id);
        if(ridx > ae_resource_count)
        {
            return TRITON_ERR_INVAL;
        }

        if(ae_resource_entries[ridx].id != resource_id)
        {
            return TRITON_ERR_INVAL;
        }

        if(ae_resource_entries[ridx].id == -1)
        {
            return TRITON_ERR_INVAL;
        }

        if(ae_resource_entries[ridx].resource->cancel)
        {
            return ae_resource_entries[ridx].resource->cancel(context, op_id);
        }
        else
        {
            return TRITON_SUCCESS;
        }
    }
}

ae_op_id_t ae_id_gen(int resource_id, uint64_t ptr)
{
    ae_op_id_t newid;
    newid.u = resource_id;
    newid.l = ptr;
    return newid;
}

uint64_t ae_id_lookup(ae_op_id_t id, int *resource_id)
{
    if(resource_id) *resource_id = id.u;
    return id.l;
}

/**
 * ae_cancel_children tries to cancel all the children of this context.  This
 * function returns TRITON_SUCCESS if all operations was successfully cancelled,
 * the callbacks for the operations are responsible for returning TRITON_ERR_CANCELLED
 * in the callback or somehow notifying through the callback that the operation was
 * cancelled.  Right now, if one of the operations fails to be cancelled, we return
 * failure immediately instead of trying to cancel the others.
 */ 
triton_ret_t ae_cancel_children(ae_context_t context, struct ae_ctl *ctl)
{
    triton_ret_t ret;
    struct ae_ctl *child_ctl;
    struct triton_list_link *entry, *safe;

    if(triton_list_empty(&ctl->children))
    {
        /* don't do anything here because there are no children
         * to cancel
         */
	return TRITON_SUCCESS;
    }

    triton_list_for_each(entry, safe, &ctl->children)
    {
	child_ctl = triton_list_get_entry(entry, struct ae_ctl, link);
	ret = ae_cancel_op(context, child_ctl->current_op_id);
	if(ret != TRITON_SUCCESS)
	{
	    /* cancel failed */
	    return ret;
	}
    }

    return TRITON_SUCCESS;
}

#include <execinfo.h>
#include <stdio.h>

void ae_backtrace(void)
{
    void *trace[16];
    char **messages = (char **)NULL;
    int size, i;

    size = backtrace(trace, 16);
    messages = backtrace_symbols(trace, size);
    for(i = 0; i < size; ++i)
    {
	fprintf(stderr, "gsl backtrace: %s\n", messages[i]);
    }
}

#include <assert.h>

triton_ret_t ae_poll(ae_context_t context, int millisecs)
{
    int resource_ms, i;
    int rid, ridx;
    int resources_polled = 0;
    triton_ret_t ret;

    if(context)
    {
        resource_ms = millisecs / context->resource_count;
        for(i = 0; i < context->resource_count; ++i)
        {
            rid = context->resource_ids[i];
            ridx = AE_RESOURCE_ID2IDX(rid);
            if(ae_resource_entries[ridx].resource->poll_context)
            {
                ret = ae_resource_entries[ridx].resource->poll_context(context, resource_ms);
                if(ret != TRITON_SUCCESS)
                {
                    return ret;
                }
                resources_polled++;
            }
        }
    }
    else
    {
        resource_ms = millisecs / ae_resource_count;
        for(i = 0; i < ae_resource_count; ++i)
        {
            if(ae_resource_entries[i].resource->poll_context)
            {
                ret = ae_resource_entries[i].resource->poll_context(
                    context, resource_ms);
                if(ret != TRITON_SUCCESS)
                {
                    return ret;
                }
                resources_polled++;
            }
        }
    }

    if(resources_polled == 0)
    {
        int nret;
        struct timespec ts;

        ts.tv_sec = (int)(millisecs / 1e3);
        ts.tv_nsec = (millisecs % 1000) * 1e6;
        nret = nanosleep(&ts, NULL);
        if(nret != 0)
        {
            return triton_error_from_errno(errno);
        }
    }
    return TRITON_SUCCESS;
}

#include <stdarg.h>

triton_ret_t ae_context_create(ae_context_t *context, int resource_count, ...)
{
    va_list ap;
    char *rname;
    int cindex = ae_context_count;
    ae_context_t c;
    int i, j, reindex;
    triton_ret_t ret;

    if(ae_context_count == AE_MAX_CONTEXTS)
    {
        return TRITON_ERR_INVAL;
    }

    c = &(ae_context_entries[cindex]);
    c->id = ae_context_count;
    ae_context_count++;
    c->resource_count = resource_count;
    c->resource_ids = malloc(sizeof(int) * resource_count);
    if(!c->resource_ids)
    {
        return TRITON_ERR_NOMEM;
    }
    reindex = 0;


    /* step through the resource names passed in and register the context with them */ 
    va_start(ap, resource_count);
    for(i = 0; i < resource_count; ++i)
    {
        int resource_found = 0;
        rname = va_arg(ap, char *);

        /* find the matching resource and register the context with that resource */

        for(j = 0; j < ae_resource_count; ++j)
        {
            if(!strcmp(ae_resource_entries[j].resource->resource_name, rname))
            {
                if(ae_resource_entries[j].resource->register_context)
                {
                    ret = ae_resource_entries[j].resource->register_context(c);
                    if(ret != TRITON_SUCCESS)
                    {
                        /* what do we do if a context fails to register with a resource? */
                        va_end(ap);
                        return ret;
                    }
                }
                c->resource_ids[reindex] = ae_resource_entries[j].id;
                reindex++;
                resource_found = 1;
                break;
            }
        }
        if(!resource_found)
        {
            va_end(ap);
            return TRITON_ERR_INVAL;
        }
    }
    va_end(ap);

    *context = c;
    return TRITON_SUCCESS; 
}

triton_ret_t ae_context_destroy(ae_context_t context)
{
    int rid, idx, i;

    for(i = 0; i < context->resource_count; ++i)
    {
        rid = context->resource_ids[i];
        idx = AE_RESOURCE_ID2IDX(rid);
        if(ae_resource_entries[idx].resource->unregister_context)
        {
            ae_resource_entries[idx].resource->unregister_context(context);
        }
    }
    free(context->resource_ids);
    context->resource_ids = NULL;
    context->id = -1;
    context->resource_count = -1;
    return TRITON_SUCCESS;
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
