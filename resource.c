#if !defined (__AESOP_EPOLL) && !defined (__AESOP_LIBEV)
#error Must define one of __AESOP_EPOLL or __AESOP_LIBEV
#endif
#if defined (__AESOP_EPOLL) && defined (__AESOP_LIBEV)
#error Must define exactly one of __AESOP_EPOLL or __AESOP_LIBEV, not both
#endif

#ifdef __AESOP_EPOLL
#include <sys/epoll.h>
#endif
#ifdef __AESOP_LIBEV
#include "src/common/libev/ev.h"
#endif
#include <stddef.h>
#include <unistd.h>
#include <fcntl.h>

#include "src/common/triton-error.h"
#include "src/aesop/resource.h"
#include "src/common/triton-log.h"

struct ae_poll_data
{
#ifdef __AESOP_EPOLL
    int pipe_fds[2];
#endif
#ifdef __AESOP_LIBEV
    ev_async async;
#endif
    triton_ret_t (*poll_context)(ae_context_t context);
    ae_context_t context;
};

struct ae_resource_entry
{
    int id;
    struct ae_poll_data poll_data;
    struct ae_resource *resource;
};

triton_debug_mask_t aesop_debug_cancel_mask;

static int ae_resource_count = 0;
static struct ae_resource_entry ae_resource_entries[AE_MAX_RESOURCES];
#ifdef __AESOP_EPOLL
static int efd = -1;
struct ae_poll_data break_target;
#endif
#ifdef __AESOP_LIBEV
static struct ev_loop *eloop = NULL;
ev_async eloop_breaker;
#endif
static pthread_t ev_loop_thread;

#define AE_RESOURCE_IDX2ID(reindex) (reindex+16)
#define AE_RESOURCE_ID2IDX(rid) (rid-16)

struct ae_context
{
    int id;
    int resource_count;
    int* resource_ids;
    struct ae_poll_data* poll_data;
#ifdef __AESOP_EPOLL
    struct ae_poll_data break_target;
    int efd;
#endif
#ifdef __AESOP_LIBEV
    struct ev_loop *eloop;
    ev_async eloop_breaker;
#endif
};

static int ae_context_count = 0;
static struct ae_context ae_context_entries[AE_MAX_CONTEXTS];

#ifdef __AESOP_EPOLL
triton_ret_t ae_resource_register(struct ae_resource *resource, int *newid)
{
    int reindex = ae_resource_count;
    int ret;
    struct epoll_event event;

    if(efd < 0)
    {
        /* This is the first resource to be registered.  Initialize the
         * epoll fd and create a pipe that can be used to break the epoll
         * wait.
         */
        efd = epoll_create(32);
        if(efd < 0)
        {
            triton_err(triton_log_default, "Error: could not create epoll fd for resource polling.\n");
            return(TRITON_ERR_EPOLL);
        }
        ret = pipe(break_target.pipe_fds);
        if(ret < 0)
        {
            triton_err(triton_log_default, "Error: could not create pipe for resource polling.\n");
            close(efd);
            return(TRITON_ERR_EPOLL);
        }
        fcntl(break_target.pipe_fds[0], F_SETFL, O_NONBLOCK);
        fcntl(break_target.pipe_fds[1], F_SETFL, O_NONBLOCK);
        break_target.context = NULL;
        break_target.poll_context = NULL;
        event.data.ptr = &break_target;
        event.events = EPOLLIN;
        ret = epoll_ctl(efd, EPOLL_CTL_ADD, break_target.pipe_fds[0], &event);
        if(ret < 0)
        {
            triton_err(triton_log_default, "Error: could not epoll fd for resource polling.\n");
            close(efd);
            close(break_target.pipe_fds[0]);
            close(break_target.pipe_fds[1]);
            return(TRITON_ERR_EPOLL);
        }
    }

    if(ae_resource_count == AE_MAX_RESOURCES)
    {
	return TRITON_ERR_INVAL;
    }

    ret = pipe(ae_resource_entries[reindex].poll_data.pipe_fds);
    if(ret < 0)
    {
        triton_err(triton_log_default, "Error: could not create pipe for resource polling.\n");
        return(TRITON_ERR_EPOLL);

    }
    fcntl(ae_resource_entries[reindex].poll_data.pipe_fds[0], F_SETFL, O_NONBLOCK);
    fcntl(ae_resource_entries[reindex].poll_data.pipe_fds[1], F_SETFL, O_NONBLOCK);
    ae_resource_entries[reindex].poll_data.poll_context =
        resource->poll_context;
    ae_resource_entries[reindex].poll_data.context = NULL;
    event.data.ptr = &ae_resource_entries[reindex].poll_data;
    event.events = EPOLLIN;
    ret = epoll_ctl(efd, EPOLL_CTL_ADD, 
        ae_resource_entries[reindex].poll_data.pipe_fds[0], &event);
    if(ret < 0)
    {
        triton_err(triton_log_default, "Error: could not epoll fd for resource polling.\n");
        return(TRITON_ERR_EPOLL);

    }

    ae_resource_entries[reindex].id = AE_RESOURCE_IDX2ID(reindex);
    ae_resource_entries[reindex].resource = resource;
    ae_resource_count++;
    *newid = AE_RESOURCE_IDX2ID(reindex);
    return TRITON_SUCCESS;
}
#endif /* __AESOP_EPOLL */

#ifdef __AESOP_LIBEV
static void ev_break_cb(EV_P_ ev_async *w, int revents)
{
    /* Break the loop.  Note that ev_run() will still process all events
     * that are pending at the time we call this, which is a good thing.
     */
    ev_break(EV_A_ EVBREAK_ONE);
    return;
}

static void ev_async_cb(EV_P_ ev_async *w, int revents)
{
    triton_ret_t tret;

    struct ae_poll_data* poll_data = 
        (struct ae_poll_data*)(((char*)w)-offsetof(struct ae_poll_data, async));

    assert(poll_data->poll_context);
    tret = poll_data->poll_context(poll_data->context);
    /* TODO: error handling? */
    triton_error_assert(tret);
    
    /* Break the loop.  Note that ev_run() will still process all events
     * that are pending at the time we call this, which is a good thing.
     */
    ev_break(EV_A_ EVBREAK_ONE);
    return;
}

triton_ret_t ae_resource_register(struct ae_resource *resource, int *newid)
{
    int reindex = ae_resource_count;
    int ret;

    ev_loop_thread = pthread_self();
    if(eloop == NULL)
    {
        /* This is the first resource to be registered.  Initialize the
         * event loop and start a watcher that can be used to break it's
         * execution
         */
        eloop = EV_DEFAULT;
        ev_async_init(&eloop_breaker, ev_break_cb);
        ev_async_start(eloop, &eloop_breaker);
    }

    if(ae_resource_count == AE_MAX_RESOURCES)
    {
	return TRITON_ERR_INVAL;
    }

    ev_async_init(&ae_resource_entries[reindex].poll_data.async, ev_async_cb);
    ae_resource_entries[reindex].poll_data.poll_context =
        resource->poll_context;
    ae_resource_entries[reindex].poll_data.context = NULL;
    ev_async_start(eloop, &ae_resource_entries[reindex].poll_data.async);

    ae_resource_entries[reindex].id = AE_RESOURCE_IDX2ID(reindex);
    ae_resource_entries[reindex].resource = resource;
    ae_resource_count++;
    *newid = AE_RESOURCE_IDX2ID(reindex);
    return TRITON_SUCCESS;
}
#endif /* __AESOP_LIBEV */

void ae_resource_unregister(int id)
{
    int idx = AE_RESOURCE_ID2IDX(id);

    /* TODO: what about contexts that might include this resource?  Do we
     * just assume that contexts are always closed before resources are
     * unregistered?
     */
#ifdef __AESOP_EPOLL
    epoll_ctl(efd, EPOLL_CTL_DEL, 
        ae_resource_entries[idx].poll_data.pipe_fds[0], NULL);
    close(ae_resource_entries[idx].poll_data.pipe_fds[0]);
    close(ae_resource_entries[idx].poll_data.pipe_fds[1]);
#endif
#ifdef __AESOP_LIBEV
    ev_async_stop(eloop, &ae_resource_entries[idx].poll_data.async);
#endif

    if(idx != (ae_resource_count -1))
    {
	/* this is not the last resource in the table; shift everything down */
	memmove(&ae_resource_entries[idx], &ae_resource_entries[idx+1],
		(sizeof(struct ae_resource_entry) * (ae_resource_count-idx-1)));
    }
    ae_resource_count--;
}

#ifdef __AESOP_EPOLL

/**
 * ae_poll_break() can be used to interrupt a currently executing ae_poll
 * call.  This would typically be used in the linkage between c and aesop
 * functions to allow the c program to continue execution after the final
 * aesop callback has completed.
 */
void ae_poll_break(ae_context_t context)
{
    int write_pipe = -1;
    char onebyte;

    if(pthread_equal(ev_loop_thread, pthread_self()))
    {
        /* this was called from the event loop thread, so we know that it is
         * already awake 
         */
        return;
    }

    if(context)
    {
        write_pipe = context->break_target.pipe_fds[1];
    }
    else
    {
        write_pipe = break_target.pipe_fds[1];
    }

    write(write_pipe, &onebyte, 1);
}

/**
 * ae_resource_request_poll() is used by a resource to inform aesop that the
 * resource needs to be polled.
 */
/* TODO: think about race conditions.  Need any locking here? */
void ae_resource_request_poll(ae_context_t context, int resource_id)
{
    int write_pipe = -1;
    int i;
    int ridx;
    char onebyte = '0';

    if(context)
    {
        /* find this resource in the context */
        for(i=0; i<context->resource_count; i++)
        {
            if(resource_id == context->resource_ids[i])
            {
                write_pipe = context->poll_data[i].pipe_fds[1];
                break;
            }
        }
    }
    else
    {   
        /* find this resource in the global list */
        ridx = AE_RESOURCE_ID2IDX(resource_id);
        write_pipe = ae_resource_entries[ridx].poll_data.pipe_fds[1];
    }

    if(write_pipe < 0)
    {
        triton_err(triton_log_default, "Error: context %p is not configured to handle resource with id %d", context, resource_id);
        for(i=0; i<ae_resource_count; i++)
        {
            if(ae_resource_entries[i].id == resource_id)
            {
                triton_err(triton_log_default, "Consider adding the \"%s\" resource your aesop context.", ae_resource_entries[i].resource->resource_name);
                assert(0);
            }
        }
        triton_err(triton_log_default, "Error: resource_id %d is unknown to aesop.  Are you using a resource that was not initialized?\n", resource_id);
        assert(0);
    }

    write(write_pipe, &onebyte, 1);
    return;
}
#endif /* __AESOP_EPOLL */

#ifdef __AESOP_LIBEV

static void find_async_watcher(ae_context_t context, int resource_id, ev_async** async_out, struct ev_loop** loop_out)
{
    ev_async* async = NULL;
    int i;
    int ridx;
    struct ev_loop *target_loop = NULL;

    if(context)
    {
        /* find this resource in the context */
        for(i=0; i<context->resource_count; i++)
        {
            if(resource_id == context->resource_ids[i])
            {
                async = &context->poll_data[i].async;
                target_loop = context->eloop;
                break;
            }
        }
    }
    else
    {   
        /* find this resource in the global list */
        ridx = AE_RESOURCE_ID2IDX(resource_id);
        async = &ae_resource_entries[ridx].poll_data.async;
        target_loop = eloop;
    }

    if(!async)
    {
        triton_err(triton_log_default, "Error: context %p is not configured to handle resource with id %d", context, resource_id);
        for(i=0; i<ae_resource_count; i++)
        {
            if(ae_resource_entries[i].id == resource_id)
            {
                triton_err(triton_log_default, "Consider adding the \"%s\" resource your aesop context.", ae_resource_entries[i].resource->resource_name);
                assert(0);
            }
        }
        triton_err(triton_log_default, "Error: resource_id %d is unknown to aesop.  Are you using a resource that was not initialized?\n", resource_id);
        assert(0);
    }

    assert(target_loop);

    *async_out = async;
    *loop_out = target_loop;

    return;
}

/**
 * ae_poll_break() can be used to interrupt a currently executing ae_poll
 * call.  This would typically be used in the linkage between c and aesop
 * functions to allow the c program to continue execution after the final
 * aesop callback has completed.
 */
void ae_poll_break(ae_context_t context)
{
    ev_async* breaker = NULL;
    struct ev_loop *target_loop = NULL;

    if(pthread_equal(ev_loop_thread, pthread_self()))
    {
        /* this was called from the event loop thread, so we know that it is
         * already awake 
         */
        return;
    }

    if(context)
    {
        breaker = &context->eloop_breaker;
        target_loop = context->eloop;
    }
    else
    {
        breaker = &eloop_breaker;
        target_loop = eloop;
    }

    ev_async_send(target_loop, breaker);
}

/**
 * ae_resource_request_poll() is used by a resource to inform aesop that the
 * resource needs to be polled.
 */
void ae_resource_request_poll(ae_context_t context, int resource_id)
{
    ev_async* async = NULL;
    struct ev_loop *target_loop = NULL;

    find_async_watcher(context, resource_id, &async, &target_loop);

    ev_async_send(target_loop, async);

    return;
}
#endif /* __AESOP_LIBEV */

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
    triton_ret_t ret;
    int error;

    triton_debug(aesop_debug_cancel_mask, "ae_cancel_op: %llu:%llu\n", llu(op_id.u), llu(op_id.l));

    if(triton_uint128_iszero(op_id))
    {
        return TRITON_SUCCESS;
    }

    ae_id_lookup(op_id, &resource_id);

    if(resource_id == 0)
    {
        intptr_t tmp = ae_id_lookup (op_id, NULL);
	ctl = (struct ae_ctl *) tmp;
        if(!ctl)
        {
            /* No blocking operation associated with this op_id.  Nothing to cancel. */
            return TRITON_SUCCESS;
        }

	error = triton_mutex_lock(&ctl->mutex);
        assert(!error);

	if(ctl->in_pwait)
	{
            ae_op_id_t *children_ids;
            int count, ind;
      
            children_ids = ae_cancel_children(context, ctl, &count);
            error = triton_mutex_unlock(&ctl->mutex);
            assert(!error);

            for(ind = 0; children_ids && ind < count; ++ind)
            {
                ret = ae_cancel_op(context, children_ids[ind]);
                if(ret != TRITON_SUCCESS)
                {
                    /* cancel failed */
                    if(children_ids) free(children_ids);
                    return ret;
                }
            }
            if(children_ids) free(children_ids);
            return TRITON_SUCCESS;
	}
	else
	{
            ae_op_id_t tmpid;
            triton_uint128_set(ctl->current_op_id, tmpid);
            triton_uint128_setzero(ctl->current_op_id);
            error = triton_mutex_unlock(&ctl->mutex);
            assert(!error);

	    ret = ae_cancel_op(context, tmpid);
            return ret;
	}
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

ae_op_id_t ae_id_gen(int resource_id, intptr_t ptr)
{
    ae_op_id_t newid;
    newid.u = resource_id;
    newid.l = ptr;
    return newid;
}

intptr_t ae_id_lookup(ae_op_id_t id, int *resource_id)
{
    if(resource_id) *resource_id = id.u;
    return (intptr_t) id.l;
}

struct op_id_entry
{
    ae_op_id_t op_id;
    triton_list_link_t link;
};

/**
 * ae_cancel_children tries to cancel all the children of this context.  This
 * function returns TRITON_SUCCESS if all operations was successfully cancelled,
 * the callbacks for the operations are responsible for returning TRITON_ERR_CANCELLED
 * in the callback or somehow notifying through the callback that the operation was
 * cancelled.  Right now, if one of the operations fails to be cancelled, we return
 * failure immediately instead of trying to cancel the others.
 */
ae_op_id_t * ae_cancel_children(ae_context_t context, struct ae_ctl *ctl, int *count)
{
    triton_ret_t ret;
    struct ae_ctl *child_ctl;
    struct triton_list_link *entry, *safe;
    ae_op_id_t *op_ids;
    int ind, c;

    triton_debug(aesop_debug_cancel_mask, "ae_cancel_children: %p\n", ctl);

    if(triton_list_empty(&ctl->children))
    {
        /* don't do anything here because there are no children
         * to cancel
         */
        *count = 0;
	return NULL;
    }

    /* need to allocate space here for op ids, since the entire control
     * structure could go away while we're cancelling
     */
    c = triton_list_count(&ctl->children);
    op_ids = malloc(sizeof(*op_ids) * c);
    if(!op_ids)
    {
        *count = 0;
        return NULL;
    }
    *count = c;

    ind = 0;
    triton_list_for_each(entry, safe, &ctl->children)
    {
	child_ctl = triton_list_get_entry(entry, struct ae_ctl, link);
        triton_uint128_set(child_ctl->current_op_id, op_ids[ind]);
        triton_uint128_setzero(child_ctl->current_op_id);
        ++ind;
    }

    *count = c;
    return op_ids;
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

#ifdef __AESOP_EPOLL
#define AE_PIPE_READ_SIZE 128

triton_ret_t ae_poll(ae_context_t context, int millisecs)
{
    triton_ret_t tret;
    int to_poll;
    int ret;
    struct epoll_event* events;
    int event_count;
    int i;
    struct ae_poll_data* poll_data;

    if(context)
    {
        /* use context specific epoll fd */
        to_poll = context->efd;
        event_count = context->resource_count + 1;
    }
    else
    {
        /* use global epoll fd */
        if(efd < 0)
        {
            /* nothing to poll yet */
            return(TRITON_SUCCESS);
        }
        to_poll = efd;
        event_count = ae_resource_count + 1;
    }

    events = malloc(sizeof(*events) * event_count);
    if(!events)
    {
        return(TRITON_ERR_NOMEM);
    }

    /* wait for a resource to indicate that it has something to do */
    event_count = epoll_wait(to_poll, events, event_count, millisecs);


    /* if interrupted, just exit and let the caller try again */
    if(event_count < 0 && errno == EINTR)
    {
        free(events);
        return(TRITON_SUCCESS);
    }

    /* general error */
    if(event_count < 0)
    {
        free(events);
        triton_err(triton_log_default, "Error: could not epoll_wait for resource polling.\n");
        return(TRITON_ERR_EPOLL);
    }

    /* this means that the epoll timed out without finding any work */
    if(event_count == 0)
    {
        free(events);
        return(TRITON_ERR_TIMEDOUT);
    }

    /* poll each resource that needs attention */
    for(i=0; i<event_count; i++)
    {
        char pipebuf[AE_PIPE_READ_SIZE];

        poll_data = (struct ae_poll_data*)events[i].data.ptr;
        assert(poll_data);
        /* empty the pipe (short reads are ok) */
        if(poll_data->pipe_fds[1] != 0) /* skip non-pipes */
        {
            do
            {
                ret = read(events[i].data.fd, pipebuf, AE_PIPE_READ_SIZE);
            } while(ret >= 1 && ret < AE_PIPE_READ_SIZE);
        }

        if(poll_data->poll_context)
        {
            tret = poll_data->poll_context(context);
            if(tret != TRITON_SUCCESS)
            {
                free(events);
                return(tret);
            }
        }
        else
        {
            /* we were just signalled to wake up; no work to do */
        }
    }

    free(events);
    return(TRITON_SUCCESS);
}
#endif /* __AESOP_EPOLL */

#ifdef __AESOP_LIBEV
static void timeout_cb(EV_P_ ev_timer *w, int revents)
{
    int* hit_timeout = (int*)w->data;
    *hit_timeout = 1;
    /* break the loop */
    ev_break(EV_A_ EVBREAK_ONE);
}

triton_ret_t ae_poll(ae_context_t context, int millisecs)
{
    triton_ret_t tret;
    struct ev_loop* target_loop;
    int ret;
    int i;
    ev_timer timeout;
    int hit_timeout = 0;

    if(context)
    {
        /* use context specific event loop */
        target_loop = context->eloop;
    }
    else
    {
        /* use global event loop */
        target_loop = eloop;
    }

    if(millisecs > 0)
    {
        timeout.data = &hit_timeout;
        ev_timer_init(&timeout, timeout_cb, (double)millisecs * 1000.0, 0);
        ev_timer_start(target_loop, &timeout);
        ev_run(target_loop, 0);
    }
    else
    {
        ev_run(target_loop, EVRUN_NOWAIT);
    }

    if(millisecs > 0)
        ev_timer_stop(target_loop, &timeout);

    /* this means that the event loop timed out without finding any work */
    if(hit_timeout == 1)
    {
        return(TRITON_ERR_TIMEDOUT);
    }

    return(TRITON_SUCCESS);
}
#endif /* __AESOP_LIBEV */

#include <stdarg.h>

triton_ret_t _ae_context_create(ae_context_t *context, const char *format __attribute__((unused)), int resource_count, ...)
{
    va_list ap;
    char *rname;
    int cindex = ae_context_count;
    ae_context_t c;
    int i, j, reindex;
    triton_ret_t ret;
#ifdef __AESOP_EPOLL
    struct epoll_event event;
    int ep_ret;
#endif

    if(ae_context_count == AE_MAX_CONTEXTS)
    {
        return TRITON_ERR_INVAL;
    }

    c = &(ae_context_entries[cindex]);
    c->id = ae_context_count;
    ae_context_count++;
    c->resource_count = resource_count;
    c->resource_ids = malloc(sizeof(*c->resource_ids) * resource_count);
    if(!c->resource_ids)
    {
        return TRITON_ERR_NOMEM;
    }
    c->poll_data = malloc(sizeof(*c->poll_data) * resource_count);
    if(!c->poll_data)
    {
        free(c->resource_ids);
        return TRITON_ERR_NOMEM;
    }

#ifdef __AESOP_LIBEV
    c->eloop = ev_loop_new(EVFLAG_AUTO);
    if(!c->eloop)
    {
        triton_err(triton_log_default, "Error: could not create libev event loop.\n");
        free(c->resource_ids);
        free(c->poll_data);
        return(TRITON_ERR_NOMEM);
    }
    ev_async_init(&c->eloop_breaker, ev_break_cb);
    ev_async_start(c->eloop, &c->eloop_breaker);
#endif
#ifdef __AESOP_EPOLL
    c->efd = epoll_create(32);
    if(c->efd < 0)
    {
        triton_err(triton_log_default, "Error: could not create epoll fd for resource polling.\n");
        free(c->resource_ids);
        free(c->poll_data);
        return(TRITON_ERR_EPOLL);
    }
    ep_ret = pipe(c->break_target.pipe_fds);
    if(ep_ret < 0)
    {
        triton_err(triton_log_default, "Error: could not create pipe for resource polling.\n");
        close(c->efd);
        free(c->resource_ids);
        free(c->poll_data);
        return(TRITON_ERR_EPOLL);
    }
    fcntl(c->break_target.pipe_fds[0], F_SETFL, O_NONBLOCK);
    fcntl(c->break_target.pipe_fds[1], F_SETFL, O_NONBLOCK);
    c->break_target.context = c;
    c->break_target.poll_context = NULL;
    event.data.ptr = &c->break_target;
    event.events = EPOLLIN;
    ep_ret = epoll_ctl(c->efd, EPOLL_CTL_ADD, c->break_target.pipe_fds[0], &event);
    if(ep_ret < 0)
    {
        triton_err(triton_log_default, "Error: could not epoll fd for resource polling.\n");
        close(c->efd);
        close(c->break_target.pipe_fds[0]);
        close(c->break_target.pipe_fds[1]);
        free(c->resource_ids);
        free(c->poll_data);
        return(TRITON_ERR_EPOLL);
    }
#endif
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
#ifdef __AESOP_EPOLL
                ep_ret = pipe(c->poll_data[reindex].pipe_fds);
                if(ep_ret < 0)
                {
                    /* TODO: clean up better here */
                    triton_err(triton_log_default, "Error: could not create pipe for resource polling.\n");
                    return(TRITON_ERR_EPOLL);
                }
                fcntl(c->poll_data[reindex].pipe_fds[0], F_SETFL, O_NONBLOCK);
                fcntl(c->poll_data[reindex].pipe_fds[1], F_SETFL, O_NONBLOCK);
#endif
#ifdef __AESOP_LIBEV
                ev_async_init(&c->poll_data[reindex].async, ev_async_cb);
#endif
                c->poll_data[reindex].poll_context = 
                    ae_resource_entries[j].poll_data.poll_context;
                c->poll_data[reindex].context = c;

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

#ifdef __AESOP_EPOLL
                /* monitor the context-specific pipe */
                event.data.ptr = &c->poll_data[reindex];
                event.events = EPOLLIN;
                ep_ret = epoll_ctl(c->efd, EPOLL_CTL_ADD, 
                    c->poll_data[reindex].pipe_fds[0], &event);
                if(ep_ret < 0 && errno != EEXIST)
                {
                    /* TODO: clean up better here */
                    triton_err(triton_log_default, "Error: could not epoll fd for resource polling.\n");
                    return(TRITON_ERR_EPOLL);
                }
#endif
#ifdef __AESOP_LIBEV
                ev_async_start(c->eloop, &c->poll_data[reindex].async);
#endif

                c->resource_ids[reindex] = ae_resource_entries[j].id;

                reindex++;
                resource_found = 1;
                break;
            }
        }
        if(!resource_found)
        {
            va_end(ap);
            return triton_error_wrap(TRITON_ERR_INVAL, TRITON_ADDR_NULL,
                                     "No aesop resource found with name '%s'.", rname);
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
#ifdef __AESOP_EPOLL
        if(context->poll_data[i].pipe_fds[0] >= 0)
            close(context->poll_data[i].pipe_fds[0]);
        if(context->poll_data[i].pipe_fds[1] >= 0)
            close(context->poll_data[i].pipe_fds[1]);
#endif
#ifdef __AESOP_LIBEV
        ev_async_stop(context->eloop, &context->poll_data[i].async);
#endif
    }
    free(context->resource_ids);
    free(context->poll_data);
    context->resource_ids = NULL;
    context->poll_data = NULL;
    context->id = -1;
    context->resource_count = -1;
#ifdef __AESOP_EPOLL
    close(context->efd);
#endif
#ifdef __AESOP_LIBEV
    ev_loop_destroy(context->eloop);
#endif
    return TRITON_SUCCESS;
}

triton_ret_t ae_cancel_branches(struct ae_ctl *ctl)
{
    triton_ret_t ret;
    ae_op_id_t *children_ids;
    int ind, count, error;
    ae_context_t context;

    triton_debug(aesop_debug_cancel_mask, "ae_cancel_branches: %p\n", ctl);

    if(!ctl)
    {
        fprintf(stderr, "can't call ae_cancel_branches from outside of a pbranch context\n");
        assert(ctl);
    }

    error = triton_mutex_lock(&ctl->mutex);
    assert(!error);

    context = ctl->context;
    children_ids = ae_cancel_children(context, ctl, &count);

    error = triton_mutex_unlock(&ctl->mutex);
    assert(!error);

    for(ind = 0; children_ids && ind < count; ++ind)
    {
        ret = ae_cancel_op(context, children_ids[ind]);
        if(ret != TRITON_SUCCESS)
        {
            if(children_ids) free(children_ids);
            return ret;
        }
    }
    if(children_ids) free(children_ids);
    return TRITON_SUCCESS;
}

int ae_count_branches(struct ae_ctl *ctl)
{
    int r, error;
    if(!ctl)
    {
        fprintf(stderr, "can't call ae_cancel_branches from outside of a pbranch context\n");
        assert(ctl);
    }
    error = triton_mutex_lock(&ctl->mutex);
    assert(!error);
    r = triton_list_count(&ctl->children);
    error = triton_mutex_unlock(&ctl->mutex);
    assert(!error);
    return r;
}

void ae_get_stack(struct ae_ctl *ctl, triton_string_t *stack, int *inout_count)
{
    int i = 0;
    while(ctl && i < *inout_count)
    {
        triton_string_init(&(stack[i++]), ctl->name);
        ctl = ctl->parent;
    }

    *inout_count = i;
}

void ae_print_stack(FILE *outstream, struct ae_ctl *ctl)
{
    int i = 0, top;
    char *stack[512];
    do
    {
        stack[i++] = strdup(ctl->name);
        ctl = ctl->parent;
    } while(ctl);

    top = 0;
    for(--i; i >= 0; --i)
    {
        fprintf(outstream, "[%d]: %s\n", top++, stack[i]);
    }

    for(i = 0; i < top; ++i)
    {
        free(stack[i]);
    }
}

#define TRITON_ERROR_WRAP_AESOP_STACK_SIZE 32

triton_ret_t ae_error_wrap_stack(struct ae_ctl *ctl, triton_ret_t parent)
{
    triton_ret_t ret = TRITON_SUCCESS;
    triton_string_t stack[TRITON_ERROR_WRAP_AESOP_STACK_SIZE];
    int rcount, i;
    int count = TRITON_ERROR_WRAP_AESOP_STACK_SIZE;
    ae_get_stack(ctl, stack, &count);
    rcount = count;
    if(count > 0)
    {
        int bleft, offset;
        bleft = count * 1024;
        char *stackstr = malloc(bleft);
        char *ptr = stackstr;

        i = 0;
        while(count-- > 0 && bleft > 0)
        {
            offset = snprintf(ptr, bleft, "\t\t[%d]:\t%s\n", i++, triton_string_get(&stack[count]));
            ptr += offset;
            bleft -= offset;
        }
        ret = triton_error_wrap(parent, TRITON_ADDR_NULL, "Error Stack:\n%s\n", stackstr);
        free(stackstr);
    }

    for(i = 0; i < rcount; ++i)
    {
        triton_string_destroy(&stack[i]);
    }

    return ret;
}
#ifdef __AESOP_LIBEV
struct ev_loop * ae_resource_get_eloop(ae_context_t context)
{
    if(!context)
        return(eloop);
    else
        return(context->eloop);
}
#endif


/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
