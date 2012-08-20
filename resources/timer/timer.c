/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */


#define _GNU_SOURCE

#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include "aesop.h"
#include "resource.h"
#include "op.h"
#include "opcache.h"
#include "resources/timer/timer.h"
#include "libev/ev.h"

/* NOTES:
 *
 * This resource sets timers in the aesop libev event loop.  
 *
 * TODO: There are some unresolved thread safety issues here.
 */

#define RESOURCE_NAME "timer"
#define TIMER_DEFAULT_SIZE 1024

static ae_opcache_t timer_opcache = NULL;
static int aesop_timer_resource_id;
static ev_timer timer_watcher;
static struct ev_loop* timer_loop = NULL;
static void timer_cb(EV_P_ ev_timer *w, int revents);
static int64_t global_timer_id_seed = 0;
static int64_t current_timer_id = 0;

struct timer_op
{
    struct timeval timer;
    int milliseconds;
    int64_t timer_id;
    ae_op_id_t op_id;
    struct ae_op op;
    int ret;
};
static triton_mutex_t timer_mutex = TRITON_MUTEX_INITIALIZER;
static ae_ops_t timer_oplist;
static ae_ops_t cancel_oplist;

ae_define_post(int, aesop_timer, int millisecs)
{
    struct timeval adjust, now;
    struct timer_op *top;
    struct ae_op *op;
    struct ae_op *iter, *safe, *holder;
    struct timer_op *timer_op_iter;

    assert(timer_opcache);

    op = ae_opcache_get(timer_opcache);
    ae_op_fill(op);

    top = ae_op_entry(op, struct timer_op, op);
    top->op_id = ae_id_gen(aesop_timer_resource_id, (uintptr_t) op);
    top->ret = 0;
    top->milliseconds = millisecs;

    adjust.tv_sec = (int)(millisecs / 1e3);
    adjust.tv_usec = (millisecs % 1000) * 1e3;
    gettimeofday(&now, NULL);
    timeradd(&adjust, &now, &(top->timer));

    triton_mutex_lock(&timer_mutex);
    top->timer_id = ++global_timer_id_seed;

    holder = NULL;
    /* NOTE: we search the timer list from the back, on the assumption that
     * the newest timers added are most likely to be the furthest away from
     * completion.  We will often see the same millisecs value over and
     * over.
     */
    ae_ops_for_each_reverse(iter, safe, &timer_oplist)
    {
       timer_op_iter = ae_op_entry(iter, struct timer_op, op);
       if(timercmp(&(top->timer), &(timer_op_iter->timer), >))
       {
           break;
       }
       holder = iter;
    }
    if(holder)
    {
        ae_ops_insert_before(op, holder, &timer_oplist);
    }
    else
    {
        ae_ops_enqueue(op, &timer_oplist);
    }

    *__ae_op_id = top->op_id;

    triton_mutex_unlock(&timer_mutex);

    ae_resource_request_poll(op->ctx, aesop_timer_resource_id); 

    return AE_SUCCESS;
}

static int aesop_timer_poll(ae_context_t context, void *arg)
{
    struct ae_op *op;
    struct ae_op *op_head;
    struct timer_op *top;
    struct timer_op *top_head;
    struct timeval result, now;
    ev_tstamp seconds;

    /* Did this end up at the head of the queue? */
    triton_mutex_lock(&timer_mutex);
    op_head = ae_ops_peek(&timer_oplist);
    triton_mutex_unlock(&timer_mutex);

    /*
     * Update the libev timer if there is a new deadline
     */
    if ((op_head != NULL) &&
        (top_head = ae_op_entry(op_head, struct timer_op, op)) &&
        (top_head->timer_id != current_timer_id))
    {
        current_timer_id = top_head->timer_id;

        top = ae_op_entry(op_head, struct timer_op, op);
        gettimeofday(&now, NULL);
        timersub(&(top->timer), &now, &result);
        seconds = (double)result.tv_sec + ((double)result.tv_usec / 1e6);

        /* timer has already expired, whoops.
         * handle completing it in the libev callback.
         */
        if (seconds < 0.0)
        {
            seconds = 0.0001;
        }

        if(timer_loop)
            ev_timer_stop(timer_loop, &timer_watcher);
        ev_timer_set(&timer_watcher, seconds, 0);
        timer_loop = ae_resource_get_eloop(op_head->ctx);
        ev_timer_start(timer_loop, &timer_watcher);
    }

    /* harvest any timers that have been cancelled */
    triton_mutex_lock(&timer_mutex);
    while((op = ae_ops_dequeue(&cancel_oplist)))
    {
        triton_mutex_unlock(&timer_mutex);

        top = ae_op_entry(op, struct timer_op, op);
        assert(top->ret == -ECANCELED);
        ae_opcache_complete_op(timer_opcache, op, int, top->ret);
        triton_mutex_lock(&timer_mutex);
    }
    triton_mutex_unlock(&timer_mutex);

    return AE_SUCCESS;
}

static int aesop_timer_cancel(ae_context_t triton_ctx, ae_op_id_t op_id)
{
    /* TODO: do we have to check for races here (trying to cancel an ae_op
     * that no longer exists) , or does aesop do that for us?
     */
    int resource_id;
    struct ae_op *op;
    struct timer_op *top;
    ae_context_t ctx;
        
    /* lock to keep the op from disappearing out from under us due to
     * completion 
     */
    triton_mutex_lock(&timer_mutex);

    op = intptr2op (ae_id_lookup(op_id, &resource_id));
    assert(resource_id == aesop_timer_resource_id);

    /* should still be in an op list */
    if(!ae_ops_exists(&timer_oplist, &op->link))
    {
        /* already complete */
        triton_mutex_unlock(&timer_mutex);
        return(AE_SUCCESS);
    }

    top = ae_op_entry(op, struct timer_op, op);
    top->ret = -ECANCELED;

    /* NOTE: don't bother modifying the posix timer when we cancel
     * something.  In the worst case it will fire one time more than is
     * strictly necessary and reset itself.
     */

    assert(ae_ops_exists(&timer_oplist, &op->link));
    ae_ops_del(op);

    /* move to a special queue of cancelled timers */
    ctx = op->ctx;
    ae_ops_enqueue(op, &cancel_oplist);

    triton_mutex_unlock(&timer_mutex);

    /* request a poll for aesop to harvest the cancelled timer */
    ae_resource_request_poll(ctx, aesop_timer_resource_id); 

    return AE_SUCCESS;
}

struct ae_resource aesop_timer_resource =
{
    .resource_name = RESOURCE_NAME,
    .poll_context = aesop_timer_poll,
    .cancel = aesop_timer_cancel,
    .config_array = NULL
};

__attribute__((constructor)) void aesop_timer_init_register(void);

__attribute__((constructor)) void aesop_timer_init_register(void)
{
    ae_resource_init_register("timer", aesop_timer_init, aesop_timer_finalize);
}

int aesop_timer_init(void)
{
    int ret;

    ev_init(&timer_watcher, timer_cb);
    
    ae_ops_init(&timer_oplist);
    ae_ops_init(&cancel_oplist);

    ret = AE_OPCACHE_INIT(struct timer_op, op, TIMER_DEFAULT_SIZE, &timer_opcache);
    if(ret != 0)
    {
        return AE_ERR_SYSTEM;
    }

    return ae_resource_register(&aesop_timer_resource, &aesop_timer_resource_id);
}

void aesop_timer_finalize(void)
{
    ae_resource_unregister(aesop_timer_resource_id);

    ae_opcache_destroy(timer_opcache);
}

static void timer_cb(EV_P_ ev_timer *w, int revents)
{
    struct ae_op *gop;
    struct timer_op *top;
    struct timeval now;
    struct timeval diff;
    ev_tstamp tstamp;
    
    triton_mutex_lock(&timer_mutex);

    gop = ae_ops_peek(&timer_oplist);
    top = ae_op_entry(gop, struct timer_op, op);

    gettimeofday(&now, NULL);

    /* complete the timers that have hit (t < now) */
    while(top && timercmp(&(top->timer), &now, <))
    {
	/* this timer has hit, so we pop and call callback */
	gop = ae_ops_dequeue(&timer_oplist);
        top = ae_op_entry(gop, struct timer_op, op);
        triton_mutex_unlock(&timer_mutex);

        ae_opcache_complete_op(timer_opcache, gop, int, 0);
	/* setup for next iteration */
        triton_mutex_lock(&timer_mutex);
	gop = ae_ops_peek(&timer_oplist);
        top = ae_op_entry(gop, struct timer_op, op);
    }

    /* is there anything left in the queue?  If so, arm the timer again */
    gop = ae_ops_peek(&timer_oplist);
    top = ae_op_entry(gop, struct timer_op, op);
    if(top)
    {
        current_timer_id = top->timer_id;

        gettimeofday(&now, NULL);
        timersub(&top->timer, &now, &diff);
        if(diff.tv_sec < 0 || diff.tv_usec < 0)
        {
            /* whoops, we already need to harvest this one */
            diff.tv_sec = 0;
            diff.tv_usec = 1;
        }

        tstamp = (ev_tstamp)diff.tv_sec + (ev_tstamp)diff.tv_usec / 1000000.0;
        if(timer_loop)
            ev_timer_stop(timer_loop, &timer_watcher);
        ev_timer_set(&timer_watcher, tstamp, 0);
        timer_loop = ae_resource_get_eloop(gop->ctx);
        ev_timer_start(timer_loop, &timer_watcher);
    }
    
    triton_mutex_unlock(&timer_mutex);

    return;
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
