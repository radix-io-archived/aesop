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

static triton_mutex_t module_lock = TRITON_MUTEX_INITIALIZER;
static int module_refcount = 0;

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

   /**
    * Resources are supposed to check if the calling ctl is in cancelled
    * state and return immediately.
    */
   if (ae_resource_is_cancelled ())
   {
      *__ae_retval = -ECANCELED;
      return AE_IMMEDIATE_COMPLETION;
   }

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
      /* no need to check here if the op already completed; It wouldn't have
       * been put on this queue if it was already marked for pending
       * completion.
       */
      triton_mutex_unlock(&timer_mutex);

      top = ae_op_entry(op, struct timer_op, op);
      assert(top->ret == -ECANCELED);
      ae_opcache_complete_op(timer_opcache, op, int, top->ret);
      triton_mutex_lock(&timer_mutex);
   }
   triton_mutex_unlock(&timer_mutex);

   return AE_SUCCESS;
}

/**
 * This cancel function will not be called if the op was already marked for
 * completion (by ae_op_complete)
 */
static int aesop_timer_cancel(ae_context_t triton_ctx, ae_op_id_t op_id)
{
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

   /* Aesop does not call cancel for items we already marked with 
    * ae_op_complete; Nor should it call cancel for the same op twice.
    *
    * So, the item op will either be:
    *    - on the timer list, not expired
    *    - on the cancelled list already (placed there by the lib ev
    *    callback, which happens if the callback fires and got the lock
    *    before we got the mutex, but after aesop decided to cancel the op.
    */

   /* note: ae_ops_get_queue requires op to point to a valid op;
    * it dereferences the pointer. */
   ae_ops_t * onlist = ae_ops_get_queue (op);

   assert (onlist);

   if (onlist == &cancel_oplist)
   {
      triton_mutex_unlock (&timer_mutex);
      /* item was already placed on the cancelled list by the libev callback
      */
      return AE_SUCCESS;
   }

   /* item should still be on the timer list; Otherwise, aesop asked us to
    * cancel an invalid op! */
   assert (onlist == &timer_oplist);

   /* place the entry on the cancelled list */

   top = ae_op_entry(op, struct timer_op, op);
   top->ret = -ECANCELED;

   /* NOTE: don't bother modifying the posix timer when we cancel
    * something.  In the worst case it will fire one time more than is
    * strictly necessary and reset itself.
    */

   /* ae_ops_exists does a linear search... */
   /* assert(ae_ops_exists(&timer_oplist, &op->link)); */

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

int aesop_timer_init(void)
{
   int ret;

   triton_mutex_lock(&module_lock);

   if(!module_refcount)
   {
       ev_init(&timer_watcher, timer_cb);

       ae_ops_init(&timer_oplist);
       ae_ops_init(&cancel_oplist);

       ret = AE_OPCACHE_INIT(struct timer_op, op, TIMER_DEFAULT_SIZE, &timer_opcache);
       if(ret != 0)
       {
          triton_mutex_unlock(&module_lock);
          return AE_ERR_SYSTEM;
       }

       ret = ae_resource_register(&aesop_timer_resource, 
          &aesop_timer_resource_id);
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

void aesop_timer_finalize(void)
{
   triton_mutex_lock(&module_lock);
   module_refcount--;

   if(!module_refcount)
   {
      ae_resource_unregister(aesop_timer_resource_id);
      ae_opcache_destroy(timer_opcache);
   }
   triton_mutex_unlock(&module_lock);
}

static void timer_cb(EV_P_ ev_timer *w, int revents)
{
   struct ae_op *gop;
   struct timer_op *top;
   struct timeval now;
   struct timeval diff;
   ev_tstamp tstamp;
   int cancelled;
   int need_poll = 0;
   ae_context_t ctx;

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

      /* check that this op wasn't isn't being cancelled already */
      cancelled = ae_op_complete (gop);

      /*
       * We could complete the op here no matter if it is already marked for
       * cancellation or not; The problem would be that, if we complete here and
       * release the op back to the cache, aesop will still call cancel for
       * this op, meaning the cancel function would no longer be able to
       * quickly dereference the op to see if it is active or not.
       *
       * So, if we know it is going to be cancelled (and aesop is going to
       * call the cancel function for this op), we simply place it on the
       * cancelled list here and request a poll.
       */

      if (!cancelled)
      {
         ctx = gop->ctx;
         ae_opcache_complete_op(timer_opcache, gop, int, 0);
      }
      else
      {
         need_poll = 1;
         ctx = gop->ctx;
         ae_ops_enqueue (gop, &cancel_oplist);
         top->ret = -ECANCELED;
      }

      /* setup for next iteration */

      triton_mutex_lock (&timer_mutex);

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


   /*
    * Don't bother requesting a poll if it already ran and the cancel list is
    * now empty.
    */
   if (need_poll && ae_ops_empty (&cancel_oplist))
   {
      need_poll = 0;
   }

   triton_mutex_unlock(&timer_mutex);

   if (need_poll)
   {
      ae_resource_request_poll(ctx, aesop_timer_resource_id);
   }

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
