/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#include <ev.h>
#include <stddef.h>
#include <unistd.h>
#include <fcntl.h>

#include "resource.h"
#include "aesop.h"
#include "ae-debug.h"
#include "op.h"

struct ae_poll_data
{
    ev_async async;
    int (*poll)(void *user_data);
    void *user_data;
};

/* data structures and tables used to track information for resources that
 * actually have been initialized and are now active
 */
struct ae_resource_entry
{
    int id;
    int debug;
    struct ae_poll_data poll_data;
    struct ae_resource *resource;
};

static int ae_resource_count = 0;
#define MAX_RESOURCES 32
static struct ae_resource_entry ae_resource_entries[MAX_RESOURCES]; 
static struct ev_loop *eloop = NULL;
ev_async eloop_breaker;

#define AE_RESOURCE_IDX2ID(reindex) (reindex+16)
#define AE_RESOURCE_ID2IDX(rid) (rid-16)

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
    int ret;

    struct ae_poll_data* poll_data = 
        (struct ae_poll_data*)(((char*)w)-offsetof(struct ae_poll_data, async));

    assert(poll_data->poll);
    ret = poll_data->poll(poll_data->user_data);
    /* TODO: error handling? */
    aesop_error_assert(ret);
    
    /* Break the loop.  Note that ev_run() will still process all events
     * that are pending at the time we call this, which is a good thing.
     */
    ev_break(EV_A_ EVBREAK_ONE);
    return;
}

int ae_resource_register(struct ae_resource *resource, int *newid)
{
    return ae_resource_register_with_data(resource, newid, NULL);
}

int ae_resource_register_with_data(struct ae_resource *resource, int *newid,
                                   void *user_data)
{
    int reindex = -1;
    int i;

    if(ae_resource_count == 0)
    {
        /* No resources are registered. Make sure that the
         * resource array is in a valid initial configuration.
         */
        for(i=0; i<MAX_RESOURCES; i++)
        {
            memset(&ae_resource_entries[i], 0, sizeof(ae_resource_entries[i]));
            ae_resource_entries[i].id = -1;
        }
    }

    if(eloop == NULL)
    {
        /* This is the first resource to be registered.  Initialize the
         * event loop and start a watcher that can be used to break it's
         * execution
         */
        eloop = ev_default_loop (EVFLAG_AUTO);
        ev_async_init(&eloop_breaker, ev_break_cb);
        ev_async_start(eloop, &eloop_breaker);
    }

    if(ae_resource_count == MAX_RESOURCES)
    {
	return AE_ERR_INVALID;
    }

    /* find a free entry */
    for(i=0; i<MAX_RESOURCES; i++)
    {
        if(ae_resource_entries[i].id == -1)
        {
            reindex = i;
            break;
        }
    }
    assert(reindex > -1 && reindex < MAX_RESOURCES);

    ev_async_init(&ae_resource_entries[reindex].poll_data.async, ev_async_cb);
    ae_resource_entries[reindex].poll_data.poll = resource->poll;
    ae_resource_entries[reindex].poll_data.user_data = user_data;
    ev_async_start(eloop, &ae_resource_entries[reindex].poll_data.async);

    ae_resource_entries[reindex].id = AE_RESOURCE_IDX2ID(reindex);
    ae_resource_entries[reindex].resource = resource;
    ae_resource_entries[reindex].debug = 0;
    ae_resource_count++;
    *newid = AE_RESOURCE_IDX2ID(reindex);
    return AE_SUCCESS;
}

void ae_resource_unregister(int id)
{
    int idx = AE_RESOURCE_ID2IDX(id);

    ev_async_stop(eloop, &ae_resource_entries[idx].poll_data.async);

    memset(&ae_resource_entries[idx], 0, sizeof(ae_resource_entries[idx]));
    ae_resource_entries[idx].id = -1;

    ae_resource_count--;
}

static void find_async_watcher(int resource_id, ev_async** async_out)
{
    ev_async* async = NULL;
    int ridx;
   
    /* find this resource in the global list */
    ridx = AE_RESOURCE_ID2IDX(resource_id);
    async = &ae_resource_entries[ridx].poll_data.async;

    if(!async)
    {
        aesop_err("Error: resource_id %d is unknown to aesop.  Are you using"
                " a resource that was not initialized?\n", resource_id);
        assert(0);
    }

    *async_out = async;

    return;
}

/**
 * ae_poll_break() can be used to interrupt a currently executing ae_poll
 * call.  This would typically be used in the linkage between c and aesop
 * functions to allow the c program to continue execution after the final
 * aesop callback has completed.
 */
void ae_poll_break()
{
    // Note: it is safe to call ev_break from outside of ev_run
    // (in which case it has no effect), so we don't care if this is
    // called from within the main event loop thread or from some other
    // thread.
    //
    // This might (but probably not, as ev_break will still process all
    // pending events before breaking) cause the next ev_run call to return
    // early, but that should be OK as the event loop will typically call
    // ev_run forever.
    ev_async_send(eloop, &eloop_breaker);
    ev_break(eloop, EVBREAK_ONE);
}

/**
 * ae_resource_request_poll() is used by a resource to inform aesop that the
 * resource needs to be polled.
 */
void ae_resource_request_poll(int resource_id)
{
    ev_async* async = NULL;

    find_async_watcher(resource_id, &async);

    ev_async_send(eloop, async);

    return;
}

ae_op_id_t ae_id_gen(int resource_id, intptr_t ptr)
{
    ae_op_id_t newid;
    newid.u = resource_id;
    newid.l = ptr;
    return newid;
}

/**
 * Returns the resouce ID of the op in *resource_id, and the user data as the
 * return of the function.
 */
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

static void timeout_cb(EV_P_ ev_timer *w, int revents)
{
    int* hit_timeout = (int*)w->data;
    *hit_timeout = 1;
    /* break the loop */
    ev_break(EV_A_ EVBREAK_ONE);
}

int ae_poll(int millisecs)
{
    ev_timer timeout;
    int hit_timeout = 0;

    if(millisecs > 0)
    {
        timeout.data = &hit_timeout;
        ev_timer_init(&timeout, timeout_cb, (double)millisecs * 1000.0, 0);
        ev_timer_start(eloop, &timeout);
        ev_run(eloop, 0);
    }
    else
    {
        ev_run(eloop, EVRUN_NOWAIT);
    }

    if(millisecs > 0)
        ev_timer_stop(eloop, &timeout);

    /* this means that the event loop timed out without finding any work */
    if(hit_timeout == 1)
    {
        return(AE_ERR_TIMEDOUT);
    }

    return(AE_SUCCESS);
}

#include <stdarg.h>

/**
 * Given an op_id, try to call the resource to cancel
 * the operation if it was generated by a resource.
 * Otherwise, do nothing.
 */
static int ae_cancel_resource_op (ae_op_id_t op)
{
   int resource_id;

   intptr_t data = ae_id_lookup (op, &resource_id);
   
   ae_debug_cancel(" ae_cancel_resource_op: %lu,%i\n", (long unsigned) data,
         resource_id);

   /** if !resource_id, then this is not a resource op */
   assert (resource_id);

   if (!resource_id)
   {
      return AE_ERR_INVALID;
   }

   int ridx = AE_RESOURCE_ID2IDX(resource_id);

   assert(ridx > -1 && ridx < MAX_RESOURCES);

   assert (ae_resource_entries[ridx].id == resource_id);

   if(ae_resource_entries[ridx].resource->cancel)
   {
      return ae_resource_entries[ridx].resource->cancel(op);
   }
   else
   {
      /**
       * If the resource has no cancel function, we have to assume the
       * operation can wait indefinitely. Return error.
       */
      return AE_ERR_INVALID;
   }
}

/**
 * Set the cancel flag for this ae_ctl and all of its child ae_ctl,
 * and tries to cancel each active resource operation along the way.
 *
 * Returns AE_SUCCESS if all ongoing operations were signalled to cancel,
 * and an AE_ERR_ code if one of the operations could not be cancelled.
 */
int ae_cancel_ctl (struct ae_ctl * ctl);

int ae_cancel_ctl (struct ae_ctl * ctl)
{
   int finalret;
    
   triton_mutex_lock (&ctl->mutex);

   ae_debug_cancel("ae_cancel_ctl: %p\n (%s)", ctl, ctl->name);

   ctl->op_state |= OP_REQUEST_CANCEL;

   /* since this ctl is locked, we know that no pbranch can start/stop while
    * we have the lock (and thus no children created).
    *
    * Also, since we locked the ctl, no new operation can be posted.
    *
    * We set the cancel flag and cancel and send a request to cancel any
    * ongoing operations.
    */
   struct triton_list_link * entry;
   struct triton_list_link * safe;

   finalret = AE_SUCCESS;

   /* First do all the children */
   triton_list_for_each (entry, safe, &ctl->children)
   {
      struct ae_ctl * child_ctl = triton_list_get_entry(entry,
            struct ae_ctl, link);

      int ret = ae_cancel_ctl (child_ctl);

      if (ret != AE_SUCCESS)
         finalret = ret;

   }


   /* Check if we have something going on */
   int resource_id;
   intptr_t data = ae_id_lookup (ctl->current_op_id,
         &resource_id);

   if (!resource_id)
   {
      int ret = AE_SUCCESS;
      /* We're in a context that called another blocking
       * function or is currently calling a non-blocking function;
       * Recurse if there is another context */
      if (data)
      {
         ret = ae_cancel_ctl ((struct ae_ctl *) data);
      }

      if (ret != AE_SUCCESS)
         finalret = ret;
   }
   else
   {
      /* we actually called a true resource function from this context */
      // Request a cancellation of each resource function
      // if it didn't complete yet  / wasn't cancelled
      if (!(ctl->op_state & OP_COMPLETED))
      {
         int ret = AE_SUCCESS;

         assert (resource_id);
         /* We're in a context that actually called a resource function */
         ret = ae_cancel_resource_op (ctl->current_op_id);


         if (ret == AE_SUCCESS)
         {
            /* only mark as cancelled if there was no problem with the
             * cancellation; if there was, we return the error and leave it up
             * to the programmer to try again by calling cancel_branches
             * again.
             */
            ctl->op_state &= OP_COMPLETED_CANCELLED;
         }

         if (ret != AE_SUCCESS)
            finalret = ret;
      }
   }

   triton_mutex_unlock (&ctl->mutex);
   return finalret;
}

/**
 * This function is called with the ctl structure of parent of the pwait.
 */
int ae_cancel_branches(struct ae_ctl *ctl)
{

    //int ret;
    //ae_op_id_t *children_ids;
    //int ind, count, error;

    ae_debug_cancel("ae_cancel_branches: %p\n", ctl);

    if(!ctl || !ctl->in_pwait)
    {
        fprintf(stderr, "can't call ae_cancel_branches from outside of a pbranch context\n");
        assert(ctl);
    }

    return ae_cancel_ctl (ctl);
}

int ae_count_branches(struct ae_ctl *ctl)
{
    int r, error;
    if(!ctl || !ctl->in_pwait)
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

void ae_get_stack(struct ae_ctl *ctl, char * *stack, int *inout_count)
{
    int i = 0;
    while(ctl && i < *inout_count)
    {
        stack[i++] = strdup (ctl->name);
        ctl = ctl->parent;
    }

    *inout_count = i;
}

void ae_print_stack(FILE *outstream, struct ae_ctl *ctl)
{
    int i = 0;
    char *stack[512];
    int count = sizeof (stack)/sizeof (stack[0]);

    ae_get_stack (ctl, &stack[0], &count);

    for(i = count-1; i >= 0; --i)
    {
        fprintf(outstream, "[%d]: %s\n", i, stack[i]);
    }

    for(i = 0; i < count; ++i)
    {
        free(stack[i]);
    }
}

int aesop_dbg_blocking = 0;
int aesop_dbg_cancel = 0;
int aesop_dbg_pbranch = 0;


int aesop_debug_from_env (void)
{
   const char * SPLIT = ",";
   const char * d = getenv ("AESOP_DEBUG");
   if (!d)
   {
      return AE_SUCCESS;
   }

   char * token = strdup (d);

   char * saveptr;
   const char * current = strtok_r (token, SPLIT, &saveptr);

   while (current)
   {
      aesop_set_debugging (current, 1);
      current = strtok_r (0, SPLIT, &saveptr);
   }

   free (token);

   return AE_SUCCESS;
}

int aesop_set_debugging(const char* resource, int value)
{
    int i;

    if(value != 0 && value != 1)
    {
        /* on or off are the only possible options */
        return(AE_ERR_INVALID);
    }

    /* special cases; these aren't resources.  These are just different
     * aspects of the aesop internals
     */
    if(!strcmp(resource, "ae_blocking"))
    {
        aesop_dbg_blocking = value;
        return(0);
    }
    else if (!strcmp(resource, "ae_cancel"))
    {
        aesop_dbg_cancel = value;
        return(0);
    }
    else if (!strcmp(resource, "ae_pbranch"))
    {
        aesop_dbg_pbranch = value;
    }

    /* check for matching resources and set their debugging value */
    if (ae_resource_count)
    {
       for(i=0; i<MAX_RESOURCES; i++)
       {
          if(ae_resource_entries[i].id == -1 || !ae_resource_entries[i].resource)
             continue;

          if(!strcmp(ae_resource_entries[i].resource->resource_name, resource))
          {
             ae_resource_entries[i].debug = value;
             return(0);
          }
       }
    }

    return(AE_ERR_NOT_FOUND);
}

int ae_check_debug_flag(int resource_id)
{
    int idx = AE_RESOURCE_ID2IDX(resource_id);

    /* return value of debug flag for this resource */
    return(ae_resource_entries[idx].debug);
}

int ae_resource_cleanup (void)
{
   ev_async_stop (eloop, &eloop_breaker);
   ev_default_destroy ();

   return(0);
}


/* Returns 0 if a cancel() operation is already in progress, returns 1 if no
 * cancel() operation is in progress.  In the latter case the caller is
 * expected to complete the operation normally.
 */
int ae_op_complete (struct ae_op * op)
{
   int ret = 0;
   struct ae_ctl * ctl = op->user_ptr;

   triton_mutex_lock (&ctl->mutex);

   if (!(ctl->op_state & OP_COMPLETED))
   {
      ctl->op_state |= OP_COMPLETED_NORMAL;
      ret = 1;
   }

   triton_mutex_unlock (&ctl->mutex);

   return ret;
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
