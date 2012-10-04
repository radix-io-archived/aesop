/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#include "libev/ev.h"
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
    int (*poll_context)(ae_context_t context, void *user_data);
    ae_context_t context;
    void *user_data;
};

/* data structures and tables used to track information for
 * resources that are available to be initialized
 */
struct ae_resource_init_data
{
    char* name;
    int (*init)(void);
    void (*finalize)(void);
    unsigned int count;
};
#define MAX_RESOURCES 32
static struct ae_resource_init_data ae_resource_init_table[MAX_RESOURCES];
static int ae_resource_init_table_count = 0;

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
static struct ae_resource_entry ae_resource_entries[AE_MAX_RESOURCES]; 
static struct ev_loop *eloop = NULL;
ev_async eloop_breaker;
static pthread_t ev_loop_thread;

#define AE_RESOURCE_IDX2ID(reindex) (reindex+16)
#define AE_RESOURCE_ID2IDX(rid) (rid-16)

struct ae_context
{
    int id;
    int resource_count;
    int* resource_ids;
    struct ae_poll_data* poll_data;
    struct ev_loop *eloop;
    ev_async eloop_breaker;
};

static int ae_context_count = 0;
static struct ae_context ae_context_entries[AE_MAX_CONTEXTS];


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

    assert(poll_data->poll_context);
    ret = poll_data->poll_context(poll_data->context, poll_data->user_data);
    /* TODO: error handling? */
    aesop_error_assert(ret);
    
    /* Break the loop.  Note that ev_run() will still process all events
     * that are pending at the time we call this, which is a good thing.
     */
    ev_break(EV_A_ EVBREAK_ONE);
    return;
}

int ae_resource_init_register(const char* resource_name, 
    int (*init)(void),
    void (*finalize)(void))
{
    int i;

    /* see if we have already registered this resource */
    for(i=0; i<ae_resource_init_table_count; i++)
    {
        if(strcmp(ae_resource_init_table[i].name, resource_name) == 0)
        {
            return(AE_SUCCESS);
        }
    }

    if(ae_resource_init_table_count > MAX_RESOURCES)
    {
        return(AE_ERR_OVERFLOW);
    }
    ae_resource_init_table[ae_resource_init_table_count].name = 
        strdup(resource_name);
    if(!ae_resource_init_table[ae_resource_init_table_count].name)
        return(AE_ERR_SYSTEM);

    ae_resource_init_table[ae_resource_init_table_count].init = init;
    ae_resource_init_table[ae_resource_init_table_count].finalize = finalize;
    ae_resource_init_table[ae_resource_init_table_count].finalize = finalize;
    ae_resource_init_table_count++;

    return(AE_SUCCESS);
}


static int ae_resource_init_register_cleanup (void)
{
   int i;

   for(i=0; i<ae_resource_init_table_count; i++)
   {
      free (ae_resource_init_table[i].name);
      ae_resource_init_table[i].name = 0;
   }
   ae_resource_init_table_count = 0;

   return AE_SUCCESS;
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

    ev_loop_thread = pthread_self();
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

    if(ae_resource_count == AE_MAX_RESOURCES)
    {
	return AE_ERR_INVALID;
    }

    /* find a free entry */
    for(i=0; i<AE_MAX_RESOURCES; i++)
    {
        if(ae_resource_entries[i].id == -1)
        {
            reindex = i;
            break;
        }
    }
    assert(reindex > -1 && reindex < MAX_RESOURCES);

    ev_async_init(&ae_resource_entries[reindex].poll_data.async, ev_async_cb);
    ae_resource_entries[reindex].poll_data.poll_context =
        resource->poll_context;
    ae_resource_entries[reindex].poll_data.context = NULL;
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

    /* TODO: what about contexts that might include this resource?  Do we
     * just assume that contexts are always closed before resources are
     * unregistered?
     */
    ev_async_stop(eloop, &ae_resource_entries[idx].poll_data.async);

    memset(&ae_resource_entries[idx], 0, sizeof(ae_resource_entries[idx]));
    ae_resource_entries[idx].id = -1;

    ae_resource_count--;
}

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
        aesop_err("Error: context %p is not configured to handle resource with id %d", context, resource_id);
        for(i=0; i<MAX_RESOURCES; i++)
        {
            if(ae_resource_entries[i].id == resource_id)
            {
                aesop_err("Consider adding the \"%s\" resource your aesop context.", ae_resource_entries[i].resource->resource_name);
                assert(0);
            }
        }
        aesop_err("Error: resource_id %d is unknown to aesop.  Are you using a resource that was not initialized?\n", resource_id);
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

    if(pthread_equal(ev_loop_thread, pthread_self()))
    {
        /* this was called from the event loop thread, so we know that it is
         * already awake.  Just make sure that it exits if the
         * ae_poll_break() as called from a libev callback.
         */
        ev_break(target_loop, EVBREAK_ONE);
        return;
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

int ae_poll(ae_context_t context, int millisecs)
{
    struct ev_loop* target_loop;
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
        return(AE_ERR_TIMEDOUT);
    }

    return(AE_SUCCESS);
}

#include <stdarg.h>

int _ae_context_create(ae_context_t *context, const char *format __attribute__((unused)), int resource_count, ...)
{
    va_list ap;
    char *rname;
    int cindex = ae_context_count;
    ae_context_t c;
    int i, j, reindex;
    int ret;

    if(ae_context_count == AE_MAX_CONTEXTS)
    {
        return AE_ERR_INVALID;
    }

    c = &(ae_context_entries[cindex]);
    c->id = ae_context_count;
    ae_context_count++;
    c->resource_count = resource_count;
    c->resource_ids = malloc(sizeof(*c->resource_ids) * resource_count);
    if(!c->resource_ids)
    {
        return AE_ERR_SYSTEM;
    }
    c->poll_data = malloc(sizeof(*c->poll_data) * resource_count);
    if(!c->poll_data)
    {
        free(c->resource_ids);
        return AE_ERR_SYSTEM;
    }

    c->eloop = ev_loop_new(EVFLAG_AUTO);
    if(!c->eloop)
    {
        aesop_err("Error: could not create libev event loop.\n");
        free(c->resource_ids);
        free(c->poll_data);
        return(AE_ERR_SYSTEM);
    }
    ev_async_init(&c->eloop_breaker, ev_break_cb);
    ev_async_start(c->eloop, &c->eloop_breaker);

    reindex = 0;

    /* step through the resource names passed in and register the context with them */
    va_start(ap, resource_count);
    for(i = 0; i < resource_count; ++i)
    {
        int resource_found = 0;
        rname = va_arg(ap, char *);

        /* find the matching resource and register the context with that resource */

        for(j = 0; j < MAX_RESOURCES; ++j)
        {
            if(!strcmp(ae_resource_entries[j].resource->resource_name, rname))
            {
                ev_async_init(&c->poll_data[reindex].async, ev_async_cb);
                c->poll_data[reindex].poll_context = 
                    ae_resource_entries[j].poll_data.poll_context;
                c->poll_data[reindex].context = c;
                c->poll_data[reindex].user_data = 
                    ae_resource_entries[j].poll_data.user_data;

                if(ae_resource_entries[j].resource->register_context)
                {
                    ret = ae_resource_entries[j].resource->register_context(c);
                    if(ret != AE_SUCCESS)
                    {
                        /* what do we do if a context fails to register with a resource? */
                        va_end(ap);
                        return ret;
                    }
                }
                ev_async_start(c->eloop, &c->poll_data[reindex].async);

                c->resource_ids[reindex] = ae_resource_entries[j].id;

                reindex++;
                resource_found = 1;
                break;
            }
        }
        if(!resource_found)
        {
            va_end(ap);
            return AE_ERR_NOT_FOUND;
        }
    }
    va_end(ap);

    *context = c;
    return AE_SUCCESS;
}

int ae_context_destroy(ae_context_t context)
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
        ev_async_stop(context->eloop, &context->poll_data[i].async);
    }
    free(context->resource_ids);
    free(context->poll_data);
    context->resource_ids = NULL;
    context->poll_data = NULL;
    context->id = -1;
    context->resource_count = -1;
    ev_loop_destroy(context->eloop);
    return AE_SUCCESS;
}


/**
 * Given an op_id and a context, try to call the resource to cancel
 * the operation if it was generated by a resource.
 * Otherwise, do nothing.
 */
static int ae_cancel_resource_op (ae_context_t context, ae_op_id_t op)
{
   int resource_id;

   intptr_t data = ae_id_lookup (op, &resource_id);
   data;

   if (!resource_id)
      return 0;

   int ridx = AE_RESOURCE_ID2IDX(resource_id);

   assert(ridx > -1 && ridx < MAX_RESOURCES);

   assert (ae_resource_entries[ridx].id == resource_id);

   if(ae_resource_entries[ridx].resource->cancel)
   {
      return ae_resource_entries[ridx].resource->cancel(context, op);
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
   int finalret = AE_SUCCESS;

   triton_mutex_lock (&ctl->mutex);

   ctl->op_state != OP_REQUEST_CANCEL;

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

   triton_list_for_each (entry, safe, &ctl->children)
   {
      struct ae_ctl * child_ctl = triton_list_get_entry(entry,
            struct ae_ctl, link);

      int ret = ae_cancel_ctl (child_ctl);

      if (ret != AE_SUCCESS)
         finalret = ret;

      // Request a cancellation of each resource function
      // if it didn't complete yet  / wasn't cancelled
      if (!(child_ctl->op_state & OP_COMPLETED))
      {
         ret = ae_cancel_resource_op (child_ctl->context,
               child_ctl->current_op_id);
         if (ret == AE_SUCCESS)
         {
            /* only mark as cancelled if there was no problem with the
             * cancellation; if there was, we return the error and leave it up
             * to the programmer to try again by calling cancel_branches
             * again.
             */
            child_ctl->op_state &= OP_COMPLETED_CANCELLED;
         }
      }

      if (ret != AE_SUCCESS)
         finalret = ret;
   }
   triton_mutex_unlock (&ctl->mutex);
   return finalret;
}

/**
 * This function is called with the ctl structure of parent of the pwait.
 */
int ae_cancel_branches(struct ae_ctl *ctl)
{

    int ret;
    ae_op_id_t *children_ids;
    int ind, count, error;
    ae_context_t context;

    ae_debug_cancel("ae_cancel_branches: %p\n", ctl);

    if(!ctl)
    {
        fprintf(stderr, "can't call ae_cancel_branches from outside of a pbranch context\n");
        assert(ctl);
    }

    return ae_cancel_ctl (ctl);
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

struct ev_loop * ae_resource_get_eloop(ae_context_t context)
{
    if(!context)
        return(eloop);
    else
        return(context->eloop);
}

int aesop_set_config(const char* key, const char* value)
{
    int i;
    int ret;
    struct ae_resource_config* config;

    for(i=0; i<MAX_RESOURCES; i++)
    {
        if(ae_resource_entries[i].id == -1)
            continue;

        config = ae_resource_entries[i].resource->config_array; 
        while(config != NULL && config->name != NULL)
        {
            if(strcmp(config->name, key) == 0)
            {
                ret = config->updater(key, value);
                return(ret);
            }
            config++;
        }
    }

    return(AE_ERR_NOT_FOUND);
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
    for(i=0; i<MAX_RESOURCES; i++)
    {
        if(ae_resource_entries[i].id == -1)
            continue;

        if(!strcmp(ae_resource_entries[i].resource->resource_name, resource))
        {
            ae_resource_entries[i].debug = value;
            return(0);
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

static int ae_resource_init_helper (int i)
{
   int ret = AE_SUCCESS;

   if (!ae_resource_init_table[i].count)
   {
      ret = ae_resource_init_table[i].init();
      if (ret != AE_SUCCESS)
         return ret;
   }

   ++ae_resource_init_table[i].count;

   return ret;
}

static int ae_resource_finalize_helper (int i)
{
   int ret = AE_SUCCESS;

   assert (ae_resource_init_table[i].count);

   --ae_resource_init_table[i].count;

   if(!ae_resource_init_table[i].count)
   {
      ae_resource_init_table[i].finalize();
   }

   return ret;
}

static int ae_resource_string_helper (const char* resource, int init)
{
    int i;

    for(i=0; i<ae_resource_init_table_count; i++)
    {
        if(!strcmp(resource, ae_resource_init_table[i].name))
        {
           if (init)
              ae_resource_init_helper (i);
           else
              ae_resource_finalize_helper (i);
        }
    }

    return(AE_ERR_NOT_FOUND);
}

static int ae_resource_all_helper (int init)
{
    int i;
    int ret = AE_SUCCESS;

    for(i=0; i<ae_resource_init_table_count; i++)
    {
        ret = (init ? ae_resource_init_helper (i) 
                    : ae_resource_finalize_helper (i));

        if(ret != AE_SUCCESS)
           break;
    }

    return ret;
}

int ae_resource_init (const char * s)
{
   return ae_resource_string_helper (s, 1);
}

int ae_resource_finalize (const char * s)
{
   return ae_resource_string_helper (s, 0);
}

int ae_resource_finalize_all (void)
{
   return ae_resource_all_helper (0);
}

int ae_resource_init_all (void)
{
   return ae_resource_all_helper (1);
}

int ae_resource_finalize_active (void)
{
    int i;
    int ret = AE_SUCCESS;

    for(i=0; i<ae_resource_init_table_count; i++)
    {
       while (ae_resource_init_table[i].count)
       {
          ret = ae_resource_finalize_helper (i);
          if (ret != AE_SUCCESS)
             return ret;
       }
    }

    return ret;
}

int ae_resource_cleanup (void)
{
   ae_resource_init_register_cleanup ();
   ev_async_stop (eloop, &eloop_breaker);
   ev_default_destroy ();

   return(0);
}


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
