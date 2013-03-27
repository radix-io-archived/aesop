#include "worker.h"
#include <triton-list.h>
#include <triton-thread.h>
#include "resource.h"

struct aesop_worker_t
{
   triton_list_link_t        link;
   aesop_worker_function_t   function;
   void *                    data;
};

static triton_mutex_t worker_lock = TRITON_MUTEX_INITIALIZER;
static triton_list_t  worker_list;
static unsigned int   worker_refcount = 0;
static int            worker_resource_id;


aesop_worker_t * aesop_worker_register (aesop_worker_function_t func,
      void * data)
{
   assert (worker_refcount);

   aesop_worker_t * n = malloc (sizeof (aesop_worker_t));
   n->function = func;
   n->data = data;
   triton_list_link_clear (&n->link);
   return n;
}

int aesop_worker_unregister (aesop_worker_t * worker)
{
   assert (worker_refcount);

   assert (!triton_list_is_linked (&worker->link));
   free (worker);
   return AE_SUCCESS;
}

int aesop_worker_schedule (aesop_worker_t * worker)
{
   assert (worker_refcount);

   int was_empty = 0;

   triton_mutex_lock (&worker_lock);

   if (!triton_list_is_linked (&worker->link))
   {
      was_empty = triton_list_empty (&worker_list);
      triton_list_add_back (&worker->link, &worker_list);
   }

   triton_mutex_unlock (&worker_lock);

   if (was_empty)
   {
      /*
       * We don't have access to the context since this is not a blocking
       * function.
       */
      ae_resource_request_poll (worker_resource_id);
   }

   return AE_SUCCESS;
}


static int aesop_worker_poll (void * arg)
{
   assert (worker_refcount);

   /*
    * This is safe since the poll function is the only one removing things
    * from the list, so we know our iterator is stable.
    */
   
   triton_list_link_t * cur;
   triton_list_link_t * tmp;

   triton_mutex_lock (&worker_lock);

   while (!triton_list_empty (&worker_list))
   {
      triton_list_for_each (cur, tmp, &worker_list)
      {
         triton_list_del (cur);

         /* unlock while calling function */
         triton_mutex_unlock (&worker_lock);

         aesop_worker_t * worker = triton_list_get_entry (cur, aesop_worker_t,
               link);
         worker->function (worker->data);

         triton_mutex_lock (&worker_lock);
      }
   }

   triton_mutex_unlock (&worker_lock);

   return AE_SUCCESS;
}

static struct ae_resource worker_resource =
{
    .resource_name = "worker",
    .poll = aesop_worker_poll
};


/**
 * Note: no auto registration any longer.
 * Users of the API have to call init and finalize manually.
 */

int aesop_worker_init (void)
{
   triton_mutex_lock (&worker_lock);
   if (!worker_refcount)
   {
      ae_resource_register (&worker_resource, &worker_resource_id);
      triton_list_init (&worker_list);
   }
   ++worker_refcount;
   triton_mutex_unlock (&worker_lock);
   return AE_SUCCESS;
}

int aesop_worker_finalize (void)
{
   triton_mutex_lock (&worker_lock);

   assert (triton_list_empty (&worker_list));
   assert (worker_refcount);

   --worker_refcount;

   if (!worker_refcount)
   {
      ae_resource_unregister (worker_resource_id);
   }

   triton_mutex_unlock (&worker_lock);
   return AE_SUCCESS;
}

