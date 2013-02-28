/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

/* This needs to be the full path for make -- if relative,
 * make will try to build ./resourcebuilder.h and fail.
 */
#include "resources/sem/sem.h"
#include "resource.h"

static int sem_resource_id;
static triton_mutex_t sem_module_lock = TRITON_MUTEX_INITIALIZER;
static unsigned int   sem_module_refcount = 0;

static triton_mutex_t sem_cancel_lock = TRITON_MUTEX_INITIALIZER;
static ae_ops_t       sem_cancel_queue;

/**
 * When a caller is blocked on the semaphore, it's context is stored in the op
 * and enqueued on the ae_sem_t structure using the link ability of the op
 * structure.
 */
typedef struct
{
   ae_op_t        op;
   aesop_sem_t *  sem;  /* needed for cancel to find the sem */
} sem_wait_t;

/**
 * Initialize semaphore; Set the initial value
 */
int aesop_sem_init (aesop_sem_t * sem, unsigned int value)
{
   assert (sem_module_refcount);

   triton_mutex_init (&sem->lock, 0);
   sem->value = value;
   ae_ops_init (&sem->wait_queue);
   return AE_SUCCESS;
}

/**
 * Destroy semaphore.
 * There should be no callers sleeping on the semaphore.
 */
int aesop_sem_destroy (aesop_sem_t * sem)
{
   assert (sem_module_refcount);

   /* no need to lock: it is unsafe/not allowed to destroy a semaphore that is
    * being waited on or is being use in another call.
    */
   assert (ae_ops_empty (&sem->wait_queue));

   return AE_SUCCESS;
}


int aesop_sem_get (aesop_sem_t * sem, unsigned int * value)
{
   assert (sem_module_refcount);

   triton_mutex_lock (&sem->lock);
   *value = sem->value;
   triton_mutex_unlock (&sem->lock);

   return AE_SUCCESS;
}


int aesop_sem_set (aesop_sem_t * sem, unsigned int value)
{
   int ret = AE_SUCCESS;

   assert (sem_module_refcount);

   triton_mutex_lock (&sem->lock);
   if (!ae_ops_empty (&sem->wait_queue))
   {
      assert (0 && "aesop_sem_set in the presence of waiters!");
      ret = AE_ERR_EXIST;
   }
   else
   {
      ret = AE_SUCCESS;
      sem->value = value;
   }
   triton_mutex_unlock (&sem->lock);

   return ret;
}

int aesop_sem_up (aesop_sem_t * sem)
{
   assert (sem_module_refcount);

   ae_op_t * release = 0;

   triton_mutex_lock (&sem->lock);
   if (ae_ops_empty (&sem->wait_queue))
   {
      ++sem->value;
   }
   else
   {
      ae_op_t * iter = 0;
      release = 0;

      while (!ae_ops_empty (&sem->wait_queue))
      {
         iter = ae_ops_dequeue (&sem->wait_queue);
         /**
          * If the op was marked for cancellation, do nothing.
          * The cancel function will shortly be called for this opid,
          * which will call the callback and free the op structure.
          */
         if (ae_op_complete (iter))
         {
            release = iter;
            break;
         }
      }

      if (!release)
      {
         /* Turns out all of our waiters have been cancelled.
          * Increment the semaphore instead.
          */
         ++sem->value;
      }
   }
   triton_mutex_unlock (&sem->lock);

   if (release)
   {
      ae_op_execute (release, int, AE_SUCCESS);
      free (release);
   }

   return AE_SUCCESS;
}


ae_define_post(int, aesop_sem_down, aesop_sem_t * sem)
{
   /** check if the ctl is marked for cancellation */
   if (ae_resource_is_cancelled ())
   {
      *__ae_retval = AE_ERR_CANCELLED;
      return AE_IMMEDIATE_COMPLETION;
   }

   triton_mutex_lock (&sem->lock);

   /* check if we need to block or not */
   if (sem->value)
   {
      --sem->value;
      triton_mutex_unlock (&sem->lock);
      *__ae_retval = AE_SUCCESS;
      return AE_IMMEDIATE_COMPLETION;
   }


   /* Ok, we need to wait... */
   sem_wait_t * newwait = malloc (sizeof (sem_wait_t));
   newwait->sem = sem;
   ae_op_fill (&newwait->op);
   ae_ops_link_init (&newwait->op);
   *__ae_op_id = ae_id_gen (sem_resource_id, (uintptr_t) newwait);
   ae_ops_enqueue (&newwait->op, &sem->wait_queue);

   triton_mutex_unlock (&sem->lock);

   return AE_SUCCESS;
}

#define find_base_t(_ptr, _type, _member) \
    ((_ptr != NULL) ?                     \
        ((_type *)((char *)(_ptr) - (unsigned long)((&((_type *)0)->_member)))) : NULL)

static int sem_cancel (ae_context_t ctx, ae_op_id_t op_id)
{
   int resource_id;

   sem_wait_t * wait = (sem_wait_t *)
      intptr2op (ae_id_lookup (op_id, &resource_id));

   /* consistency checks */
   assert (resource_id == sem_resource_id);
   assert (wait);

   // We lock the sem before checking the op list member, as somebody could be
   // removing the entry from the list. Note that op cannot go away,
   // as before calling sem_cancel aesop already completed the op, meaning
   // ae_op_complete will return false, causing sem_up () not to remove this
   // entry any longer.
   aesop_sem_t * sem = wait->sem;

   triton_mutex_lock (&sem->lock);

   // Some sanity checking
   // Find the queue this sem_wait_t is in
   ae_ops_t * queue = ae_ops_get_queue (&wait->op);

   /*
    * There is a chance the somebody calling sem_up on this sem just removed
    * has already removed the op from the wait queue.
    */
   if (queue)
   {
      // Find the aesop_sem_t holding the queue
      aesop_sem_t * sem2 = find_base_t (queue, aesop_sem_t, wait_queue);
      assert (sem == sem2);
   }

   /* We know that, before sem_cancel is called, aesop has marked the passed
    * in op_id as being complete/cancelled. This means that this op pointer
    * (and the associated caller blocking in aesop_sem_down)
    * will not be released by a call to aesop_sem_up.
    *
    * This means that we can trust that:
    *   1) op is not going away
    *   2) sem is not going away (this would be a programmer error, since
    *      this would involve freeing a semaphore while there are callers
    *      blocked on it).
    */

   /* Now there are two options:
    *    1) the op is still on the wait queue
    *    2) the op was about to be completed, but after it was marked for
    *    cancellation, and aesop_sem_up removed it from the queue but did not
    *    complete it.
    */

   /* remove op from the wait list */
   if (queue)
   {
      assert (queue == &sem->wait_queue);
      ae_ops_del (&wait->op);
   }

   triton_mutex_unlock (&sem->lock);

   /* schedule callback to be called outside of the cancel function... */
   triton_mutex_lock (&sem_cancel_lock);
   ae_ops_enqueue (&wait->op, &sem_cancel_queue);
   triton_mutex_unlock (&sem_cancel_lock);

   ae_resource_request_poll (ctx, sem_resource_id);


   return AE_SUCCESS;
}

static int sem_poll (ae_context_t ctx, void * arg)
{
   triton_mutex_lock (&sem_cancel_lock);
   while (!ae_ops_empty (&sem_cancel_queue))
   {
      ae_op_t * iter = ae_ops_dequeue (&sem_cancel_queue);

      // Find begin of structure counting back from the op member
      sem_wait_t * wait = find_base_t (iter, sem_wait_t, op);
      triton_mutex_unlock (&sem_cancel_lock);

      // Complete op indicating cancellation
      ae_op_execute (&wait->op, int, AE_ERR_CANCELLED);

      triton_mutex_lock (&sem_cancel_lock);
      free (wait);
   }
   triton_mutex_unlock (&sem_cancel_lock);

   return AE_SUCCESS;
}


static struct ae_resource sem_resource =
{
   .resource_name = "aesop_sem",
   .cancel = sem_cancel,
   .poll_context = sem_poll,
};



int aesop_sem_module_init (void)
{
   int ret = AE_SUCCESS;

   triton_mutex_lock (&sem_module_lock);

   if (!sem_module_refcount)
   {
      ret = ae_resource_register (&sem_resource, &sem_resource_id);

      if (ret == AE_SUCCESS)
      {
         ae_ops_init (&sem_cancel_queue);
      }
   }

   ++sem_module_refcount;

   triton_mutex_unlock (&sem_module_lock);
   return ret;
}

void aesop_sem_module_finalize (void)
{
   triton_mutex_lock (&sem_module_lock);

   --sem_module_refcount;

   if (!sem_module_refcount)
   {
      triton_mutex_lock (&sem_cancel_lock);
      assert (ae_ops_empty (&sem_cancel_queue));
      ae_ops_destroy (&sem_cancel_queue);
      triton_mutex_unlock (&sem_cancel_lock);

      ae_resource_unregister (sem_resource_id);
   }

   triton_mutex_unlock (&sem_module_lock);
}














