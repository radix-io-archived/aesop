/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

/* This needs to be the full path for make -- if relative,
 * make will try to build ./resourcebuilder.h and fail.
 */
#include "resources/resourcebuilder/resourcebuilder.h"
#include "src/c-utils/triton-thread.h"
#include "resource.h"

#define RB_DEFAULT_SIZE 64
#define RB_RESOURCE_NAME "resourcebuilder"

static int rb_resource_id;

enum {
       STATUS_UNINITIALIZED = 0,
       STATUS_EMPTY,
       STATUS_ARMED,
       STATUS_COMPLETED_SUCCESS,
       STATUS_COMPLETED_CANCEL
     };


void rb_slot_initialize (rb_slot_t * slot)
{
   OPA_store_int (&slot->status, STATUS_EMPTY);
#ifdef RB_NO_OPA
   pthread_mutex_init (&slot->lock, 0);
#endif
}

/*
 * returns true if the slot was:
 *   moved from STATUS_UNINITIALIZED to STATUS_EMPTY.
 *   already in STATUS_EMPTY.
 */
int rb_slot_reinitialize (rb_slot_t * slot)
{
   const int newstatus = STATUS_EMPTY;
   int status;

   status = OPA_cas_int (&slot->status, STATUS_UNINITIALIZED, newstatus);
   return ((status == STATUS_UNINITIALIZED) || (status == STATUS_EMPTY));
}

void rb_slot_clear (rb_slot_t *slot)
{
    OPA_store_int (&slot->status, STATUS_UNINITIALIZED);
    return;
}

/**
 * Complete the slot with the given error code.
 * Return AE_SUCCESS if successful, AE_ERR_INVALID if the slot was already
 * completed.
 * Return AE_ERR_NOT_FOUND if the slot was not properly initialized or was
 * already destroyed.
 */
static int rb_callback (rb_slot_t * slot, int success)
{
#ifdef RB_NO_OPA
   const int newstatus = (success ? STATUS_COMPLETED_SUCCESS
         : STATUS_COMPLETED_CANCEL);
   int ret;
   void (*callback)(void *, int) = 0;
   void * user_ptr = 0;

   triton_mutex_lock (&slot->lock);
   switch (slot->status)
   {
      case STATUS_UNINITIALIZED:
         ret = AE_ERR_NOT_FOUND;
         break;
      case STATUS_EMPTY:
         /* slot is not armed yet */
         slot->status = newstatus;
         break;
      case STATUS_ARMED:
         {
            int resource_id;
            struct ae_op * op = &slot->op;

            assert (slot == (rb_slot_t *) intptr2op (ae_id_lookup (slot->op_id,
                        &resource_id)));

            assert (resource_id == rb_resource_id);

            slot->status = newstatus;

            callback = slot->op.callback;
            user_ptr = slot->op.user_ptr;
            ret = AE_SUCCESS;
            break;
         }
      case STATUS_COMPLETED_SUCCESS:
      case STATUS_COMPLETED_CANCEL:
         /* already completed */
         ret = AE_ERR_INVALID;
         break;
   }

   triton_mutex_unlock (&slot->lock);

   if (callback)
      callback (user_ptr, (success ? AE_SUCCESS : AE_ERR_INVALID));

   return ret;

#else
   const int newstatus = (success ? STATUS_COMPLETED_SUCCESS
         : STATUS_COMPLETED_CANCEL);

   while (1)
   {
      int status =  OPA_cas_int (&slot->status, STATUS_ARMED, newstatus);

      if (status == STATUS_UNINITIALIZED)
      {
         /* invalid slot */
         return AE_ERR_NOT_FOUND;
      }

      if (status == STATUS_COMPLETED_SUCCESS || status == STATUS_COMPLETED_CANCEL)
      {
         /* was already completed */
         return AE_ERR_INVALID;
      }

      if (status == STATUS_ARMED)   /* we changed status ! */
      {
         /* was armed, we just completed it */

         /* some internal checks */
         int resource_id;

         assert (slot == (rb_slot_t *) intptr2op (ae_id_lookup (slot->op_id,
                     &resource_id)));

         assert (resource_id == rb_resource_id);

         // Complete op
         void (*callback)(void *, int) = slot->op.callback;
         void * user_ptr = slot->op.user_ptr;
         callback (user_ptr, (success ? AE_SUCCESS : AE_ERR_INVALID));
         return AE_SUCCESS;
      }

      if (status == STATUS_EMPTY) /* Did not change status; try again */
      {
         status = OPA_cas_int (&slot->status, STATUS_EMPTY, newstatus);
         if (status == STATUS_EMPTY)
         {
            /* just went from EMPTY -> COMPLETED: immediate completion */
            return AE_SUCCESS;
         }

         /* somebody changed the status in between our previous check and this
          * one. Try again */
      }
   }
   return AE_SUCCESS; /* silence warning */
#endif
}

int rb_slot_complete (rb_slot_t * slot)
{
   /* We return AE_SUCCESS if we completed the slot,
    * AE_ERR_INVALID if the slot was already completed or cancelled.
    */
   return (rb_callback (slot, 1) == AE_SUCCESS ?
         AE_SUCCESS       /* we completed the slot */
       : AE_ERR_INVALID   /* slot was already completed */);
}

void rb_slot_destroy (rb_slot_t * slot)
{
   /* make sure the slot has been properly completed (or cancelled) */
   const int status = OPA_load_int (&slot->status);
   assert (status == STATUS_COMPLETED_CANCEL ||
         status == STATUS_COMPLETED_SUCCESS);
   OPA_store_int (&slot->status, STATUS_UNINITIALIZED);
}

ae_define_post (int, rb_slot_capture, rb_slot_t * slot)
{
#ifdef RB_NO_OPA
   int ret;
   int aret;

   triton_mutex_lock (&slot->lock);
   switch (slot->status)
   {
      case STATUS_COMPLETED_SUCCESS:
         ret = AE_SUCCESS;
         aret = AE_IMMEDIATE_COMPLETION;
         break;
      case STATUS_COMPLETED_CANCEL:
         aret = AE_ERR_INVALID;
         ret = AE_IMMEDIATE_COMPLETION;
         break;
      case STATUS_UNINITIALIZED:
         ret = AE_ERR_NOT_FOUND;
         aret = AE_IMMEDIATE_COMPLETION;
         break;
      case STATUS_ARMED:
         ret = AE_ERR_INVALID;
         aret = AE_IMMEDIATE_COMPLETION;
         break;
      case STATUS_EMPTY:
         ae_op_fill (&slot->op);
         slot->op_id = ae_id_gen (rb_resource_id, (uintptr_t) slot);
         ret = AE_SUCCESS;
         aret = AE_SUCCESS;
         slot->status = STATUS_ARMED;
         break;
   }
   triton_mutex_unlock (&slot->lock);

   *__ae_retval = ret;
   return aret;

#else
   /* Only arm the slot if it hasn't completed yet */
   int status;

   /* need to do this first before changing status */
   ae_op_fill (&slot->op);
   slot->op_id = ae_id_gen (rb_resource_id, (uintptr_t) slot);

   status = OPA_cas_int (&slot->status, STATUS_EMPTY, STATUS_ARMED);

   if (status == STATUS_UNINITIALIZED)
   {
      /* invalid slot */
      *__ae_retval = AE_ERR_NOT_FOUND;
      return AE_IMMEDIATE_COMPLETION;
   }

   if (status == STATUS_COMPLETED_SUCCESS)
   {
      *__ae_retval = AE_SUCCESS;
      return AE_IMMEDIATE_COMPLETION; /* immediate completion */
   }

   if (status == STATUS_COMPLETED_CANCEL)
   {
      /* immediate completion */
      /* was already cancelled. (Really not possible right now) */
      assert (0);
      *__ae_retval = AE_ERR_INVALID;
      return AE_IMMEDIATE_COMPLETION;
   }

   if (status == STATUS_ARMED)
   {
      assert (0 && "Slot was already armed (and we overwrote the context)!");
      return AE_IMMEDIATE_COMPLETION;
   }
   assert (status == STATUS_EMPTY);
   /* we just transitioned from empty to armed */

   return AE_SUCCESS;
#endif
}

static int rb_cancel (ae_context_t rb_ctx, ae_op_id_t op_id)
{
   int resource_id;

   /* we store the slot pointer into the op_id to avoid having to do
    * the weird member-offset math */
   rb_slot_t * slot = (rb_slot_t *)
      intptr2op (ae_id_lookup (op_id, &resource_id));

   /* consistency checks */
   assert (resource_id == rb_resource_id);
   assert (ae_op_id_equal (op_id, slot->op_id));

   /* Make the capture function return AE_ERR_INVALID since the slot
    * was cancelled. */
   return (rb_callback (slot, 0) == AE_SUCCESS ?
         AE_SUCCESS       /* we managed to cancel the slot */
       : AE_ERR_INVALID);   /* slot already completed */
}

struct ae_resource rb_resource =
{
   .resource_name = RB_RESOURCE_NAME,
   .cancel = rb_cancel,
   .config_array = 0
};



static int rb_init (void);

int rb_init (void)
{
   return ae_resource_register (&rb_resource, &rb_resource_id);
}

static void rb_finalize (void);

void rb_finalize (void)
{
   ae_resource_unregister (rb_resource_id);
}

__attribute__((constructor)) void rb_init_register(void);

__attribute__((constructor)) void rb_init_register(void)
{
    ae_resource_init_register(RB_RESOURCE_NAME, rb_init, rb_finalize);
}



/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
