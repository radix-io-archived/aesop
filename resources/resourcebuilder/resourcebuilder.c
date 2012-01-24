/* This needs to be the full path for make -- if relative,
 * make will try to build ./resourcebuilder.h and fail.
 */
#include "resources/resourcebuilder/resourcebuilder.h"

#define RB_DEFAULT_SIZE 64
#define RB_RESOURCE_NAME "resourcebuilder"

static int rb_resource_id;


void rb_slot_initialize (rb_slot_t * slot)
{
   OPA_store_int (&slot->completed, 0);
}

/**
 * Complete the slot with the given error code.
 * Return AE_SUCCESS if successful, AE_ERR_INVALID if the slot was already
 * completed.
 */
static int rb_callback (rb_slot_t * slot, int returncode)
{
   if (OPA_fetch_and_incr_int (&slot->completed))
   {
      /* somebody else already completed this slot (probably cancelled) */
      return AE_ERR_INVALID;
   }

   // Complete op
   void (*callback)(void *, int) = slot->op.callback;
   void * user_ptr = slot->op.user_ptr;
   callback (user_ptr, returncode);

   return AE_SUCCESS;
}

int rb_slot_complete (rb_slot_t * slot)
{
   int resource_id;
   struct ae_op * op = &slot->op;

   assert (slot == (rb_slot_t *) intptr2op (ae_id_lookup (slot->op_id,
               &resource_id)));

   assert (resource_id == rb_resource_id);

   /* We return AE_SUCCESS if we completed the slot,
    * AE_ERR_INVALID if the slot was already completed or cancelled.
    */
   return (rb_callback (slot, AE_SUCCESS) == AE_SUCCESS ?
         AE_SUCCESS       /* we completed the slot */
       : AE_ERR_INVALID   /* slot was already completed */);
}

void rb_slot_destroy (rb_slot_t * slot)
{
   /* make sure the slot has been properly completed (or cancelled) */
   assert (OPA_load_int (&slot->completed));
}

ae_define_post (int, rb_slot_capture, rb_slot_t * slot)
{
   /* slot must be unused */
   assert (!OPA_load_int (&slot->completed));

   ae_op_fill (&slot->op);
   slot->op_id = ae_id_gen (rb_resource_id, (uintptr_t) slot);
   return AE_SUCCESS;
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
   return (rb_callback (slot, AE_ERR_INVALID) == AE_SUCCESS ?
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


