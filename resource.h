#ifndef __RESOURCE_H__
#define __RESOURCE_H__

#include "src/common/triton-thread.h"
#include "src/common/triton-list.h"

#define AE_MAX_RESOURCES 255
#define AE_MAX_CONTEXTS 1024

#define AE_RESOURCE_MASK (((uint64_t)0xFF)<<56)
#define AE_GET_RESOURCE_MASK(resource_id) (AE_RESOURCE_MASK&(((uint64_t)resource_id)<<56))
#define AE_GET_RESOURCE_ID(op_id) (((op_id&AE_RESOURCE_MASK)>>56)&0xFF)

typedef struct ae_context *ae_context_t;

typedef struct ae_hints *ae_hints_t;

typedef uint64_t ae_op_id_t;

ae_op_id_t ae_id_gen(int resource_id, uint64_t ptr);
uint64_t ae_id_lookup(ae_op_id_t id, int *resource_id);

/* The resource structure is defined by a given resource, and registered
 * to the aesop management code during resource initialization.
 */
struct ae_resource
{
    const char *resource_name;
    triton_ret_t (*test)(ae_op_id_t id, int ms_timeout);
    triton_ret_t (*poll_context)(ae_context_t context, int ms_timeout);
    triton_ret_t (*cancel)(ae_context_t ctx, ae_op_id_t id);
    triton_ret_t (*register_context)(ae_context_t context);
    triton_ret_t (*unregister_context)(ae_context_t context);
};

/* Called by resources to register themselves to the resource framework */
triton_ret_t ae_resource_register(struct ae_resource *resource, int *newid);
void ae_resource_unregister(int resource_id);

/* Contexts are created to allow separation of polling for different logical
 * groups of operations.
 */
triton_ret_t ae_context_create(ae_context_t *context, int resource_count, ...);

/* Context destruction.  Called to cleanup state allocated in ae_context_create.
 */
triton_ret_t ae_context_destroy(ae_context_t context);

/* Poll for completion of the operations within the given context up to a
 * timeout value.
 */
triton_ret_t ae_poll(ae_context_t context, int ms);

/* Cancel an operation */
triton_ret_t ae_cancel_op(ae_context_t context, ae_op_id_t op_id);

/* The ae_ctl structure is used by the aesop generated code to manage parallel and
 * nested operations.  This structure is not needed by resource writers or aesop 
 * code.
 * TODO: move these to a separate header
 */
struct ae_ctl
{
    ae_op_id_t current_op_id;
    int cancelled;
    triton_mutex_t mutex;
    triton_list_t children;
    struct triton_list_link link;
    int posted;
    int completed;
    int allposted;
    int hit_pbreak;
    int in_pwait;    
};

/* internal function -- used by generated code */
int ae_cancel_children(ae_context_t ctx, void *ptr);

void ae_backtrace(void);

#endif /* __AE_RESOURCE_H__ */
