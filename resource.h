/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#ifndef __RESOURCE_H__
#define __RESOURCE_H__

#include "ae-types.h"
#include "ae-error.h"
#include "ae-ctl.h"
#include "hints.h"
#include <triton-uint128.h>
#include <triton-thread.h>
#include <triton-list.h>
#include <triton-string.h>

#define AE_MAX_CONTEXTS 1024

#define AE_RESOURCE_MASK (((uint64_t)0xFF)<<56)
#define AE_GET_RESOURCE_MASK(resource_id) (AE_RESOURCE_MASK&(((uint64_t)resource_id)<<56))
#define AE_GET_RESOURCE_ID(op_id) (((op_id&AE_RESOURCE_MASK)>>56)&0xFF)

/**
 * The post function should:
 *    - return AE_IMMEDIATE_COMPLETION
 *        and set *__ae_retval to the return of the functino.
 *    - return AE_SUCCESS
 *        and set *__ae_op_id to the opid of the posted operation
 *    - return an error
 *
 *  __ae_user_ptr will point to the ctl of the caller.
 */
#define ae_define_post(__ret_type, __fname, __fargs...)                    \
    int __fname(void (*__ae_callback)(void *ptr, __ret_type ret), \
                         void *__ae_user_ptr,                              \
                         ae_hints_t *__ae_hints,                           \
                         ae_op_id_t *__ae_op_id,                           \
                         int __ae_internal,                                \
                         __ret_type *__ae_retval,                          \
                         ##__fargs)


/**
 * Check if the current control structure has the cancelled state.
 * This function is meant to be used from within a resource post call
 * to check if the function should fail immediately.
 *
 * This function assumes that, within a resource post call,
 * __ae_user_ptr points to the ctl structure of the caller
 * (which is the case for resource post functions called by aesop).
 *
 * No lock is needed as the post function is called with the ctl lock held.
 */
#define ae_resource_is_cancelled() \
   ((struct ae_ctl *) __ae_user_ptr)->op_state & OP_REQUEST_CANCEL


/**
 * This function can be called from within a blocking function to clear the
 * cancelled state. It is a user function, and should not be called from
 * within a resource.
 */
static inline void ae_clear_cancel (struct ae_ctl * ctl)
{
   triton_mutex_lock (&ctl->mutex);
   ctl->op_state &= ~OP_REQUEST_CANCEL;
   triton_mutex_unlock (&ctl->mutex);
}

/**
 * Set the cancellation flag for this context.
 * This function can be used to restore the cancel signal after (temporarily
 * clearing it, for example when blocking functions need to be called to
 * perform state cleanup)
 */
static inline void ae_set_cancel (struct ae_ctl * ctl)
{
    triton_mutex_lock (&ctl->mutex);
    ctl->op_state |= OP_REQUEST_CANCEL;
    triton_mutex_unlock (&ctl->mutex);
}

int ae_check_debug_flag(int resource_id);

#define ae_debug(__resource_id, __format, ...) \
    if(ae_check_debug_flag(__resource_id)) fprintf(stderr, __format, ## __VA_ARGS__)

/* The resource structure is defined by a given resource, and registered
 * to the aesop management code during resource initialization.
 *
 * The cancel function should cancel asynchronously;
 *   it cannot block until the operation was cancelled.
 *   it (might) be OK to call the callback from the cancel function
 *        (not sure for now) -- probably best to avoid it.
 */
struct ae_resource
{
    const char *resource_name;
    int (*test)(ae_op_id_t id, int ms_timeout);
    int (*poll)(void *user_data);
    int (*cancel)(ae_op_id_t id);
};

/**
 * Cleanup resource 
 */
int ae_resource_cleanup (void);

/* initializes the resource named in the argument.  This function is called
 * by aesop itself when it wants to activate a resource. 
 */
int ae_resource_init(const char* resource);
int ae_resource_init_all(void);

/**
 * Finalize specified/all resources
 */
int ae_resource_finalize_all (void);

int ae_resource_finalize (const char * resource);

int ae_resource_finalize_active (void);

/* Called by resources to register themselves to the resource framework once
 * they are initialized and ready for use by aesop */
int ae_resource_register(struct ae_resource *resource, int *newid);
int ae_resource_register_with_data(struct ae_resource *resource, int *newid, void *user_data);
void ae_resource_unregister(int resource_id);

/* Called by resources to request polling from the event loop */
void ae_resource_request_poll(int resource_id);

/* Cancel an operation */
int ae_cancel_op(ae_op_id_t op_id);


/**
 * This function should be called by resources when they are about to complete
 * the op and call the callback. It marks the op as completed, so it can no
 * longer be cancelled (or returns 0 if the op was already cancelled or
 * completed).
 *
 * This function assumes that the user_ptr field of ae_op points to the parent
 * ctl structure (as is the case for resource post functions called by aesop.
 *
 * Return non-zero if the op was marked as completed, 0 otherwise.
 *
 * This function locks the ctl structure.
 */

struct ae_op;

int ae_op_complete (struct ae_op * op);



void ae_backtrace(void);

void ae_get_stack(struct ae_ctl *ctl, triton_string_t *stack, int *inout_count);
void ae_print_stack(FILE *outstream, struct ae_ctl *ctl);

#ifdef AESOP_PARSER
#define aesop_get_stack(stack, io_count) ae_get_stack(&__ae_ctl->gen, stack, io_count)
#else
static inline void aesop_get_stack(triton_string_t *strings, int *inout_count)
{
    *inout_count = 0;
}
#endif

#ifdef AESOP_PARSER
#define aesop_print_stack(stream) ae_print_stack(stream, &__ae_ctl->gen)
#else
static inline void aesop_print_stack(FILE *stream) { }
#endif

#endif /* __AE_RESOURCE_H__ */

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
