found something!
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
#include "src/c-utils/triton-uint128.h"
#include "src/c-utils/triton-thread.h"
#include "src/c-utils/triton-list.h"
#include "src/c-utils/triton-string.h"

#define AE_MAX_RESOURCES 255
#define AE_MAX_CONTEXTS 1024

#define AE_RESOURCE_MASK (((uint64_t)0xFF)<<56)
#define AE_GET_RESOURCE_MASK(resource_id) (AE_RESOURCE_MASK&(((uint64_t)resource_id)<<56))
#define AE_GET_RESOURCE_ID(op_id) (((op_id&AE_RESOURCE_MASK)>>56)&0xFF)


#define ae_define_post(__ret_type, __fname, __fargs...)                    \
    int __fname(void (*__ae_callback)(void *ptr, __ret_type ret), \
                         void *__ae_user_ptr,                              \
                         ae_hints_t *__ae_hints,                           \
                         ae_context_t __ae_ctx,                            \
                         ae_op_id_t *__ae_op_id,                           \
                         int __ae_internal,                                \
                         __ret_type *__ae_retval,                          \
                         ##__fargs)

int ae_check_debug_flag(int resource_id);

#define ae_debug(__resource_id, __format, ...) \
    if(ae_check_debug_flag(__resource_id)) fprintf(stderr, __format, ## __VA_ARGS__)

/* this structure defines a configuration parameter for a resource */
struct ae_resource_config
{
    const char* name;
    const char* default_value;
    const char* description;
    int (*updater)(const char* key, const char* value);
};

/* The resource structure is defined by a given resource, and registered
 * to the aesop management code during resource initialization.
 */
struct ae_resource
{
    const char *resource_name;
    int (*test)(ae_op_id_t id, int ms_timeout);
    int (*poll_context)(ae_context_t context);
    int (*cancel)(ae_context_t ctx, ae_op_id_t id);
    int (*register_context)(ae_context_t context);
    int (*unregister_context)(ae_context_t context);
    struct ae_resource_config* config_array;  /* terminated by entry with NULL name */
};

/* called to register the initialization and finalization methods for a
 * resource with the aesop framework.  Does not activate the resource.
 */
int ae_resource_init_register(const char* resource_name, 
    int (*init)(void),
    void (*finalize)(void));

/* initializes the resource named in the argument.  This function is called
 * by aesop itself when it wants to activate a resource. 
 */
int ae_resource_init(const char* resource);
int ae_resource_init_all(void);

/* Called by resources to register themselves to the resource framework once
 * they are initialized and ready for use by aesop */
int ae_resource_register(struct ae_resource *resource, int *newid);
void ae_resource_unregister(int resource_id);

/* Called by resources to request polling from the event loop */
void ae_resource_request_poll(ae_context_t context, int resource_id);
/* this function is used by resources that want access to the event loop
 * used by aesop for this context
 */
struct ev_loop * ae_resource_get_eloop(ae_context_t context);

/* Cancel an operation */
int ae_cancel_op(ae_context_t context, ae_op_id_t op_id);

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
