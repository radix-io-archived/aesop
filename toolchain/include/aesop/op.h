/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */


#ifndef __OP_H__
#define __OP_H__

#include "aesop.h"
#include <triton-list.h>

#include <stdint.h>

/**
 * TODO: there's really no reason why the op structure has to be mixed in with
 * the opcache; The link member is needed for the op_cache, and is not needed
 * for the op.
 */
typedef struct ae_op
{
    void *callback;
    /*int (*op_worker)(struct ae_op* op); */
    void *user_ptr;
    ae_hints_t *hints;
    triton_list_link_t link;
} ae_op_t;

#define ae_op_from_link(_link) triton_list_get_entry((_link), struct ae_op, link)

#define ae_op_entry(_op, _type, _member) \
    ((_op != NULL) ?                     \
        ((_type *)((char *)(_op) - (unsigned long)((&((_type *)0)->_member)))) : NULL)

#define ae_invoke_callback(_op, __ret_type, _result) \
    ((void (*)(void *, __ret_type))(_op)->callback)((_op)->user_ptr, _result)

#define ae_op_fill(_op)                         \
    do {                                        \
        (_op)->callback = (__ae_callback);      \
        (_op)->user_ptr = (__ae_user_ptr);      \
        (_op)->hints = (__ae_hints);            \
    } while(0)

#define ae_op_fill_with_params(_op, _cb, _up, _hints) \
    do {                                                    \
        (_op)->callback = (_cb);                            \
        (_op)->user_ptr = (_up);                            \
        (_op)->hints = (_hints);                            \
    } while(0)

#define ae_ops_link_clear(_op) triton_list_link_clear(&(_op)->link)

#define ae_op_clear(_op)                        \
    do {                                        \
        (_op)->callback = NULL;                 \
        (_op)->user_ptr = NULL;                 \
        (_op)->hints = NULL;                    \
        ae_ops_link_clear(_op);                 \
    } while(0)

typedef triton_list_t ae_ops_t;

#define ae_ops_init(ops) triton_list_init(ops)

#define ae_ops_destroy(ops) (void) ops

#define ae_ops_enqueue(_op, _queue) triton_queue_enqueue(&(_op)->link, _queue)

/**
 * Initialize the ae op link
 */
static inline void ae_ops_link_init (ae_op_t * op)
{
   triton_list_link_clear (&op->link);
}

static inline ae_op_t *ae_ops_dequeue(ae_ops_t *queue)
{
    triton_list_link_t *llink;
    llink = triton_queue_dequeue(queue);
    if(!llink) return NULL;
    return ae_op_from_link(llink);
}

/**
 * Return the ae_ops_t this op is on, NULL if not on any queue.
 */
static inline ae_ops_t * ae_ops_get_queue (ae_op_t * op)
{
   if (!op)
      return 0;

   return (ae_ops_t *) triton_list_return_list (&op->link);
}

#define ae_ops_del(_op) triton_list_del(&(_op)->link)
#define ae_ops_empty(_ops) triton_list_empty(_ops)

static inline ae_op_t *ae_ops_peek(ae_ops_t *queue)
{
    triton_list_link_t *llink;
    llink = triton_queue_peek(queue);
    if(!llink) return NULL;
    return ae_op_from_link(llink);
}

#define ae_ops_for_each(_pos, _safe, _ops) triton_list_for_each_entry(_pos, _safe, _ops, struct ae_op, link)
#define ae_ops_for_each_reverse(_pos, _safe, _ops) triton_list_for_each_entry_reverse(_pos, _safe, _ops, struct ae_op, link)
#define ae_ops_exists(_ops, _op) triton_list_exists(_ops, _op)
#define ae_ops_count(_ops) triton_list_count(_ops)
#define ae_ops_find(_ops, _compare, _ptr) triton_list_find(_ops, _compare, _ptr)

#define ae_ops_insert_after(_new, _after, _ops) triton_list_insert_after(&(_new)->link, &(_after)->link, _ops)
#define ae_ops_insert_before(_new, _before, _ops) triton_list_insert_before(&(_new)->link, &(_before)->link, _ops)


/*
 * This function was added to simplify casting from an op_id to the op
 * structure.
 */
static inline struct ae_op * intptr2op (intptr_t op)
{
   return (struct ae_op *) op;
}

/**
 * Call the callback described by the op.
 */
#define ae_op_execute(__op, __ret_type, __error_code) do { \
    struct ae_ctl *__ctl = (__op)->user_ptr; \
    __ctl->op_state &= ~OP_COMPLETED_NORMAL; \
    void (*__callback)(void *, __ret_type) = (__op)->callback;	\
    void* __user_ptr = (__op)->user_ptr; \
    __ret_type __saved_error_code = __error_code; \
    __callback(__user_ptr, __saved_error_code); \
    } while(0)



#endif

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
