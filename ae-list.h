#ifndef AE_LIST_H
#define AE_LIST_H

/**
 * Aesop uses Triton list functions by default.  Replace
 * this header with your own if you want to use Aesop without
 * Triton.
 */
#include "src/common/triton-list.h"

typedef triton_list_t ae_list_t;
typedef triton_list_link_t ae_list_link_t;

#define ae_list_link triton_list_link

#define ae_list_init triton_list_init

#define ae_list_count triton_list_count

#define ae_list_empty triton_list_empty

#define ae_list_link_clear triton_list_link_clear
#define ae_list_add_back triton_list_add_back

#define ae_list_del triton_list_del

#define ae_queue_enqueue triton_queue_enqueue
#define ae_queue_dequeue triton_queue_dequeue
#define ae_queue_peek triton_queue_peek

#define ae_list_for_each_entry triton_list_for_each_entry
#define ae_list_for_each_entry_reverse triton_list_for_each_entry_reverse

#define ae_list_exists triton_list_exists
#define ae_list_find triton_list_find

#define ae_list_insert_after triton_list_insert_after
#define ae_list_insert_before triton_list_insert_before

#define ae_list_get_entry triton_list_get_entry

#endif
