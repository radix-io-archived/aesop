
#include "ae-ctl.h"

static triton_list_t ae_lone_pbranch_list = TRITON_LIST_STATIC_INITIALIZER(ae_lone_pbranch_list);
static triton_mutex_t ae_lone_pbranch_mutex = TRITON_MUTEX_INITIALIZER;

void ae_lone_pbranches_add(struct ae_ctl *ctl)
{
    triton_mutex_lock(&ae_lone_pbranch_mutex);
    triton_queue_enqueue(&ctl->link, &ae_lone_pbranch_list);
    triton_mutex_unlock(&ae_lone_pbranch_mutex);
}

void ae_lone_pbranches_remove(struct ae_ctl *ctl)
{
    triton_mutex_lock(&ae_lone_pbranch_mutex);
    triton_list_del(&ctl->link);
    triton_mutex_unlock(&ae_lone_pbranch_mutex);
}

int ae_lone_pbranches_count(void)
{
    int ret;
    triton_mutex_lock(&ae_lone_pbranch_mutex);
    ret = triton_list_count(&ae_lone_pbranch_list);
    triton_mutex_unlock(&ae_lone_pbranch_mutex);
    return ret;
}
