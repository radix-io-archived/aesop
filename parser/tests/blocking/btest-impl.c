
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <pthread.h>
#include "src/aesop/aesop.h"
#include "src/common/triton-list.h"
#include "src/aesop/parser/tests/blocking/btest.h"

static int btest_resource_id;

struct bsleep_op
{
    int sleep;
    ae_op_id_t id;
    struct ae_op op;
};

struct btest_op
{
    int *value;
    ae_op_id_t id;
    struct ae_op op;
};

static ae_ops_t list1;
static ae_ops_t list2;
static ae_ops_t list3;
static ae_ops_t slist;
static ae_ops_t clist;
static ae_ops_t flist;
static ae_ops_t rlist;

static ae_opcache_t test_opcache;
static ae_opcache_t sleep_opcache;

ae_define_post(int, ictest1, int *a)
{
    ++(*a);
    __ae_callback(__ae_user_ptr, 0);
    return TRITON_SUCCESS;
}

static void *tctest1_threadfun(void *ptr)
{
    struct ae_op *op;
    struct btest_op *bop;
    op = (struct ae_op *)ptr;
    bop = ae_op_entry(op, struct btest_op, op);
    *(bop->value) += 1;
    ae_opcache_complete_op(test_opcache, &bop->op, int, 0);

    return NULL;
}

ae_define_post(int, tctest1, int *a)
{
    pthread_t tid;
    struct ae_op *op;
    struct btest_op *bop;
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    pthread_create(&tid, NULL, tctest1_threadfun, op);
    usleep(1000);

    return TRITON_SUCCESS;
}

ae_define_post(int, btest1, int *a)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST1: %d\n", *a);
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list1);

    return TRITON_SUCCESS;
}

ae_define_post(int, btest2, int *a)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST2: %d\n", *a);
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list2);

    return TRITON_SUCCESS;
}

ae_define_post(int, btest3, int *a)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST3: %d\n", *a);
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list3);

    return TRITON_SUCCESS;
}

ae_define_post(int, btest_sleep, int secs)
{
    struct ae_op *op;
    struct bsleep_op *bop;
    printf("BTEST SLEEP\n");
    op = ae_opcache_get(sleep_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct bsleep_op, op);
    bop->sleep = secs;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &slist);

    return TRITON_SUCCESS;
}

ae_define_post(int, btest_forever)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST FOREVER\n");
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->value = NULL;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &flist);

    return TRITON_SUCCESS;
}

ae_define_post(int, btest_random)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST RANDOM\n");
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->value = NULL;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &rlist);

    return TRITON_SUCCESS;
}

static struct bsleep_op * poll_sleep_list(void)
{
   struct ae_op *t;
   t = ae_ops_dequeue(&slist);
   if(t)
   {
	   return ae_op_entry(t, struct bsleep_op, op);
   }
   return NULL;
}

static struct btest_op * poll_list(ae_ops_t *list)
{
   struct ae_op *t;
   t = ae_ops_dequeue(list);
   if(t)
   {
	   return ae_op_entry(t, struct btest_op, op);
   }
   return NULL;
}

static triton_ret_t btest_poll(ae_context_t context, int ms)
{
   struct btest_op *b;
   struct bsleep_op *s;

   b = poll_list(&list1);
   if(b)
   {
	*(b->value) += 1;
        ae_opcache_complete_op(test_opcache, &b->op, int, 0);
   }

   b = poll_list(&list2);
   if(b)
   {
	*(b->value) += 2;
        ae_opcache_complete_op(test_opcache, &b->op, int, 0);
   }

   b = poll_list(&list3);
   if(b)
   {
	*(b->value) += 3;
        ae_opcache_complete_op(test_opcache, &b->op, int, 0);
   }

   b = poll_list(&clist);
   if(b)
   {
        ae_opcache_complete_op(test_opcache, &b->op, int, -1);
   }

   b = poll_list(&rlist);
   if(b)
   {
       ae_opcache_complete_op(test_opcache, &b->op, int, random());
   }

   s = poll_sleep_list();
   if(s)
   {
	sleep(s->sleep);
        ae_opcache_complete_op(sleep_opcache, &s->op, int, 0);
   }
   return TRITON_SUCCESS;
}

static triton_ret_t btest_cancel(ae_context_t ctx, ae_op_id_t op_id)
{
   struct ae_op *t, *tmp;
   struct btest_op *b;

   ae_ops_for_each(t, tmp, &flist)
   {
	b = ae_op_entry(t, struct btest_op, op);
	if(ae_op_id_equal(b->id, op_id))
	{
	    ae_ops_del(t);
	    printf("forever op cancelled\n");
	    ae_ops_enqueue(&b->op, &clist);
	}
   }

   return TRITON_SUCCESS;
}     

struct ae_resource btest_resource =
{
    .resource_name = "btest",
    .poll_context = btest_poll,
    .cancel = btest_cancel
};

int btest_init(void)
{
    ae_resource_register(&btest_resource, &btest_resource_id);

    AE_OPCACHE_INIT(struct bsleep_op, op, 1024, &sleep_opcache);
    AE_OPCACHE_INIT(struct btest_op, op, 1024, &test_opcache);

    ae_ops_init(&list1);
    ae_ops_init(&list2);
    ae_ops_init(&list3);
    ae_ops_init(&slist);
    ae_ops_init(&clist);
    ae_ops_init(&flist);
    ae_ops_init(&rlist);

    return 0;
}

void btest_finalize(void)
{
    ae_resource_unregister(btest_resource_id);

    ae_opcache_destroy(sleep_opcache);
    ae_opcache_destroy(test_opcache);
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
