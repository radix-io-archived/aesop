
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <pthread.h>
#include "src/aesop/aesop.h"
#include "src/common/triton-list.h"
#include "src/aesop/parser/tests/blocking/btest.h"
#include "src/common/triton-init.h"

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
static ae_ops_t list_fail10;
static ae_ops_t slist;
static ae_ops_t srlist;
static ae_ops_t clist;
static ae_ops_t flist;
static ae_ops_t rlist;

static ae_opcache_t test_opcache;
static ae_opcache_t sleep_opcache;

ae_define_post(int, ictest1, int *a)
{
    ++(*a);
    *__ae_retval = 0;
    return AE_IMMEDIATE_COMPLETION;
}

ae_define_post(int, ictest_random)
{
    int r = random() % 1000;
    printf("ictest_random sleep: %d\n", r);
    usleep(r);
    *__ae_retval = 0;
    return AE_IMMEDIATE_COMPLETION;
}

static void *tctest1_threadfun(void *ptr)
{
    struct ae_op *op;
    struct btest_op *bop;
    op = (struct ae_op *)ptr;
    bop = ae_op_entry(op, struct btest_op, op);
    *(bop->value) += 1;
    ae_opcache_complete_op(test_opcache, &bop->op, int, 0);
    pthread_exit(ptr);

    return NULL;
}

ae_define_post(int, tctest1, int *a)
{
    int ret;
    pthread_t tid;
    pthread_attr_t attr;
    struct ae_op *op;
    struct btest_op *bop;
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ret = pthread_attr_init(&attr);
    assert(ret == 0);
    ret = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    assert(ret == 0);
    ret = pthread_create(&tid, &attr, tctest1_threadfun, op);
    assert(ret == 0);
    pthread_attr_destroy(&attr);
    usleep(1000);

    return TRITON_SUCCESS;
}

static void *tcrandom_threadfun(void *ptr)
{
    int r;
    struct ae_op *op;
    struct btest_op *bop;
    op = (struct ae_op *)ptr;
    bop = ae_op_entry(op, struct btest_op, op);
    r = random() % 1000;
    printf("tctest_random sleep: %d\n", r);
    usleep(r);
    ae_opcache_complete_op(test_opcache, &bop->op, int, 0);
    pthread_exit(ptr);

    return NULL;
}

ae_define_post(int, tctest_random)
{
    int ret;
    pthread_t tid;
    pthread_attr_t attr;
    struct ae_op *op;
    struct btest_op *bop;
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    printf("tctest_random\n");
    ret = pthread_attr_init(&attr);
    assert(ret == 0);
    ret = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    assert(ret == 0);
    ret = pthread_create(&tid, &attr, tcrandom_threadfun, op);
    assert(ret == 0);
    pthread_attr_destroy(&attr);
    usleep(random() % 1000);
    return TRITON_SUCCESS;
}

ae_define_post(triton_ret_t, btest_fail10, int *a)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST_FAIL10: %d\n", *a);
    op = ae_opcache_get(test_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct btest_op, op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list_fail10);
    ae_resource_request_poll(op->ctx, btest_resource_id);

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
    ae_resource_request_poll(op->ctx, btest_resource_id);

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
    ae_resource_request_poll(op->ctx, btest_resource_id);

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
    ae_resource_request_poll(op->ctx, btest_resource_id);

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
    ae_resource_request_poll(op->ctx, btest_resource_id);

    return TRITON_SUCCESS;
}

ae_define_post(int, btest_sleep_random)
{
    struct ae_op *op;
    struct bsleep_op *bop;
    printf("BTEST SLEEP RANDOM\n");
    op = ae_opcache_get(sleep_opcache);
    ae_op_fill(op);
    bop = ae_op_entry(op, struct bsleep_op, op);
    bop->sleep = random() % 1000;
    bop->id = ae_id_gen(btest_resource_id, (uint64_t)(op->cache_id));
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &srlist);
    ae_resource_request_poll(op->ctx, btest_resource_id);

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
    ae_resource_request_poll(op->ctx, btest_resource_id);

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
    ae_resource_request_poll(op->ctx, btest_resource_id);

    return TRITON_SUCCESS;
}

static struct bsleep_op * poll_sleep_list(int *more)
{
   struct ae_op *t;
   t = ae_ops_dequeue(&slist);
   if(t)
   {
       if(ae_ops_empty(&slist))
       {
           *more = 0;
       }
       else
       {
           *more = 1;
       }
       return ae_op_entry(t, struct bsleep_op, op);
   }
   return NULL;
}

static struct bsleep_op * poll_rsleep_list(int *more)
{
   struct ae_op *t;
   t = ae_ops_dequeue(&srlist);
   if(t)
   {
       if(ae_ops_empty(&srlist))
       {
           *more = 0;
       }
       else
       {
           *more = 1;
       }
       return ae_op_entry(t, struct bsleep_op, op);
   }
   return NULL;
}

static struct btest_op * poll_list(ae_ops_t *list, int *more)
{
   struct ae_op *t;
   t = ae_ops_dequeue(list);
   if(t)
   {
       if(ae_ops_empty(list))
       {
           *more = 0;
       }
       else
       {
           *more = 1;
       }
       return ae_op_entry(t, struct btest_op, op);
   }
   return NULL;
}

static triton_ret_t btest_poll(ae_context_t context)
{
   int more, request;
   struct btest_op *b;
   struct bsleep_op *s;

   more = 0;
   request = 0;

   b = poll_list(&list1, &more);
   if(b)
   {
	*(b->value) += 1;
        ae_opcache_complete_op(test_opcache, &b->op, int, 0);
   }
   request = more;

   b = poll_list(&list2, &more);
   if(b)
   {
	*(b->value) += 2;
        ae_opcache_complete_op(test_opcache, &b->op, int, 0);
   }
   request = request | more;

   b = poll_list(&list3, &more);
   if(b)
   {
	*(b->value) += 3;
        ae_opcache_complete_op(test_opcache, &b->op, int, 0);
   }
   request = request | more;

   b = poll_list(&list_fail10, &more);
   if(b)
   {
	*(b->value) += 1;
        if(*(b->value) >= 10)
        {
            printf("BTEST_FAIL10 returning success.\n");
            ae_opcache_complete_op(test_opcache, &b->op, triton_ret_t, TRITON_SUCCESS);
        }
        else
        {
            printf("BTEST_FAIL10 returning TRITON_ERR_AGAIN.\n");
            ae_opcache_complete_op(test_opcache, &b->op, triton_ret_t, TRITON_ERR_AGAIN);
        }
   }
   request = request | more;

   b = poll_list(&clist, &more);
   if(b)
   {
        ae_opcache_complete_op(test_opcache, &b->op, int, -1);
   }
   request = request | more;

   b = poll_list(&rlist, &more);
   if(b)
   {
       ae_opcache_complete_op(test_opcache, &b->op, int, random());
   }
   request = request | more;

   s = poll_sleep_list(&more);
   if(s)
   {
	sleep(s->sleep);
        ae_opcache_complete_op(sleep_opcache, &s->op, int, 0);
   }
   request = request | more;

   s = poll_rsleep_list(&more);
   if(s)
   {
	usleep(s->sleep);
        ae_opcache_complete_op(sleep_opcache, &s->op, int, 0);
   }
   request = request | more;

   if(request)
   {
       ae_resource_request_poll(context, btest_resource_id);
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
            ae_resource_request_poll(ctx, btest_resource_id);
	}
   }

   return TRITON_SUCCESS;
}     

static triton_ret_t btest_cancel_immed(ae_context_t ctx, ae_op_id_t op_id)
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
            ae_opcache_complete_op(test_opcache, &b->op, int, 0);
            ae_resource_request_poll(ctx, btest_resource_id);
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

static triton_ret_t btest_init(void)
{
    ae_resource_register(&btest_resource, &btest_resource_id);

    AE_OPCACHE_INIT(struct bsleep_op, op, 1024, &sleep_opcache);
    AE_OPCACHE_INIT(struct btest_op, op, 1024, &test_opcache);

    ae_ops_init(&list1);
    ae_ops_init(&list2);
    ae_ops_init(&list3);
    ae_ops_init(&list_fail10);
    ae_ops_init(&slist);
    ae_ops_init(&srlist);
    ae_ops_init(&clist);
    ae_ops_init(&flist);
    ae_ops_init(&rlist);

    return TRITON_SUCCESS;
}

static void btest_finalize(void)
{
    ae_resource_unregister(btest_resource_id);

    ae_opcache_destroy(sleep_opcache);
    ae_opcache_destroy(test_opcache);
}

__attribute__((constructor)) void btest_init_register(void);
__attribute__((constructor)) void btest_init_register(void)
{
    triton_init_register("aesop.blocking.test", btest_init, btest_finalize, NULL, "aesop.control", "triton.resource.timer");
}

void btest_enable_immediate_cancel(void)
{
    /* switch function pointers */
    btest_resource.cancel = btest_cancel_immed;
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ts=8 sts=4 sw=4 expandtab
 */
