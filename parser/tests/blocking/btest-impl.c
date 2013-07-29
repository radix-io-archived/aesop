/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <pthread.h>
#include "aesop.h"
#include <triton-list.h>
#include "parser/tests/blocking/btest.h"
#include "resource.h"

#define RESOURCE_NAME "btest"

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
static ae_ops_t list_consec;
static ae_ops_t list_fail10;
static ae_ops_t slist;
static ae_ops_t srlist;
static ae_ops_t clist;
static ae_ops_t flist;
static ae_ops_t rlist;

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
#if 0
    int normal_completion;
#endif
    op = (struct ae_op *)ptr;
    bop = ae_op_entry(op, struct btest_op, op);
    *(bop->value) += 1;

#if 0
    normal_completion = ae_op_complete(op);
    assert(normal_completion);
#endif
    ae_op_execute(op, int, 0);
    free(bop);
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
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ret = pthread_attr_init(&attr);
    assert(ret == 0);
    ret = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    assert(ret == 0);
    ret = pthread_create(&tid, &attr, tctest1_threadfun, op);
    assert(ret == 0);
    pthread_attr_destroy(&attr);
    usleep(1000);

    return 0;
}

static void *tcrandom_threadfun(void *ptr)
{
    int r;
    struct ae_op *op;
    struct btest_op *bop;
#if 0
    int normal_completion;
#endif
    op = (struct ae_op *)ptr;
    bop = ae_op_entry(op, struct btest_op, op);
    r = random() % 1000;
    printf("tctest_random sleep: %d\n", r);
    usleep(r);
#if 0
    normal_completion = ae_op_complete(op);
    assert(normal_completion);
#endif
    ae_op_execute(op, int, 0);
    free(bop);
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
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
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
    return 0;
}

ae_define_post(int, btest_fail10, int *a)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST_FAIL10: %d\n", *a);
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list_fail10);
    ae_resource_request_poll(btest_resource_id);

    return 0;
}

ae_define_post(int, btest_consec)
{
    struct ae_op *op;
    struct btest_op *bop;
    ae_debug(btest_resource_id, "btest_consec called.\n");

    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->value = 0;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list_consec);
    ae_resource_request_poll(btest_resource_id);

    return 0;
}

ae_define_post(int, btest1, int *a)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST1: %d\n", *a);
    ae_debug(btest_resource_id, "btest1 called.\n");

    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list1);
    ae_resource_request_poll(btest_resource_id);

    return 0;
}

ae_define_post(int, btest2, int *a)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST2: %d\n", *a);
    ae_debug(btest_resource_id, "btest2 called.\n");
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list2);
    ae_resource_request_poll(btest_resource_id);

    return 0;
}

ae_define_post(int, btest3, int *a)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST3: %d\n", *a);
    ae_debug(btest_resource_id, "btest3 called.\n");
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->value = a;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &list3);
    ae_resource_request_poll(btest_resource_id);

    return 0;
}

ae_define_post(int, btest_sleep, int secs)
{
    struct ae_op *op;
    struct bsleep_op *bop;
    printf("BTEST SLEEP\n");
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->sleep = secs;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &slist);
    ae_resource_request_poll(btest_resource_id);

    return 0;
}

ae_define_post(int, btest_sleep_random)
{
    struct ae_op *op;
    struct bsleep_op *bop;
    printf("BTEST SLEEP RANDOM\n");
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->sleep = random() % 1000;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &srlist);
    ae_resource_request_poll(btest_resource_id);

    return 0;
}

ae_define_post(int, btest_forever)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST FOREVER\n");
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->value = NULL;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &flist);
    ae_resource_request_poll(btest_resource_id);

    return 0;
}

ae_define_post(int, btest_random)
{
    struct ae_op *op;
    struct btest_op *bop;
    printf("BTEST RANDOM\n");
    bop = malloc(sizeof(*bop));
    assert(bop);
    op = &bop->op;
    ae_op_fill(op);
    ae_ops_link_init(op);
    bop->value = NULL;
    bop->id = ae_id_gen(btest_resource_id, (intptr_t) op);
    *__ae_op_id = bop->id;
    ae_ops_enqueue(op, &rlist);
    ae_resource_request_poll(btest_resource_id);

    return 0;
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

static int consec_count = 0;

static int btest_poll(void *user_data)
{
   int more, request;
   struct btest_op *b;
   struct bsleep_op *s;
   int normal_completion;

   more = 0;
   request = 0;

   b = poll_list(&list1, &more);
   if(b)
   {
	*(b->value) += 1;
#if 0
        normal_completion = ae_op_complete(&b->op);
        assert(normal_completion);
#endif
        ae_op_execute(&b->op, int, 0);
        free(b);
   }
   request = more;

   b = poll_list(&list2, &more);
   if(b)
   {
	*(b->value) += 2;
#if 0
        normal_completion = ae_op_complete(&b->op);
        assert(normal_completion);
#endif
        ae_op_execute(&b->op, int, 0);
        free(b);
   }
   request = request | more;

   b = poll_list(&list3, &more);
   if(b)
   {
	*(b->value) += 3;
#if 0
        normal_completion = ae_op_complete(&b->op);
        assert(normal_completion);
#endif
        ae_op_execute(&b->op, int, 0);
        free(b);
   }
   request = request | more;

   /* TODO: update the remainder of this resource to use ae_op_complete()
    * once the consecutive-resource-completions.ae test program issue is
    * resolved.
    */
   b = poll_list(&list_consec, &more);
   if(b)
   {
        normal_completion = ae_op_complete(&b->op);
        printf("DEBUG: iteration %d of btest_consec(): op: %p, op->user_ptr (ctl): %p, return code from ae_op_complete(op): %d\n", consec_count, &b->op, b->op.user_ptr, normal_completion);
        consec_count++;
        assert(normal_completion);
        ae_op_execute(&b->op, int, 0);
        free(b);
   }
   request = request | more;

   b = poll_list(&list3, &more);
   if(b)
   {
	*(b->value) += 3;
#if 0
        normal_completion = ae_op_complete(&b->op);
        assert(normal_completion);
#endif
        ae_op_execute(&b->op, int, 0);
        free(b);
   }
   request = request | more;


   b = poll_list(&list_fail10, &more);
   if(b)
   {
	*(b->value) += 1;
        if(*(b->value) >= 10)
        {
            printf("BTEST_FAIL10 returning success.\n");
#if 0
            normal_completion = ae_op_complete(&b->op);
            assert(normal_completion);
#endif
            ae_op_execute(&b->op, int, 0);
            free(b);
        }
        else
        {
            printf("BTEST_FAIL10 returning EAGAIN.\n");
#if 0
            normal_completion = ae_op_complete(&b->op);
            assert(normal_completion);
#endif
            ae_op_execute(&b->op, int, -EAGAIN);
            free(b);
        }
   }
   request = request | more;

   b = poll_list(&clist, &more);
   if(b)
   {
#if 0
        normal_completion = ae_op_complete(&b->op);
        assert(normal_completion);
#endif
        ae_op_execute(&b->op, int, -1);
        free(b);
   }
   request = request | more;

   b = poll_list(&rlist, &more);
   if(b)
   {
#if 0
       normal_completion = ae_op_complete(&b->op);
       assert(normal_completion);
#endif
       ae_op_execute(&b->op, int, random());
       free(b);
   }
   request = request | more;

   s = poll_sleep_list(&more);
   if(s)
   {
	sleep(s->sleep);
#if 0
        normal_completion = ae_op_complete(&s->op);
        assert(normal_completion);
#endif
        ae_op_execute(&s->op, int, 0);
        free(s);
   }
   request = request | more;

   s = poll_rsleep_list(&more);
   if(s)
   {
	usleep(s->sleep);
#if 0
        normal_completion = ae_op_complete(&s->op);
        assert(normal_completion);
#endif
        ae_op_execute(&s->op, int, 0);
        free(s);
   }
   request = request | more;

   if(request)
   {
       ae_resource_request_poll(btest_resource_id);
   }
   return AE_SUCCESS;
}

static int btest_cancel(ae_op_id_t op_id)
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
            ae_resource_request_poll(btest_resource_id);
	}
   }

   return AE_SUCCESS;
}     

static int btest_cancel_immed(ae_op_id_t op_id)
{
   struct ae_op *t, *tmp;
   struct btest_op *b;
#if 0
   int normal_completion;
#endif

   /* NOTE: This is known not to work. Aesop no longer allows cancel to 
    * invoke callbacks directly
    */
   assert(0);

   ae_ops_for_each(t, tmp, &flist)
   {
	b = ae_op_entry(t, struct btest_op, op);
	if(ae_op_id_equal(b->id, op_id))
	{
	    ae_ops_del(t);
	    printf("forever op cancelled\n");
#if 0
            normal_completion = ae_op_complete(&b->op);
            assert(normal_completion);
#endif
            ae_op_execute(&b->op, int, -1);
            free(b);
	}
   }

   return AE_SUCCESS;
}     

struct ae_resource btest_resource =
{
    .resource_name = RESOURCE_NAME,
    .poll = btest_poll,
    .cancel = btest_cancel,
};

int btest_init(void)
{
    ae_resource_register(&btest_resource, &btest_resource_id);

    ae_ops_init(&list1);
    ae_ops_init(&list2);
    ae_ops_init(&list3);
    ae_ops_init(&list_consec);
    ae_ops_init(&list_fail10);
    ae_ops_init(&slist);
    ae_ops_init(&srlist);
    ae_ops_init(&clist);
    ae_ops_init(&flist);
    ae_ops_init(&rlist);

    return AE_SUCCESS;
}

void btest_finalize(void)
{
    ae_resource_unregister(btest_resource_id);
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
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
