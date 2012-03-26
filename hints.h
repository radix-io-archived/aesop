#ifndef __AE_HINTS_H__
#define __AE_HINTS_H__

#include "src/c-utils/triton-base.h"
#include "src/c-utils/triton-list.h"
#include "ae-thread.h"

struct ae_hints
{
    triton_list_t entries;
    int transfer_count;
    struct ae_hints *parent;
    triton_mutex_t lock;
    int refcount;
    int needs_free;
};
typedef struct ae_hints ae_hints_t;

#define AE_HINT_TRANSFER_FLAG 0x1

#ifdef AESOP_PARSER
#define aesop_hints_get(__key, __length, __value) \
    ae_hints_get(__ae_ctl->gen.hints, __key, __length, __value)
#define aesop_hints_put(__key, __length, __value, __overwrite) \
    ae_hints_put(&__ae_ctl->gen.hints, __key, __length, __value, __overwrite)
#define aesop_hints_modify(__key, __length, __value) \
    ae_hints_modify(__ae_ctl->gen.hints, __key, __length, __value)
#define aesop_hints_del(__key) \
    ae_hints_del(__ae_ctl->gen.hints, __key);
#else

/* dummy functions to allow for compiling */
static inline int aesop_hints_get(const char *key, int length, void *value)
{
    return AE_ERR_SYSTEM;
}
static inline int aesop_hints_put(const char *key, int length, void *value, int overwrite)
{
    return AE_ERR_SYSTEM;
}
static inline int aesop_hints_modify(ae_hints_t *hints, const char *key, int length, void *value)
{
    return AE_ERR_SYSTEM;
}
static inline int aesop_hints_del(ae_hints_t *hints, const char *key)
{
    return AE_ERR_SYSTEM;
}
#endif

/**
 * Initialize the hints framework
 */
int ae_hints_component_init(void);

/** 
 * Shut down the hints framework
 */
void ae_hints_component_finalize(void);

/**
 * Put might allocate a hint structure if there was no previous hint allocated
 */
int ae_hints_put(ae_hints_t ** hints,
                          const char *key,
                          int length,
                          void *value,
                          int overwrite);

int ae_hints_get(ae_hints_t *hints,
                          const char *key,
                          int length,
                          void *value);

/* This is different from a put() with overwrite, because it will walk 
 * up to
 * parent hints, and it will return ENOENT if the hint in question is not
 * found.  It is intended for lower level components that need to modify a
 * hint that was injected by a higher level component.
  */
int ae_hints_modify(ae_hints_t * hints,
                          const char *key,
                          int length,
                          void *value);

int ae_hints_del(ae_hints_t *hints,
                          const char *key);


/**
 * Copy all hints by incrementing refcount on old hints and pointing new
 * hints to old.  Hints added to the new hints will not show up on the old hints
 */
int ae_hints_copy(ae_hints_t *oldh, ae_hints_t ** newh);

/**
 * Deep copy of all hints from old to new.  Updates to old or new are not reflected
 * in the other.
 */
int ae_hints_clone(ae_hints_t *oldh, ae_hints_t ** newh);

/**
 * Allocates a pointer for new hints and does a ae_hints_copy.
 */
int ae_hints_dup(ae_hints_t *oldh, ae_hints_t ** newh);

/**
 * Check that hints hold a value for a particular hint type.
 */
int ae_hints_check(ae_hints_t *hints, int type);

/**
 * Initialize members of hint structure.
 */
int ae_hints_init(ae_hints_t *h);

/**
 * Destroy members of hint structure, including all hints.  If
 * the refcount is non-zero, just decrements refcount.  If hints struct
 * has a parent pointer, calls ae_hints_destroy on parent.
 */
void ae_hints_destroy(ae_hints_t *h);

/**
 * Calls destroy on hint structure and then frees pointer.  Companion to ae_hints_dup.
 */
void ae_hints_free(ae_hints_t *h);

int ae_hints_type_register(
    const char *key,
    int flags);

#endif /* __HINTS_H__ */

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
