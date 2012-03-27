found something!
/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */


#include "ae-error.h"
#include "hints.h"
#include "triton-hash.h"
#include "jenkins-hash.h"
#include "triton-list.h"

/**
 * hint info structure for static hint definitions.
 */
struct ae_hint_info
{
    char * key;
    uint32_t type;
    int flags;
    struct triton_hash_link key_link;
    struct triton_hash_link type_link;
};

struct ae_hint_entry
{
    uint32_t type;
    int flags;
    char *key;
    void *value;
    int length;
    struct triton_hash_link link;
};

static struct triton_hash_table *hints_type_key_table = NULL;
static struct triton_hash_table *hints_type_table = NULL;

static int hints_key_compare(const void *key, struct triton_hash_link *link)
{
    struct ae_hint_info *info = triton_hash_get_entry(link, struct ae_hint_info, key_link);
    return !strcmp(key, info->key);
}

static int hints_key_hash(const void *key, int table_size)
{
    uint32_t pc = 0, pb = 0;
    bj_hashlittle2(key, strlen(key), &pc, &pb);
    return pc & ( table_size - 1 );
}

static int hints_type_compare(const void *t, struct triton_hash_link *link)
{
    struct ae_hint_info *info = triton_hash_get_entry(link, struct ae_hint_info, type_link);
    const int *type = (const int *)t;
    return (*type == info->type);
}

static int hints_type_hash(const void *t, int table_size)
{
    const uint32_t *type = (const uint32_t *)t;
    return (*type) & (table_size - 1);
}

static void ae_hint_info_destroy(void * hi)
{
    struct ae_hint_info *info = (struct ae_hint_info *)hi;

    free(info->key);
    free(info);
}

int ae_hints_component_init(void)
{
    if(hints_type_key_table != NULL)
    {
        return(0);
    }

    hints_type_key_table = triton_hash_init(hints_key_compare, hints_key_hash, bj_hashsize(10));
    if(hints_type_key_table == NULL)
    {
        return AE_ERR_SYSTEM;
    }

    assert(hints_type_table == NULL);
    hints_type_table = triton_hash_init(hints_type_compare, hints_type_hash, bj_hashsize(10));
    if(hints_type_table == NULL)
    {
        triton_hash_destroy_and_finalize(hints_type_key_table, struct ae_hint_info, key_link, ae_hint_info_destroy);
        return AE_ERR_SYSTEM;
    }
    return 0;
}

void ae_hints_component_finalize(void)
{
    assert(hints_type_key_table != NULL);
    triton_hash_destroy_and_finalize(
        hints_type_key_table, struct ae_hint_info, key_link, ae_hint_info_destroy);
    hints_type_key_table = NULL;

    assert(hints_type_table != NULL);
    triton_hash_finalize(hints_type_table);
    hints_type_table = NULL;
}

int ae_hints_type_register(
    const char *key,
    int flags)
{
    int ret;
    struct ae_hint_info *newtype;
    uint32_t pc = 0, pb = 0;

    if(hints_type_key_table == NULL)
    {
        ret = ae_hints_component_init();
        if(ret != 0)
        {
            return ret;
        }
    }

    newtype = malloc(sizeof(*newtype));
    if(!newtype)
    {
        return AE_ERR_SYSTEM;
    }

    newtype->key = strdup(key);
    bj_hashlittle2(newtype->key, strlen(newtype->key), &pc, &pb);
    newtype->type = pc;
    newtype->flags = flags;

    triton_list_link_clear(&newtype->key_link);
    triton_hash_add(hints_type_key_table, newtype->key, &newtype->key_link);

    triton_list_link_clear(&newtype->type_link);
    triton_hash_add(hints_type_table, &newtype->type, &newtype->type_link);
    
    return 0;
}

static inline struct ae_hint_info *ae_hints_get_info_by_key(const char *key)
{
    struct triton_hash_link *link;
    link = triton_hash_search(hints_type_key_table, key);
    if(link)
    {
        return triton_hash_get_entry(link, struct ae_hint_info, key_link);
    }
    return NULL;
}

static inline struct ae_hint_info *ae_hints_get_info_by_type(int type)
{
    struct triton_hash_link *link;
    link = triton_hash_search(hints_type_table, &type);
    if(link)
    {
        return triton_hash_get_entry(link, struct ae_hint_info, type_link);
    }
    return NULL;
}

static int make_new_hint(
    struct ae_hint_entry **new_hint,
    int length,
    void *value,
    uint32_t type,
    int flags,
    int copy_value)
{
    struct ae_hint_entry *h;

    h = malloc(sizeof(*h));
    if (!h)
    {
        return AE_ERR_SYSTEM;
    }

    h->length = length;

    if(copy_value)
    {
        h->value = malloc(h->length);
        if(!h->value)
        {
            free(h);
            return AE_ERR_SYSTEM;
        }

        memcpy(h->value, value, length);
    }
    else
    {
        h->value = value;
    }

    h->type = type;
    h->flags = flags;
    triton_list_link_clear(&h->link);
    *new_hint = h;
    return 0;
}

int ae_hints_init(ae_hints_t *hints)
{
    triton_list_init(&hints->entries);
    hints->parent = NULL;
    triton_mutex_init(&hints->lock, NULL);
    hints->refcount = 1;
    hints->needs_free = 0;
    return 0;
}

static void delete_hint(struct ae_hint_entry *h)
{
    if(h->value) free(h->value);
    free(h);
}

int ae_hints_del(ae_hints_t *hints, const char *key)
{
    if (!hints)
       return AE_ERR_INVALID;

    struct ae_hint_info *info;
    struct ae_hint_entry *entry, *tmpentry;
    int ret;

    info = ae_hints_get_info_by_key(key);
    if(!info)
    {
        return AE_ERR_NOT_FOUND;
    }

    triton_mutex_lock(&hints->lock);

    triton_list_for_each_entry(entry, tmpentry, &hints->entries, struct ae_hint_entry, link)
    {
        if(entry->type == info->type)
        {
            triton_list_del(&entry->link);
            if(entry->flags & AE_HINT_TRANSFER_FLAG)
            {
                hints->transfer_count--;
            }
            delete_hint(entry);
            triton_mutex_unlock(&hints->lock);
            return 0;
        }
    }
    triton_mutex_unlock(&hints->lock);
    return AE_ERR_NOT_FOUND;
}

int ae_hints_modify(ae_hints_t * hints,
                          const char *key,
                          int length,
                          void *value)
{
    if (!hints)
       return AE_ERR_INVALID;

    struct ae_hint_info *info;
    struct ae_hint_entry *entry, *tmpentry;
    int ret;

    info = ae_hints_get_info_by_key(key);
    if(!info)
    {
        return AE_ERR_NOT_FOUND;
    }

    while(hints)
    {
        triton_mutex_lock(&hints->lock);
        triton_list_for_each_entry(entry, tmpentry, &hints->entries, struct ae_hint_entry, link)
        {
            if(entry->type == info->type)
            {
                if(length != entry->length)
                {
                    triton_mutex_unlock(&hints->lock);
                    return AE_ERR_INVALID;
                }
                memcpy(entry->value, value, entry->length);
                triton_mutex_unlock(&hints->lock);
                return 0;
            }
        }
        triton_mutex_unlock(&hints->lock);
        hints = hints->parent;
    }
    return AE_ERR_NOT_FOUND;
}

static ae_hints_t * alloc_init_hint ()
{
   ae_hints_t * h = malloc(sizeof(*h));
   int ret;

   if(!h) return 0;

   ret = ae_hints_init(h);
   if(ret != 0)
   {
      free(h);
      return 0;
   }
   return h;
}

int ae_hints_put(ae_hints_t ** h,
                          const char *key,
                          int length,
                          void *value,
                          int overwrite)
{
    ae_hints_t * hints;
    struct ae_hint_info *info;
    struct ae_hint_entry *entry, *tmpentry;
    int ret;

    /**
     * If the hint structure wasn't allocated yet, do so now.
     */
    if (!*h)
    {
       *h = alloc_init_hint ();
    }

    hints = *h;

    info = ae_hints_get_info_by_key(key);
    if(!info)
    {
        return AE_ERR_NOT_FOUND;
    }

    triton_mutex_lock(&hints->lock);

    ret = AE_ERR_EXIST;
    triton_list_for_each_entry(entry, tmpentry, &hints->entries, struct ae_hint_entry, link)
    {
        if(entry->type == info->type)
        {
            if(overwrite)
            {
                free(entry->value);
                entry->value = malloc(length);
                memcpy(value, entry->value, length);
                entry->length = length;
                ret = 0;
            }
            triton_mutex_unlock(&hints->lock);
            return ret;
        }
    }

    ret = make_new_hint(&entry, length, value, info->type, info->flags, 1);
    if(ret != 0)
    {
        triton_mutex_unlock(&hints->lock);
        return ret;
    }

    triton_queue_enqueue(&entry->link, &hints->entries);
    if(entry->flags & AE_HINT_TRANSFER_FLAG)
    {
        hints->transfer_count++;
    }

    triton_mutex_unlock(&hints->lock);
    return 0;
}

int ae_hints_get(ae_hints_t *hints,
                          const char *key,
                          int length,
                          void *value)
{
    struct ae_hint_entry *entry, *tmpentry;
    struct ae_hint_info *info;
    ae_hints_t *p;

    assert(hints);

    info = ae_hints_get_info_by_key(key);
    if(!info)
    {
        return AE_ERR_NOT_FOUND;
    }

    while(hints)
    {
        triton_mutex_lock(&hints->lock);
        triton_list_for_each_entry(entry, tmpentry, &hints->entries, struct ae_hint_entry, link)
        {
            if(entry->type == info->type)
            {
                memcpy(value, entry->value, entry->length);
                triton_mutex_unlock(&hints->lock);
                return 0;
            }
        }
        triton_mutex_unlock(&hints->lock);
        hints = hints->parent;
    }
    return AE_ERR_NOT_FOUND;
}

void ae_hints_destroy(ae_hints_t *hints)
{
    struct ae_hint_entry *entry, *tmpentry;
    int r;

    triton_mutex_lock(&hints->lock);
    assert(hints->refcount > 0);
    r = --hints->refcount;
    if(r == 0)
    {
        triton_list_for_each_entry(entry, tmpentry, &hints->entries, struct ae_hint_entry, link)
        {
            triton_list_del(&entry->link);
            if(entry->flags & AE_HINT_TRANSFER_FLAG)
            {
                hints->transfer_count--;
            }
            delete_hint(entry);
        }
    }
    triton_mutex_unlock(&hints->lock);
    if(hints->parent) ae_hints_destroy(hints->parent);
    hints->parent = NULL;

    if(r == 0 && hints->needs_free) free(hints);
}

int ae_hints_dup(ae_hints_t *oldh, ae_hints_t ** newh)
{
    if (!oldh)
    {
       *newh = oldh;
       return 0;
    }

    struct ae_hints *h;
    int ret;

    if (!oldh)
    {
       *newh = oldh;
       return 0;
    }

    /* ae_hints_copy will allocate h if needed */
    h = 0;
    ret = ae_hints_copy(oldh, &h);
    if(ret != 0)
    {
        free(h);
        return ret;
    }

    if (h)
       h->needs_free = 1;

    *newh = h;
    return 0;
}

int ae_hints_copy(ae_hints_t *oldh, ae_hints_t ** newh)
{
    if (!oldh)
       return 0;

    if (!*newh)
    {
       *newh = alloc_init_hint ();
    }

    ae_hints_init(*newh);

    triton_mutex_lock(&oldh->lock);
    oldh->refcount++;
    assert (!(*newh)->parent); /* make sure we have correct refcounts */
    (*newh)->parent = oldh;
    triton_mutex_unlock(&oldh->lock);

    return 0;
}

int ae_hints_clone(ae_hints_t *oldh, ae_hints_t ** pnh)
{
    if (!oldh)
       return 0;

    if (!*pnh)
    {
       *pnh = alloc_init_hint ();
    }

    ae_hints_t * newh = *pnh;
    int ret;
    ae_hints_t *tmp;
    struct ae_hint_entry *nh, *entry, *tmpentry;

    ret = ae_hints_init(newh);
    if(ret != 0)
    {
        return ret;
    }

    /* for safety, lock old hint while copying hints */
    triton_mutex_lock(&oldh->lock);
    tmp = oldh;
    while(tmp)
    {

        triton_list_for_each_entry(entry, tmpentry, &tmp->entries, struct ae_hint_entry, link)
        {
            ret = make_new_hint(&nh, entry->length, entry->value, entry->type, entry->flags, 1);
            if(ret != 0)
            {
                goto error;
            }
            triton_queue_enqueue(&nh->link, &newh->entries);
            if(entry->flags & AE_HINT_TRANSFER_FLAG)
            {
                newh->transfer_count++;
            }
        }
        tmp = tmp->parent;
    }

    goto done;
error:
    ae_hints_destroy(newh);
done:
    triton_mutex_unlock(&oldh->lock);
    return ret;
}

int ae_hints_check(ae_hints_t *hints, int type)
{
    struct ae_hint_entry *entry, *tmpentry;

    triton_mutex_lock(&hints->lock);
    triton_list_for_each_entry(entry, tmpentry, &hints->entries, struct ae_hint_entry, link)
    {
        if(entry->type == type)
        {
            triton_mutex_unlock(&hints->lock);
            return AE_ERR_EXIST;
        }
    }
    triton_mutex_unlock(&hints->lock);
    if(hints->parent) return ae_hints_check(hints->parent, type);
}

static uint32_t count_transferable_hints(struct ae_hints *h)
{
    uint64_t count = 0;
    struct ae_hints_entry *entry, *tmpentry;

    while(h)
    {
        count += h->transfer_count;
        h = h->parent;
    }
    return count;
}

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
