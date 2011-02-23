
#include "src/aesop/hints.h"
#include "src/common/triton-hash.h"
#include "src/common/jenkins-hash.h"
#include "src/common/triton-list.h"

/**
 * hint info structure for static hint definitions.
 */
struct ae_hint_info
{
    char * key;
    uint32_t type;
    int flags;
    struct aer_encoder *encoding;
    struct triton_hash_link key_link;
    struct triton_hash_link type_link;
};

struct ae_hint_entry
{
    uint32_t type;
    int flags;
    char *key;
    struct aer_encoder *encoding;
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

#include "src/common/triton-init.h"

static triton_ret_t ae_hints_component_init(void)
{
    if(hints_type_key_table != NULL)
    {
        return(TRITON_SUCCESS);
    }

    hints_type_key_table = triton_hash_init(hints_key_compare, hints_key_hash, bj_hashsize(10));
    if(hints_type_key_table == NULL)
    {
        return TRITON_ERR_NOMEM;
    }

    assert(hints_type_table == NULL);
    hints_type_table = triton_hash_init(hints_type_compare, hints_type_hash, bj_hashsize(10));
    if(hints_type_table == NULL)
    {
        triton_hash_destroy_and_finalize(hints_type_key_table, struct ae_hint_info, key_link, ae_hint_info_destroy);
        return TRITON_ERR_NOMEM;
    }
    return TRITON_SUCCESS;
}

static void ae_hints_component_finalize(void)
{
    assert(hints_type_key_table != NULL);
    triton_hash_destroy_and_finalize(
        hints_type_key_table, struct ae_hint_info, key_link, ae_hint_info_destroy);
    hints_type_key_table = NULL;

    assert(hints_type_table != NULL);
    triton_hash_finalize(hints_type_table);
    hints_type_table = NULL;
}

__attribute__((constructor)) void ae_hints_init_register(void);

__attribute__((constructor)) void ae_hints_init_register(void)
{
    triton_init_register("aesop.hints", ae_hints_component_init, ae_hints_component_finalize, NULL);
}


triton_ret_t ae_hints_type_register(
    const char *key,
    int flags,
    struct aer_encoder *encoder)
{
    triton_ret_t ret;
    struct ae_hint_info *newtype;
    uint32_t pc = 0, pb = 0;

    if(hints_type_key_table == NULL)
    {
        ret = ae_hints_component_init();
        if(ret != TRITON_SUCCESS)
        {
            return ret;
        }
    }

    newtype = malloc(sizeof(*newtype));
    if(!newtype)
    {
        return TRITON_ERR_NOMEM;
    }

    newtype->key = strdup(key);
    bj_hashlittle2(newtype->key, strlen(newtype->key), &pc, &pb);
    newtype->type = pc;
    newtype->encoding = encoder;
    newtype->flags = flags;

    triton_list_link_clear(&newtype->key_link);
    triton_hash_add(hints_type_key_table, newtype->key, &newtype->key_link);

    triton_list_link_clear(&newtype->type_link);
    triton_hash_add(hints_type_table, &newtype->type, &newtype->type_link);
    
    return TRITON_SUCCESS;
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

static triton_ret_t make_new_hint(
    struct ae_hint_entry **new_hint,
    int length,
    void *value,
    uint32_t type,
    int flags,
    struct aer_encoder *encoding,
    int copy_value)
{
    struct ae_hint_entry *h;

    h = malloc(sizeof(*h));
    if (!h)
    {
        return TRITON_ERR_NOMEM;
    }

    h->length = length;

    if(copy_value)
    {
        h->value = malloc(h->length);
        if(!h->value)
        {
            free(h);
            return TRITON_ERR_NOMEM;
        }

        memcpy(h->value, value, length);
    }
    else
    {
        h->value = value;
    }

    h->type = type;
    h->flags = flags;
    h->encoding = encoding;
    triton_list_link_clear(&h->link);
    *new_hint = h;
    return TRITON_SUCCESS;
}

triton_ret_t ae_hints_init(ae_hints_t *hints)
{
    triton_list_init(&hints->entries);
    hints->parent = NULL;
    triton_mutex_init(&hints->lock, NULL);
    hints->refcount = 1;
    hints->needs_free = 0;
    return TRITON_SUCCESS;
}

static void delete_hint(struct ae_hint_entry *h)
{
    if(h->value) free(h->value);
    free(h);
}

triton_ret_t ae_hints_del(ae_hints_t *hints, const char *key)
{
    struct ae_hint_info *info;
    struct ae_hint_entry *entry, *tmpentry;
    triton_ret_t ret;

    info = ae_hints_get_info_by_key(key);
    if(!info)
    {
        return TRITON_ERR_HINT_MISSING_TYPE;
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
            return TRITON_SUCCESS;
        }
    }
    triton_mutex_unlock(&hints->lock);
    return TRITON_ERR_NOENT;
}

triton_ret_t ae_hints_put(ae_hints_t *hints,
                          const char *key,
                          int length,
                          void *value,
                          int overwrite)
{
    struct ae_hint_info *info;
    struct ae_hint_entry *entry, *tmpentry;
    triton_ret_t ret;

    info = ae_hints_get_info_by_key(key);
    if(!info)
    {
        return TRITON_ERR_HINT_MISSING_TYPE;
    }

    triton_mutex_lock(&hints->lock);

    ret = TRITON_ERR_EXIST;
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
                ret = TRITON_SUCCESS;
            }
            triton_mutex_unlock(&hints->lock);
            return ret;
        }
    }

    ret = make_new_hint(&entry, length, value, info->type, info->flags, info->encoding, 1);
    if(ret != TRITON_SUCCESS)
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
    return TRITON_SUCCESS;
}

triton_ret_t ae_hints_get(ae_hints_t *hints,
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
        return TRITON_ERR_HINT_MISSING_TYPE;
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
                return TRITON_SUCCESS;
            }
        }
        triton_mutex_unlock(&hints->lock);
        hints = hints->parent;
    }
    return TRITON_ERR_NOENT;
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

triton_ret_t ae_hints_dup(ae_hints_t *oldh, ae_hints_t **newh)
{
    struct ae_hints *h;
    triton_ret_t ret;

    h = malloc(sizeof(*h));
    if(!h) return TRITON_ERR_NOMEM;

    ret = ae_hints_init(h);
    if(ret != TRITON_SUCCESS)
    {
        free(h);
        return ret;
    }
    ret = ae_hints_copy(oldh, h);
    if(ret != TRITON_SUCCESS)
    {
        free(h);
        return ret;
    }
    h->needs_free = 1;
    *newh = h;
    return TRITON_SUCCESS;
}

triton_ret_t ae_hints_copy(ae_hints_t *oldh, ae_hints_t *newh)
{
    ae_hints_init(newh);

    triton_mutex_lock(&oldh->lock);
    oldh->refcount++;
    newh->parent = oldh;
    triton_mutex_unlock(&oldh->lock);

    return TRITON_SUCCESS;
}

triton_ret_t ae_hints_clone(ae_hints_t *oldh, ae_hints_t *newh)
{
    triton_ret_t ret;
    ae_hints_t *tmp;
    struct ae_hint_entry *nh, *entry, *tmpentry;

    ret = ae_hints_init(newh);
    if(ret != TRITON_SUCCESS)
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
            ret = make_new_hint(&nh, entry->length, entry->value, entry->type, entry->flags, entry->encoding, 1);
            if(ret != TRITON_SUCCESS)
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

triton_ret_t ae_hints_check(ae_hints_t *hints, int type)
{
    struct ae_hint_entry *entry, *tmpentry;

    triton_mutex_lock(&hints->lock);
    triton_list_for_each_entry(entry, tmpentry, &hints->entries, struct ae_hint_entry, link)
    {
        if(entry->type == type)
        {
            triton_mutex_unlock(&hints->lock);
            return TRITON_ERR_EXIST;
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

uint64_t aer_encode_size_ae_hints_t(
    const char *n __unused, void *x)
{
    uint32_t tcount;
    uint64_t count;
    struct ae_hints *h = (struct ae_hints *)x;
    struct ae_hint_entry *entry, *tmpentry;

    tcount = count_transferable_hints(h);
    count = aer_encode_size_uint32_t(NULL, &tcount);

    while(h)
    {
        triton_list_for_each_entry(entry, tmpentry, &h->entries, struct ae_hint_entry, link)
        {
            if(entry->flags & AE_HINT_TRANSFER_FLAG)
            {
                count += aer_encode_size_int32_t(NULL, &entry->type);
                count += aer_encode_size_int32_t(NULL, &entry->flags);
                count += entry->encoding->encode_size(NULL, entry->value);
            }
        }

        h = h->parent;
    }

    return count;
}

triton_ret_t aer_encode_ae_hints_t(
    triton_buffer_t *buf, const char *n __unused, void *x)
{
    uint32_t hcount;
    triton_ret_t ret;
    int null_type = 0;
    struct ae_hints *h = (struct ae_hints *)x;
    struct ae_hint_entry *entry, *tmpentry;

    assert(buf);
    assert(x);

    hcount = count_transferable_hints(h);

    ret = aer_encode_uint32_t(buf, "count", &hcount);
    if(ret != TRITON_SUCCESS)
    {
        return ret;
    }

    while(h)
    {
        triton_list_for_each_entry(entry, tmpentry, &h->entries, struct ae_hint_entry, link)
        {
            if(entry->flags & AE_HINT_TRANSFER_FLAG)
            {
                ret = aer_encode_uint32_t(buf, "type", &entry->type);
                if(ret != TRITON_SUCCESS)
                {
                    return ret;
                }

                ret = aer_encode_int32_t(buf, "flags", &entry->flags);
                if(ret != TRITON_SUCCESS)
                {
                    return ret;
                }

                ret = aer_encode_uint32_t(buf, "length", &entry->length);
                if(ret != TRITON_SUCCESS)
                {
                    return ret;
                }

                ret = entry->encoding->encode(buf, NULL, entry->value);
                if(ret != TRITON_SUCCESS)
                {
                    return ret;
                }
            }
        }

        h = h->parent;
    }

    return TRITON_SUCCESS;
}

triton_ret_t aer_decode_ae_hints_t(
    triton_buffer_t *buf, char **n __unused, void *x)
{
    triton_ret_t ret;
    struct ae_hint_info *info;

    uint32_t hcount, length, type;
    int flags;
    struct ae_hints *h;
    int i;
    void *v;

    assert(buf);
    assert(x);

    h = (struct ae_hints *)x;

    triton_mutex_lock(&h->lock);

    ret = aer_decode_uint32_t(buf, NULL, &hcount);
    if(ret != TRITON_SUCCESS)
    {
        goto error;
    }

    for(i = 0; i < hcount; ++i)
    {
        int found;
        struct ae_hint_entry *entry, *tmpentry;

        ret = aer_decode_int32_t(buf, NULL, &type);
        if(ret != TRITON_SUCCESS)
        {
            goto error;
        }

        ret = aer_decode_int32_t(buf, NULL, &flags);
        if(ret != TRITON_SUCCESS)
        {
            goto error;
        }

        info = ae_hints_get_info_by_type(type);
        if(info == NULL)
        {
            ret = TRITON_ERR_HINT_MISSING_TYPE;
            goto error;
        }

        ret = aer_decode_uint32_t(buf, NULL, &length);
        if(ret != TRITON_SUCCESS)
        {
            goto error;
        }

        v = malloc(length);
        if(v == NULL)
        {
            ret = TRITON_ERR_NOMEM;
            goto error;
        }

        ret = info->encoding->decode(buf, NULL, v);
        if(ret != TRITON_SUCCESS)
        {
            free(v);
            goto error;
        }

        /* if it already exists, replace it with decoded value */
        found = 0;
        triton_list_for_each_entry(entry, tmpentry, &h->entries, struct ae_hint_entry, link)
        {
            if(entry->type == info->type)
            {
                free(entry->value);
                entry->value = v;
                entry->length = length;
                found = 1;
                break;
            }
        }
            
        if(!found)
        {
            struct ae_hint_entry *nh;
            /* otherwise make a new hint and insert at end */
            ret = make_new_hint(&nh, length, v, type, flags, info->encoding, 0);
            if(ret != TRITON_SUCCESS)
            {
                goto error;
            }

            triton_queue_enqueue(&nh->link, &h->entries);
            h->transfer_count++;
        }
    }
        
    goto done;
error:
    triton_mutex_unlock(&h->lock);

done:
    return ret;
}

triton_ret_t aer_init_null_ae_hints_t(void *x)
{
    memset(x, 0, sizeof(struct ae_hints));
    return TRITON_SUCCESS;
}

triton_ret_t aer_copy_ae_hints_t(void *x, void *y)
{
    return ae_hints_copy((ae_hints_t *)x, (ae_hints_t *)y);
}

void aer_destroy_ae_hints_t(void *x)
{
    return;
}

struct aer_encoder aer_encoder_ae_hints_t =
{
    .encode = aer_encode_ae_hints_t,
    .decode = aer_decode_ae_hints_t,
    .encode_size = aer_encode_size_ae_hints_t,
    .init_null = aer_init_null_ae_hints_t,
    .copy = aer_copy_ae_hints_t,
    .destroy = aer_destroy_ae_hints_t
};

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
