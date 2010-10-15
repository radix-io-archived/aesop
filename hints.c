
#include "src/aesop/hints.h"
#include "src/common/triton-hash.h"
#include "src/common/jenkins-hash.h"

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

/**
 * actual hint structure
 */
struct ae_hints
{
    uint32_t type;
    int flags;
    char *key;
    struct aer_encoder *encoding;
    void *value;
    int length;
    struct ae_hints *next;
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

__attribute__((constructor)) void ae_hints_init_register(void);

__attribute__((constructor)) void ae_hints_init_register(void)
{
    triton_init_register("aesop.hints", ae_hints_init, ae_hints_finalize, NULL, 0);
}


triton_ret_t ae_hints_init(void)
{
    assert(hints_type_key_table == NULL);
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

void ae_hints_finalize(void)
{
    assert(hints_type_key_table != NULL);
    triton_hash_destroy_and_finalize(hints_type_key_table, struct ae_hint_info, key_link, ae_hint_info_destroy);
    hints_type_key_table = NULL;

    assert(hints_type_table != NULL);
    triton_hash_finalize(hints_type_table);
    hints_type_table = NULL;
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
        ret = ae_hints_init();
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

    triton_list_link_clear(&newtype->key_link);
    triton_hash_add(hints_type_key_table, newtype->key, &newtype->key_link);

    triton_list_link_clear(&newtype->type_link);
    triton_hash_add(hints_type_table, &newtype->type, &newtype->type_link);
    
    return TRITON_SUCCESS;
}

static inline triton_ret_t ae_hints_check(ae_hints_t hints, int type);

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

static triton_ret_t make_new_hint(ae_hints_t *new_hint, int length, void *value, uint32_t type, int flags, struct aer_encoder *encoding)
{
    struct ae_hints *h;

    h = malloc(sizeof(*h));
    if (!h)
    {
        return TRITON_ERR_NOMEM;
    }

    h->length = length;
    h->value = malloc(h->length);
    if(!h->value)
    {
        free(h);
        return TRITON_ERR_NOMEM;
    }

    memcpy(h->value, value, length);

    h->type = type;
    h->flags = flags;
    h->encoding = encoding;

    *new_hint = h;
    return TRITON_SUCCESS;
}

triton_ret_t ae_hints_put(ae_hints_t *hints,
                          const char *key,
                          int length,
                          void *value)
{
    struct ae_hint_info *info;
    triton_ret_t ret;
    struct ae_hints *new_hint;

    info = ae_hints_get_info_by_key(key);
    if(!info)
    {
        return TRITON_ERR_HINT_MISSING_TYPE;
    }
    else
    {
        ret = ae_hints_check(*hints, info->type);
        if(ret == TRITON_ERR_EXIST)
        {
            return ret;
        }
    }

    ret = make_new_hint(&new_hint, length, value, info->type, info->flags, info->encoding);
    if(ret != TRITON_SUCCESS)
    {
        return ret;
    }
    new_hint->next = *hints;
    *hints = new_hint;

    return TRITON_SUCCESS;
}

triton_ret_t ae_hints_get(ae_hints_t hints,
                          const char *key,
                          int length,
                          void *value)
{
    struct ae_hints *tmp;
    struct ae_hint_info *info;

    if(!hints) return TRITON_SUCCESS;

    info = ae_hints_get_info_by_key(key);
    if(!info)
    {
        return TRITON_ERR_HINT_MISSING_TYPE;
    }

    tmp = hints;
    while(tmp)
    {
        if(tmp->type == info->type)
        {
            memcpy(value, tmp->value, tmp->length);
            return TRITON_SUCCESS;
        }
        tmp = tmp->next;
    }
    return TRITON_SUCCESS;

}

void ae_hints_destroy(ae_hints_t h)
{
    struct ae_hints *tmp, *next;

    next = h;
    while(next)
    {
        tmp = next;
        next = tmp->next;
        free(tmp);
    }
}

triton_ret_t ae_hints_copy(ae_hints_t oldh, ae_hints_t *newh)
{
    triton_ret_t ret;
    struct ae_hints *tmp, *nh;
    tmp = oldh;
    *newh = NULL;
    if(oldh == NULL) return TRITON_SUCCESS;

    while(tmp)
    {
        ret = make_new_hint(&nh, tmp->length, tmp->value, tmp->type, tmp->flags, tmp->encoding);
        if(ret != TRITON_SUCCESS)
        {
            ae_hints_destroy(*newh);
            return ret;
        }
        nh->next = *newh;
        *newh = nh;
        
        tmp = tmp->next;
    }
    return TRITON_SUCCESS;
}

static inline triton_ret_t ae_hints_check(ae_hints_t hints, int type)
{
    struct ae_hints *tmp;

    if(!hints) return TRITON_SUCCESS;

    tmp = hints;
    while(tmp)
    {
        if(tmp->type == type)
        {
            return TRITON_ERR_EXIST;
        }
        tmp = tmp->next;
    }
    return TRITON_SUCCESS;
}

uint64_t aer_encode_size_ae_hints_t(
    const char *n __unused, void *x)
{
    struct ae_hints *h = *(struct ae_hints **)x;
    uint64_t count = 0;

    while(h)
    {
        if(h->flags & AE_HINT_TRANSFER_FLAG)
        {
            count += aer_encode_size_int32_t(NULL, &h->type);
            count += aer_encode_size_int32_t(NULL, &h->flags);
            count += h->encoding->encode_size(NULL, h->value);
        }
        h = h->next;
    }
    count += 8; 

    return count;
}

triton_ret_t aer_encode_ae_hints_t(
    triton_buffer_t *buf, const char *n __unused, void *x)
{
    triton_ret_t ret;
    int null_type = 0;
    struct ae_hints *h = *(struct ae_hints **)x;
    assert(buf);
    assert(x);

    while(h)
    {
        if(h->flags & AE_HINT_TRANSFER_FLAG)
        {
            ret = aer_encode_int32_t(buf, "type", &h->type);
            if(ret != TRITON_SUCCESS)
            {
                return ret;
            }

            ret = aer_encode_int32_t(buf, "flags", &h->flags);
            if(ret != TRITON_SUCCESS)
            {
                return ret;
            }

            ret = h->encoding->encode(buf, NULL, h->value);
            if(ret != TRITON_SUCCESS)
            {
                return ret;
            }
        }

        h = h->next;
    }

    ret = h->encoding->encode(buf, NULL, &null_type);
    return TRITON_SUCCESS;
}

triton_ret_t aer_decode_ae_hints_t(
    triton_buffer_t *buf, char **n __unused, void *x)
{
    triton_ret_t ret;
    struct ae_hint_info *info;

    uint32_t count;
    struct ae_hints *h, *tmp;
    uint32_t type;
    assert(buf);
    assert(x);

    tmp = NULL;

    while(1)
    {
        ret = aer_decode_int32_t(buf, NULL, &type);
        if(ret != TRITON_SUCCESS)
        {
            ae_hints_destroy(tmp);
            return ret;
        }

        if(type == 0)
        {
            break;
        }

        info = ae_hints_get_info_by_type(type);
        if(info == NULL)
        {
            ae_hints_destroy(tmp);
            return TRITON_ERR_HINT_MISSING_TYPE;
        }

        h = malloc(sizeof(*h));
        if(!h)
        {
            ae_hints_destroy(tmp);
            return TRITON_ERR_NOMEM;
        }
        h->type = type;
        h->key = strdup(info->key);

        ret = aer_decode_int32_t(buf, NULL, &h->flags);
        if(ret != TRITON_SUCCESS)
        {
            return ret;
        }

        ret = h->encoding->decode(buf, NULL, h->value);
        if(ret != TRITON_SUCCESS)
        {
            return ret;
        }

        h->next = tmp;
        tmp = h;
    }
        
    *((struct ae_hints **)x) = h;
    return TRITON_SUCCESS;
}

triton_ret_t aer_init_ae_hints_t(void *x)
{
    return TRITON_SUCCESS;
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
    .init = aer_init_ae_hints_t,
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
