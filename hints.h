#ifndef __AE_HINTS_H__
#define __AE_HINTS_H__

#include "src/common/triton-base.h"
#include "src/common/triton-types.h"

typedef struct ae_hints *ae_hints_t;

#define AE_HINT_TRANSFER_FLAG 0x1

#include "src/common/triton-error.h"

#ifdef AESOP_PARSER
#define aesop_hints_get(__key, __length, __value) ae_hints_get(ctl->gen.hints, __key, __length, __value)
#define aesop_hints_put(__key, __length, __value) ae_hints_put(&ctl->gen.hints, __key, __length, __value)
#else

/* dummy functions to allow for compiling */
static inline triton_ret_t aesop_hints_get(const char *key, int length, void *value) { return TRITON_ERR_NOSYS; }
static inline triton_ret_t aesop_hints_put(const char *key, int length, void *value) { return TRITON_ERR_NOSYS; }
#endif

triton_ret_t ae_hints_init(void);
void ae_hints_finalize(void);

triton_ret_t ae_hints_put(ae_hints_t *hints,
                          const char *key,
                          int length,
                          void *value);

triton_ret_t ae_hints_get(ae_hints_t hints,
                          const char *key,
                          int length,
                          void *value);

triton_ret_t ae_hints_copy(ae_hints_t oldh, ae_hints_t *newh);

void ae_hints_destroy(ae_hints_t h);

#include "src/common/triton-buffer.h"

uint64_t aer_encode_size_ae_hints_t(const char *n, void *x);
triton_ret_t aer_encode_ae_hints_t(triton_buffer_t *buf, const char *n, void *x);
triton_ret_t aer_decode_ae_hints_t(triton_buffer_t *buf, char **n __unused, void *x);
triton_ret_t aer_init_null_ae_hints_t(void *x);
triton_ret_t aer_copy_ae_hints_t(void *x, void *y);
void aer_destroy_ae_hints_t(void *x);
/* TODO: fix hints encoding/decoding */

#include "src/remote/encoding.h"

triton_ret_t ae_hints_type_register(const char *key, int flags, struct aer_encoder *encoding);

extern struct aer_encoder aer_encoder_ae_hints_t;

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
