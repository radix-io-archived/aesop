
#include "src/aesop/aesop.h"
#include "src/common/triton-init.h"

triton_debug_mask_t ae_debug_blocking_funs;

static int initialized = 0;

__attribute__((constructor)) void aesop_init_register(void);

__attribute__((constructor)) void aesop_init_register(void)
{
    triton_init_register("aesop.control", aesop_init, aesop_finalize, NULL, 2, "triton.debug", "aesop.hints");
}

triton_ret_t aesop_init(void)
{
    triton_ret_t ret;
    if(initialized == 0)
    {
        ret = triton_debug_add_mask("aesop.blocking.funs", &ae_debug_blocking_funs);
        initialized++;
        return ret;
    }
    return TRITON_SUCCESS;
}

void aesop_finalize(void)
{
    initialized--;
}
