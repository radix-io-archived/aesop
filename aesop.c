
#include "src/aesop/aesop.h"
#include "src/common/triton-init.h"

triton_debug_mask_t ae_debug_blocking_funs;

static int initialized = 0;

__attribute__((constructor)) void aesop_init_register(void);

__attribute__((constructor)) void aesop_init_register(void)
{
    triton_init_register("aesop.control", aesop_init, aesop_finalize, NULL, "triton.debug", "aesop.hints");
}

extern triton_debug_mask_t aesop_debug_cancel_mask;

triton_ret_t aesop_init(void)
{
    triton_ret_t ret;
    if(initialized == 0)
    {
        ret = triton_debug_add_mask("aesop.blocking.funs", &ae_debug_blocking_funs,
                                    "Enter/Exit points of each aesop __blocking function");
        if(ret != TRITON_SUCCESS)
        {
            return ret;
        }

        ret = triton_debug_add_mask("aesop.blocking.cancel", &aesop_debug_cancel_mask,
                                    "Output when blocking operations are cancelled");

        initialized++;
        return ret;
    }
    return TRITON_SUCCESS;
}

void aesop_finalize(void)
{
    initialized--;
}
