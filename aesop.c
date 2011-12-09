
#include "src/aesop/aesop.h"
#include "src/common/triton-init.h"

triton_debug_mask_t ae_debug_blocking_funs;
triton_debug_mask_t ae_debug_pbranch_state;
extern triton_debug_mask_t aesop_debug_cancel_mask;

static int initialized = 0;

__attribute__((constructor)) void aesop_init_register(void);

__attribute__((constructor)) void aesop_init_register(void)
{
    triton_init_register("aesop.control", aesop_init, aesop_finalize, NULL, "triton.debug", "aesop.hints");
}

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

        if(ret != TRITON_SUCCESS)
        {
            return ret;
        }

        ret = triton_debug_add_mask("aesop.blocking.pbranch", &ae_debug_pbranch_state,
                                    "Show state of pwait upon completion of pbranch");
        initialized++;
        return ret;
    }
    return TRITON_SUCCESS;
}

void aesop_finalize(void)
{
    initialized--;
}

