
#include "src/aesop/aesop.h"
#include "src/common/triton-init.h"

static int initialized = 0;

__attribute__((constructor)) void aesop_init_register(void);

__attribute__((constructor)) void aesop_init_register(void)
{
    triton_init_register("aesop.control", aesop_init, aesop_finalize, NULL, "triton.debug", "aesop.hints");
}

triton_ret_t aesop_init(void)
{
    if(initialized == 0)
    {
        initialized++;
    }
    return TRITON_SUCCESS;
}

void aesop_finalize(void)
{
    initialized--;
}

