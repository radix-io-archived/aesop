
#include "src/aesop/aesop.h"
#include "src/common/triton-init.h"
#include "src/aesop/hints.h"

static int initialized = 0;

__attribute__((constructor)) void aesop_init_register(void);

__attribute__((constructor)) void aesop_init_register(void)
{
    triton_init_register("aesop.control", aesop_init, aesop_finalize, NULL);
}

triton_ret_t aesop_init(void)
{
    triton_ret_t tret;

    if(initialized == 0)
    {
        tret = ae_hints_component_init();
        if(triton_is_error(tret))
        {
            return(tret);
        }
    }
    initialized++;
    return TRITON_SUCCESS;
}

void aesop_finalize(void)
{
    initialized--;
    if(initialized == 0)
    {
        ae_hints_component_finalize();
    }

    return;
}

