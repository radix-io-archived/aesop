#include <string.h>

#include "src/aesop/aesop.h"
#include "src/aesop/resource.h"
#include "src/common/triton-init.h"
#include "src/aesop/hints.h"

static int initialized = 0;

triton_ret_t aesop_init(const char* resource_list)
{
    triton_ret_t tret;
    int ret;
    char* rsc;
    char* tmp_resource_list;

    tmp_resource_list = strdup(resource_list);
    if(!tmp_resource_list)
    {
        return(TRITON_ERR_NOMEM);
    }

    if(initialized == 0)
    {
        ret = ae_hints_component_init();
        if(ret < 0)
        {
            free(tmp_resource_list);
            return(TRITON_ERR_UNKNOWN);
        }

        if(strlen(resource_list) == 0)
        {
            tret = ae_resource_init_all();
            if(triton_is_error(tret))
            {
                free(tmp_resource_list);
                return(tret);
            }
        }
        else
        {
            for(rsc = strtok(tmp_resource_list, ",");
                rsc != NULL;
                rsc = strtok(NULL, ","))
            {
                tret = ae_resource_init(rsc);
                if(triton_is_error(tret))
                {
                    free(tmp_resource_list);
                    return(tret);
                }
            }
        }
    }
    initialized++;
    free(tmp_resource_list);
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

