/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#include <string.h>

#include "aesop.h"
#include "resource.h"
#include "hints.h"
#include "ae-init.h"

static int initialized = 0;

int aesop_init(const char* resource_list)
{
    int ret;
    char* rsc;
    char* tmp_resource_list;

    /* call __constructor__ functions */
    ae_init ();

    tmp_resource_list = strdup(resource_list);
    if(!tmp_resource_list)
    {
        return(AE_ERR_SYSTEM);
    }

    if(initialized == 0)
    {
        ret = ae_hints_component_init();
        if(ret < 0)
        {
            free(tmp_resource_list);
            return(ret);
        }

        if(strlen(resource_list) == 0)
        {
            ret = ae_resource_init_all();
            if(ret < 0)
            {
                free(tmp_resource_list);
                return(ret);
            }
        }
        else
        {
            for(rsc = strtok(tmp_resource_list, ",");
                rsc != NULL;
                rsc = strtok(NULL, ","))
            {
                ret = ae_resource_init(rsc);
                if(ret < 0)
                {
                    free(tmp_resource_list);
                    return(ret);
                }
            }
        }
    }
    initialized++;
    free(tmp_resource_list);
    return AE_SUCCESS;
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


/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
