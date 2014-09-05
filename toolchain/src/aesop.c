/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#include <string.h>

#include "aesop.h"
#include "resource.h"
#include "hints.h"

static triton_mutex_t module_lock = TRITON_MUTEX_INITIALIZER;
static int module_refcount = 0;

int aesop_init(void)
{
    int ret;

    triton_mutex_lock(&module_lock);
    if(!module_refcount)
    {
        aesop_debug_from_env ();

        ret = ae_hints_component_init();
        if(ret < 0)
        {
            return(ret);
        }
    }
    module_refcount++;
    triton_mutex_unlock(&module_lock);

    return AE_SUCCESS;
}

void aesop_finalize(void)
{
    int count;

    triton_mutex_lock(&module_lock);
    module_refcount--;

    if(!module_refcount)
    {
        count = ae_lone_pbranches_count ();
        if (count)
        {
            fprintf (stderr, "\n\n!!!!WARNING!!!!!\n"
                 "Still %i lone pbranches active!\n\n", count);
        }

        ae_hints_component_finalize();

        ae_resource_cleanup ();
    }
    triton_mutex_unlock(&module_lock);
}


/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
