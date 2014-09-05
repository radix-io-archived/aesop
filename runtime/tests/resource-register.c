/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */
#include <assert.h>
#include <aesop/aesop.h>

#include "resource.h"

struct ae_resource rsc1 = 
{
    .resource_name = "rsc1",
    .poll = NULL,
    .cancel = NULL,
};

struct ae_resource rsc2 = 
{
    .resource_name = "rsc2",
    .poll = NULL,
    .cancel = NULL,
};

int main(int argc, char **argv)
{
    int ret;
    int rsc1_id;
    int rsc2_id;

    ret = ae_resource_register(&rsc1, &rsc1_id);
    assert(ret == 0);

    ret = ae_resource_register(&rsc2, &rsc2_id);
    assert(ret == 0);

    ae_resource_unregister(rsc1_id);

    ae_resource_unregister(rsc2_id);

    return 0;
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
