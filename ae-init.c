/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#include "ae-init.h"

__attribute__((constructor)) void aesop_timer_init_register(void);
__attribute__((constructor)) void triton_aethread_init_register(void);

int ae_init (void)
{
   /* Call the constructor functions here */
   aesop_timer_init_register ();
   triton_aethread_init_register ();

   return(0);
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
