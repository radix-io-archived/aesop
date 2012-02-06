#include "ae-init.h"

__attribute__((constructor)) void triton_timer_init_register(void);
__attribute__((constructor)) void triton_aethread_init_register(void);

int ae_init (void)
{
   /* Call the constructor functions here */
   triton_timer_init_register ();
   triton_aethread_init_register ();
}
