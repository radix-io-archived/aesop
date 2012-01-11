#include "ae-init.h"

__attribute__((constructor)) void triton_timer_init_register(void);


int ae_init (void)
{
   /* Call the constructor functions here */
   triton_timer_init_register ();
}
