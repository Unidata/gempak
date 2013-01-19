/*
 * Get_Val_Scale  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Get_Val_Scale (BUFR_Val_t BV)

#else

int Get_Val_Scale (BV)
BUFR_Val_t BV;

#endif
{

   return(BV.Val_Scale);

}
