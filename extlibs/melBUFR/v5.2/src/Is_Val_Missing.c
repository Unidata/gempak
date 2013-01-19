/*
 * Is_Val_Missing  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Is_Val_Missing (BUFR_Val_t BV)

#else

int Is_Val_Missing (BV)
BUFR_Val_t BV;

#endif
{
   int flag;

   flag = BV.missing_flag;

   return (flag);
}
