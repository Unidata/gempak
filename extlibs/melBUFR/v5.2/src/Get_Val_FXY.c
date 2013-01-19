/*
 * Get_Val_FXY  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

FXY_t Get_Val_FXY (BUFR_Val_t BV)

#else

FXY_t Get_Val_FXY (BV)
BUFR_Val_t BV;

#endif
{

   return (BV.FXY_Val);
}
