/*
 * Get_Year  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_Year (void)

#else

int Get_Year ()

#endif
{
   int num;

   num = BUFR_Msg.Info.Year;

   return (num);
}
