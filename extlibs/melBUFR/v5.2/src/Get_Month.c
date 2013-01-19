/*
 * Get_Month  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_Month (void)

#else

int Get_Month ()

#endif
{
   int num;

   num = BUFR_Msg.Info.Month;

   return (num);
}
