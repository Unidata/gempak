/*
 * Get_Hour  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_Hour (void)

#else

int Get_Hour ()

#endif
{
   int num;

   num = BUFR_Msg.Info.Hour;

   return (num);
}
