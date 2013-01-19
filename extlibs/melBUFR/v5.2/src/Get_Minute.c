/*
 * Get_Minute  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_Minute (void)

#else

int Get_Minute ()

#endif
{
   int num;

   num = BUFR_Msg.Info.Minute;

   return (num);
}
