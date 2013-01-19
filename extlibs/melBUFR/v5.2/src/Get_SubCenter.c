/*
 * Get_SubCenter  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_SubCenter (void)

#else

int Get_SubCenter ()

#endif
{
   int num;

   num = BUFR_Msg.Info.SubCenter;

   return (num);
}
