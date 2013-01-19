/*
 * Get_ObservedDataFlag  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_ObservedDataFlag (void)

#else

int Get_ObservedDataFlag ()

#endif
{
   int num;

   num = BUFR_Msg.Info.ObservedData;

   return (num);
}
