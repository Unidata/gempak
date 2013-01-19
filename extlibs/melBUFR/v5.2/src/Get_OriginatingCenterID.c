/*
 * Get_OriginatingCenterID  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_OriginatingCenterID (void)

#else

int Get_OriginatingCenterID ()

#endif
{
   int num;

   num = BUFR_Msg.Info.OriginatingCenter;

   return (num);
}
