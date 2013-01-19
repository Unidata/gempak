/*
 * Get_Century  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_Century (void)

#else

int Get_Century ()

#endif
{
   int num;

   num = BUFR_Msg.Info.Century;

   return (num);
}
