/*
 * Get_BUFR_Edition  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_BUFR_Edition (void)

#else

int Get_BUFR_Edition ()

#endif
{
   int edition;

   edition = BUFR_Msg.Info.BUFR_Edition;

   return (edition);
}
