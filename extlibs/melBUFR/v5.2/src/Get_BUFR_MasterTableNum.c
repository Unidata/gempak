/*
 * Get_BUFR_MasterTableNum  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_BUFR_MasterTableNum(void)

#else

int Get_BUFR_MasterTableNum ()

#endif
{
   int ver;

   ver = BUFR_Msg.Info.BUFR_MasterTable;

   return (ver);
}
