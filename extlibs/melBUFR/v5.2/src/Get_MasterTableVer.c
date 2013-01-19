/*
 * Get_MasterTableVer  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_MasterTableVer (void)

#else

int Get_MasterTableVer ()

#endif
{
   int ver;

   ver = BUFR_Msg.Info.VersionNumberOfMasterTables;

   return (ver);
}
