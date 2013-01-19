/*
 * Get_LocalTableVer  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_LocalTableVer (void)

#else

int Get_LocalTableVer ()

#endif
{
   int num;

   num = BUFR_Msg.Info.VersionNumberOfLocalTables;

   return (num);
}
