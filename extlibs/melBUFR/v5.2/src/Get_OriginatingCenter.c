/*
 * Get_OriginatingCenter  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

char* Get_OriginatingCenter (void)

#else

char* Get_OriginatingCenter ()

#endif
{
   int num;
   char* name;
   Table0_t te;

   num = BUFR_Msg.Info.OriginatingCenter;
   te = Table0_Value(num);
   name = (char*)malloc(sizeof(char) * (strlen(te.name) + 1) ); 
   strcpy(name, te.name);
   return (name);
}
