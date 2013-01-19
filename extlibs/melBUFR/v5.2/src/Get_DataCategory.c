/*
 * Get_DataCategory  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_DataCategory (void)

#else

int Get_DataCategory ()

#endif
{
   int num;

   num = BUFR_Msg.Info.DataCategory;

   return (num);
}
