/*
 * Get_DataSubCategory  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_DataSubCategory (void)

#else

int Get_DataSubCategory ()

#endif
{
   int num;

   num = BUFR_Msg.Info.DataSubCategory;

   return (num);
}
