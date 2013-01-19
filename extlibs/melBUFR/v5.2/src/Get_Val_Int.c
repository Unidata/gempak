/*
 * Get_Val_Int  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Get_Val_Int (BUFR_Val_t BV)

#else

int Get_Val_Int (BV)
BUFR_Val_t BV;

#endif
{
   int num;

   if ( BV.Val_Type != DT_INT ){
     printf(" Get_Val_Int: data type not DT_INT\n");
     BUFR_Err_Log("Get_Val_Int");
     return (VAL_ERROR);
   }

   num = BV.Val.int_number;
   
   return (num);
}
