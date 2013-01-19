/*
 * Get_Val_Double  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

double Get_Val_Double (BUFR_Val_t BV)

#else

double Get_Val_Double (BV)
BUFR_Val_t BV;

#endif
{
   double num;

   if ( BV.Val_Type != DT_DOUBLE ){
     printf(" Get_Val_Double: data type not DT_DOUBLE\n");
     BUFR_Err_Log("Get_Val_Double");
     return (VAL_ERROR);
   }

   num = BV.Val.number;
   
   return (num);
}
