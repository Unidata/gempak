/*
 * Get_Val_Type  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

DataType_t Get_Val_Type (BUFR_Val_t BV)

#else

DataType_t Get_Val_Type (BV)
BUFR_Val_t BV;

#endif
{
   
   return( BV.Val_Type);

}
