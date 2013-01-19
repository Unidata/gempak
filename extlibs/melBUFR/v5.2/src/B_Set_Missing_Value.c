/*
 * BUFR_Set_Missing_Value - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
 
#if PROTOTYPE_NEEDED
 
void BUFR_Set_Missing_Value( double Missing_Val)
 
#else
 
void BUFR_Set_Missing_Value( Missing_Val )
double         Missing_Val;    /* User defined Missing Value indicator       */
 
#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;




    BUFR_Cntl.User_Missing_Value = Missing_Val;

    BUFR_Cntl.Missing_User_Set = 2;
}
