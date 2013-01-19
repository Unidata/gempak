/*
 * Set_Time - VERSION: %I%  %E% %T%
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Set_Time( BUFR_Info_t* BI, int time)

#else

int Set_Time( BI, time)
BUFR_Info_t* BI;
int time;
#endif
{
    if( BI == NULL )
    {
        BUFR_Err_Set( "BUFR_Info_Init", "NULL BUFR_Info_t pointer" );
        return 1;
    }


    BI->Hour = time/100;
    BI->Minute = time - BI->Hour * 100;
    
    return 0;
}
