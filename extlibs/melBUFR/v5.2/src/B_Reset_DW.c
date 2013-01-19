/*
 * BUFR_Reset_DataWidth - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Reset_DataWidth - Use descriptor 2-01-000 to restore data width
 * to default values.  Return 1 on error, else 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Reset_DataWidth( void )

#else

int BUFR_Reset_DataWidth()

#endif
{
    if( BUFR_Change_DataWidth( 0 ) )
    {
        BUFR_Err_Log( "BUFR_Reset_DataWidth" );
        return 1;
    }
    else
        return 0;
}
