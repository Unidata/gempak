/*
 * BUFR_Reset_Scale - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Reset_Scale - Use descriptor 2-02-000 to restore scale.
 * to default values.  Return 1 on error, else 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Reset_Scale( void )

#else

int BUFR_Reset_Scale()

#endif
{
    if( BUFR_Change_Scale( 0 ) )
    {
        BUFR_Err_Log( "BUFR_Reset_Scale" );
        return 1;
    }
    else
        return 0;
}
