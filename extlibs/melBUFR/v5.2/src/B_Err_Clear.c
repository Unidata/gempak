/*
 * BUFR_Err_Clear - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Err_Clear(): Reset BUFR error message.
 */

#include <mel_bufr.h>
extern BUFR_Err_t BUFR_Error;

#if PROTOTYPE_NEEDED

void BUFR_Err_Clear( void )

#else

void BUFR_Err_Clear()

#endif
{

    int i;

    if( BUFR_Error.NumLogs != 0 )
    {
        for( i=0; i < BUFR_Error.NumLogs; i++ )
            free( (void*) BUFR_Error.Log[i] );

        BUFR_Error.NumLogs = 0;
    }

    if( BUFR_Error.Message != NULL )
    {
        free( (void*) BUFR_Error.Message );
        BUFR_Error.Message = NULL;
    }

    BUFR_Error.Set         = 0;
    BUFR_Error.SystemError = 0;
    errno                  = 0;     /* Clear system error. */
}
