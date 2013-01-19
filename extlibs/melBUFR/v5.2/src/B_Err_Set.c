/*
 * BUFR_Err_Set - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Err_Set(): Store error information for a BUFR message.
 * This function is called when a BUFR error first occurs.
 */

#include <mel_bufr.h>
extern BUFR_Err_t BUFR_Error;

#if PROTOTYPE_NEEDED

void BUFR_Err_Set( char* FunctionName, char* Msg )

#else

void BUFR_Err_Set( FunctionName, Msg )
char* FunctionName;
char* Msg;

#endif
{

    if( Msg == NULL )
    {
        fprintf( stderr, "BUFR_Error(): No error message supplied.\n" );
        return;
    }

    BUFR_Err_Clear();

    BUFR_Err_Log( FunctionName );

    BUFR_Error.Message     = BUFR_strdup( Msg );
    BUFR_Error.Set         = 1;
    BUFR_Error.SystemError = errno;
}
