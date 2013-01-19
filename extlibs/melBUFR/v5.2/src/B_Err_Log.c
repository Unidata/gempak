/*
 * BUFR_Err_Log - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Err_Log(): Log names of functions (in inverse order) for which a
 * BUFR error has occurred.  This function is useful for tracing errors.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Err_Log( char* FunctionName )

#else

void BUFR_Err_Log( FunctionName )
char*   FunctionName;

#endif
{
    extern BUFR_Err_t BUFR_Error;

    static char* UnknownFunction = "<unknown>";

    if( FunctionName == NULL )
        BUFR_Error.Log[ BUFR_Error.NumLogs ] = UnknownFunction;
    else
        BUFR_Error.Log[ BUFR_Error.NumLogs ] = BUFR_strdup( FunctionName );

    BUFR_Error.NumLogs++;
}
