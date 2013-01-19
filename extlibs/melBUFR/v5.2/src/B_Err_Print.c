/*
 * BUFR_Err_Print - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Err_Print(): Print BUFR error message and function names logged to
 * the standard error and bufr_log file.
 * 
 *  CHANGE LOG
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Err_Print( char* str )

#else

void BUFR_Err_Print( str )
char* str;

#endif
{
    extern BUFR_Err_t BUFR_Error;
    extern BUFR_Cntl_t BUFR_Cntl;

    int i;

    if( !BUFR_Error.Set )
        return;

    if( str != NULL )
    {
        fprintf( stderr, "%s ", str );
        fprintf( BUFR_Cntl.bufr_log, "%s ", str );
    }
    fprintf( stderr, "%s\n", BUFR_Error.Message );
    fprintf(BUFR_Cntl.bufr_log, "%s\n", BUFR_Error.Message );

    if( BUFR_Error.SystemError != 0 )
    {
        fprintf( stderr, "System error (%d): %s\n", BUFR_Error.SystemError,
            strerror(BUFR_Error.SystemError) );
        fprintf(BUFR_Cntl.bufr_log, "System error (%d): %s\n",
	    BUFR_Error.SystemError,
            strerror(BUFR_Error.SystemError) );
    }

    fprintf( stderr, "    Error occurred in %s()\n", BUFR_Error.Log[0] );
    fprintf( BUFR_Cntl.bufr_log, "    Error occurred in %s()\n", BUFR_Error.Log[0] );

    for( i=1; i < BUFR_Error.NumLogs; i++ )
    {
        fprintf( stderr, "        which was called by %s()\n",
            BUFR_Error.Log[i] );
        fprintf(BUFR_Cntl.bufr_log, "        which was called by %s()\n",
            BUFR_Error.Log[i] );
    }

}
