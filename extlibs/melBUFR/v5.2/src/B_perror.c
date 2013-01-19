/*
 * BUFR_perror - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_perror( char* str )

#else

void BUFR_perror( str )
char* str;

#endif
{
    BUFR_Err_Log( str );

    fflush( stdout );

    BUFR_Err_Print( NULL );

    fflush( stderr );
}
