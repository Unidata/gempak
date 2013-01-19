/*
 * BUFR_Err_Init - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Err_Init(): Initialize BUFR message error log.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Err_Init( void )

#else

void BUFR_Err_Init()

#endif
{
    extern BUFR_Err_t BUFR_Error;

    int i;

    for( i=0; i < MAX_BUFR_ERR_LOG; i++ )
        BUFR_Error.Log[i] = NULL;

    BUFR_Error.NumLogs     = 0;
    BUFR_Error.Message     = NULL;
    BUFR_Error.Set         = 0;
    BUFR_Error.SystemError = 0;
}
