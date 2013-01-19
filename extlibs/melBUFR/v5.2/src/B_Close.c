/*
 * BUFR_Close - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Close( void )

#else

void BUFR_Close()

#endif
{
    extern BUFR_Msg_t BUFR_Msg;

    if( BUFR_Msg.FilePtr == NULL )
        return;

    (void) fclose( BUFR_Msg.FilePtr );

    BUFR_Msg.FilePtr = NULL;
}
