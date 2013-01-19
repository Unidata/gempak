/*
 * BUFR_AtEOD - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_AtEOD() - Return 1 if the last value in the current dataset has been
 * reached (within the BUFR message currently being decoded) but there are
 * more datasets to decode.
 */
/*
 * CHANGE LOG 
 *
 * 092997  LAH: Added cast to correct Linux warning 
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int BUFR_AtEOD( void )

#else

int BUFR_AtEOD()

#endif

{

    if( BUFR_Msg.InitStatus == NEVER_INITIALIZED )
        return 0;

    /*  092997  LAH: Added cast to correct Linux warning */
    if( BUFR_Msg.FileName[0] == (char) NULL )
        return 0;

    if( BUFR_Msg.FilePtr == NULL )
        return 0;

    return BUFR_Msg.MsgStatus == BUFR_EOD;
}
