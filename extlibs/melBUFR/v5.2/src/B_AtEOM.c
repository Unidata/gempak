/*
 * BUFR_AtEOM - VERSION: %I%  %E% %T%
 */
/*
 * LAST MODIFICATION 
 *
 * 092997  LAH: Added cast to correct Linux warning 
 */
/*
 * BUFR_AtEOM() - Return 1 if there are no more values in the message
 * being decoded but there are more BUFR messages in the file.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int BUFR_AtEOM( void )

#else

int BUFR_AtEOM()

#endif

{

    if( BUFR_Msg.InitStatus == NEVER_INITIALIZED )
        return 0;

    /* 092997  LAH: Added cast to correct Linux warning */
    if( BUFR_Msg.FileName[0] == (char) NULL )
        return 0;

    if( BUFR_Msg.FilePtr == NULL )
        return 0;

    return BUFR_Msg.MsgStatus == BUFR_EOM;
}
