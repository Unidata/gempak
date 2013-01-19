/*
 * BUFR_AtEOF - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_AtEOF() - Return 1 if there are no more values in the message
 * being decoded and there are no more BUFR messages in the file.
 */
/*
 * LAST MODIFICATION 
 *
 * 092997  LAH: Added cast to correct Linux warning 
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int BUFR_AtEOF( void )

#else

int BUFR_AtEOF()

#endif

{

    if( BUFR_Msg.InitStatus == NEVER_INITIALIZED )
        return 0;

    /* 092997  LAH: Added cast to correct Linux warning */
    if( BUFR_Msg.FileName[0] == (char) NULL )
        return 0;

    if( BUFR_Msg.FilePtr == NULL )
        return 0;

    if( BUFR_AtEOM() )
        return BUFR_Msg.FileStatus == BUFR_EOF;

    return 0;
}
