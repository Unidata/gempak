/*
 * Find_Status - VERSION: %I%  %E% %T%
 */
/*
 * Find_Status - Check the BUFR message file status and set n
 * to the status.
 */
/*
 * CHANGE LOG 
 *
 * 092997  LAH: Deleted unsued vaariable BM
 * 101097  LAH: Added void to function prototype
 *
 */
#include <mel_bufr.h>
 
#if PROTOTYPE_NEEDED
 
int Find_Status( void )
 
#else

int Find_Status( )
 
#endif
{
    extern BUFR_Msg_t BUFR_Msg;
 
    int 	    n;

    n = 0;
    if(BUFR_Msg.FileStatus == BUFR_EOM) n = BUFR_EOM;
    if(BUFR_Msg.FileStatus == BUFR_EOF) n = BUFR_EOF;

    return n;
}
