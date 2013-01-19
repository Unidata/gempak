/*
 * Set_Status - VERSION: %I%  %E% %T%
 */
/*
 * Set_Status - Set the BUFR message file status to EOM and set n
 * to the status.
 */
/*
 * CHANGE LOG
 *     092997  LAH:  Removed definition of unused variable BM
 *     101097  LAH:  Added void to function prototype
 *
 */
#include <mel_bufr.h>
 
#if PROTOTYPE_NEEDED
 
int Set_Status( void )
 
#else

int Set_Status( )
 
#endif
{
    extern BUFR_Msg_t BUFR_Msg;
 
    int 	    n;

    BUFR_Msg.FileStatus = BUFR_EOM; 
    n = BUFR_EOM;

    return n;
}
