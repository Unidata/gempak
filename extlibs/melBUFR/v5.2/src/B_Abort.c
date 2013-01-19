/*
 * BUFR_Abort - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Abort() - Issue an error message, perform a quick cleanup and
 * terminate processing.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

void BUFR_Abort( void )

#else

void BUFR_Abort()

#endif

{

    BUFR_Err_Print( NULL );

    /* Force complete destruction. */

    BUFR_Msg.ProcFlag = TYPE_UNKNOWN;

    BUFR_Destroy(1);

    abort();
}
