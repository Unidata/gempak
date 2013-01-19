/*
 * BUFR_ProcType - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_ProcType - Return processing type.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

ProcFlag_t BUFR_ProcType( void )

#else

ProcFlag_t BUFR_ProcType()

#endif
{
    extern BUFR_Msg_t BUFR_Msg;

    switch( BUFR_Msg.ProcFlag )
    {
        case TYPE_ENCODE:
        case TYPE_DECODE:
            return BUFR_Msg.ProcFlag;

        case TYPE_UNKNOWN:
        default:
            return TYPE_UNKNOWN;
    }
}
