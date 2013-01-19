/*
 * BUFR_ProcMethod - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_ProcMethod - Return processing method.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

MethFlag_t BUFR_ProcMethod( void )

#else

MethFlag_t BUFR_ProcMethod()

#endif
{
    extern BUFR_Msg_t BUFR_Msg;

    switch( BUFR_Msg.MethFlag )
    {
        case METHOD_VALUES:
        case METHOD_RAW:
        case METHOD_TEMPLATE:
            return BUFR_Msg.MethFlag;

        case METHOD_UNKNOWN:
        default:
            return METHOD_UNKNOWN;
    }
}
