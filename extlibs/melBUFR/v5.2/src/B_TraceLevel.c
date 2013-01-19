/*
 * BUFR_TraceLevel - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_TraceLevel( void )

#else

int BUFR_TraceLevel()

#endif

{
    extern int BUFR_Trace_Flag;

    return BUFR_Trace_Flag;
}
