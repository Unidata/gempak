/*
 * BUFR_Trace - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Trace( int level )

#else

void BUFR_Trace( level )
int level;

#endif

{
    extern int BUFR_Trace_Flag;

    BUFR_Trace_Flag = level;
}
