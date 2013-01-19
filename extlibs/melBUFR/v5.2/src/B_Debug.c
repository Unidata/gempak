/*
 * BUFR_Debug - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Debug( int level )

#else

void BUFR_Debug( level )
int level;

#endif

{
    extern int BUFR_Debug_Flag;

    BUFR_Debug_Flag = level;
}
