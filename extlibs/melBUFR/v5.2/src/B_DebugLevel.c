/*
 * BUFR_DebugLevel - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_DebugLevel( void )

#else

int BUFR_DebugLevel()

#endif

{
    extern int BUFR_Debug_Flag;

    return BUFR_Debug_Flag;
}
