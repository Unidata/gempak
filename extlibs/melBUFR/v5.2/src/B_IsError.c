/*
 * BUFR_IsError - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_IsError - Return 1 if a BUFR error
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_IsError( void )

#else

int BUFR_IsError()

#endif
{
    extern BUFR_Err_t BUFR_Error;

    return BUFR_Error.Set;
}
