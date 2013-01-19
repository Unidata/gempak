/*
 * EncVal_IsBad - VERSION: %I%  %E% %T%
 */
/*
 * EncVal_IsBad - Return 1 if given EncVal_t is bad, otherwise 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int EncVal_IsBad( EncVal_t EV )

#else

int EncVal_IsBad( EV )
EncVal_t EV;

#endif
{
    return ( EV.value == NULL || EV.nbits < 1 );
}
