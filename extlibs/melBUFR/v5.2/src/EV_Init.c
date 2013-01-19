/*
 * EncVal_Init - VERSION: %I%  %E% %T%
 */
/*
 * EncVal_Init - Initialize an encoded value.  Return 1 on error, else 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void EncVal_Init( EncVal_t* EV )

#else

void EncVal_Init( EV )
EncVal_t* EV;

#endif
{
    if( EV == NULL )
        return;

    EV->value = (HexStr_t) NULL;
    EV->nbits = 0;
}
