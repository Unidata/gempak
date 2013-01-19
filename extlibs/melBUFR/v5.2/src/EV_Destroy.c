/*
 * EncVal_Destroy - VERSION: %I%  %E% %T%
 */
/*
 * EncVal_Destroy - Release memory allocated to an encoded value.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void EncVal_Destroy( EncVal_t EV )

#else

void EncVal_Destroy( EV )
EncVal_t EV;

#endif
{
    if( EV.value == NULL )
        return;

    free( (void*) EV.value );
}
