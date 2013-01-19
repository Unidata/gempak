/*
 * ValStack_Destroy - VERSION: %I%  %E% %T%
 */
/*
 * ValStack_Destroy - Destroy a value stack.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void ValStack_Destroy( ValStack_t* VS )

#else

void ValStack_Destroy( VS )
ValStack_t* VS;

#endif
{
    if( VS == NULL )
        return;

    ValStack_Clear( VS );

    if( VS->head != NULL )
        free( (void*) VS->head );

    if( VS->tail != NULL )
        free( (void*) VS->tail );
}
