/*
 * BitStream_Destroy - VERSION: %I%  %E% %T%
 */
/*
 * BitStream_Destroy - Destroy a bit stream.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BitStream_Destroy( BitStream_t* BS )

#else

void BitStream_Destroy( BS )
BitStream_t* BS;

#endif
{
    if( BS == NULL )
        return;

    if( BS->buffer != NULL )
        free( (void*) BS->buffer );
}
