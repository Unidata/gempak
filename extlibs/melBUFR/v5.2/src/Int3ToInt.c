/*
 * Int3ToInt - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
/* CHANGE LOG 
*
* 100897  LAH:  Changes function typing from uint to int
*
*/
#if PROTOTYPE_NEEDED

int Int3ToInt( Int3_t i3 )

#else

int Int3ToInt( i3 )
Int3_t i3;

#endif
{
    int ul;

    ul = (i3.val[0] << 16) | (i3.val[1] << 8) | i3.val[2];

    return ul;
}

