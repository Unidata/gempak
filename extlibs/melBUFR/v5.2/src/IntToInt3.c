/*
 * IntToInt3 - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/* CHANGE LOG
*
* 100897  LAH:  Changed argument type from uint_t to int
*/

#if PROTOTYPE_NEEDED

Int3_t IntToInt3( int ul )

#else

Int3_t IntToInt3( ul )
int ul;

#endif
{
    Int3_t i3;

    /* 100997 LAH: Added uchar_t casts */
    /* 101097 LAH: Made hex constants 32 bits long */
    i3.val[0] = (uchar_t) ((ul >> 16) & 0x000000FF);
    i3.val[1] = (uchar_t) ((ul >>  8) & 0x000000FF);
    i3.val[2] = (uchar_t) (ul & 0x000000FF);

    return i3;
}
