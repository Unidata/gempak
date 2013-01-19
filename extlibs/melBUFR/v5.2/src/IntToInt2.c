/*
 * IntToInt2 - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>


/* This function is used to convert an unsigned short to an Int2 structure.
 * This is required to solve the big_endian/little_endian problem. The inverse
 * function is Int2ToInt.
 */

#if PROTOTYPE_NEEDED

Int2_t IntToInt2( int us )

#else

Int2_t IntToInt2( us )
int us;

#endif
{
    Int2_t i2;
    
    /* 100997 LAH: ADDED uchar_t casts */
    /* 101097 LAH: Made hex constants 32 bits */
    i2.val[0] = (uchar_t) ((us >> 8) & 0x000000FF);
    i2.val[1] = (uchar_t) (us & 0x000000FF);
    
    return i2;
}
