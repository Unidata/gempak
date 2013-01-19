/*
 * Int2ToInt - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/* This function is used to convert an Int2 structure to an unsigned short.
 * This is required to solve the big_endian/little_endian problem. The inverse
 * function is UshortToInt2
 */

#if PROTOTYPE_NEEDED

int Int2ToInt( Int2_t i2)

#else

int Int2ToInt( i2 )
Int2_t i2;

#endif
{
    int us;
    
    us = i2.val[0] << 8 | i2.val[1];
    
    return us;
}
