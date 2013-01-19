/*
 * BUFR_BitWidth - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_BitWidth - Return the minimum number of bits needed to represent
 * the given value.
 */
/*
 *   CHANGE LOG
 *
 *   ????97 UUU:  Function argument changed form u_int_t to int
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_BitWidth( int Val )

#else

int BUFR_BitWidth( Val )
int Val;

#endif
{
    int i;
    int  abs_val;

    /* If Val == 0, code falls through to return bit size of 1. */
    
    if ( Val < 0 ){
       abs_val = - Val;
    } else {
       abs_val = Val;
    }
    for( i=1; i < (int)BITS_IN_WORD; i++ )
    {
        abs_val >>= 1;

        if( abs_val == 0 )
            break;
    }

    if ( Val < 0 )
       i = i +1;

    return i;
}
