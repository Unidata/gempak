/*
 * TenPow - VERSION: %I%  %E% %T%
 */
/*
 * TenPow - Return 10 raised to the given integer power.  This function is
 * provided in order to allow the BUFR Libray to be a stand-alone library.
 * If, the pow() function were used instead, any program calling BUFR
 * library functions would have to link with the math library (-lm) in
 * addition to the BUFR library (-lbufr).  This is inconvenient.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

double TenPow( int Power )

#else

double TenPow( Power )
int Power;

#endif
{
    int    n;
    double result;

    if( Power > 0 )
    {
        for( n=0, result=1.0; n < Power; n++ )
            result *= 10.0;
    }
    else if( Power < 0 )
    {
        for( n=0, result=1.0; n > Power; n-- )
            result /= 10.0;
    }
    else                /* Power == 0 */
        result = 1.0;

    return result;
}
