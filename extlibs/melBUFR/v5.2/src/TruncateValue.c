/*
 * TruncateValue - VERSION: %I%  %E% %T%
 */
/*
 * TruncateValue - Return a truncated floating-point number -- even ones
 * larger than that which can be stored in an int (i.e., > 32 bits) --
 * without using any math library routines or making any assumptions about
 * floating point formats.
 *
 * Print the digit to a string, replace the first decimal point with a NULL
 * byte to terminate the string, and read the truncated value back from the
 * string.
 *
 * While this will work on any machine, it is probably the least efficient
 * method of truncation.
 */
/*
 * CHANGE LOG 
 *
 * 092997  LAH: Added cast to correct Linux warning 
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

double TruncateValue( double Value )

#else

double TruncateValue( Value )
double Value;

#endif
{
    double t;
    char   buf[1024];   /* Large enough to hold numbers > 3e256 */
    char*  bp;

    /* Without the '.16' a value gets rounded up. */

    sprintf( buf, "%.16f", Value );

    /*  092997  LAH: Added cast to correct Linux warning */
    for( bp=&buf[0]; *bp != (char) NULL; bp++ )
    {
        if( *bp == '.' )
        {
            /*  092997  LAH: Added cast to correct Linux warning */
            *bp = (char) NULL;
            break;
        }
    }

    sscanf( buf, "%lf", &t );

    return t;
}
