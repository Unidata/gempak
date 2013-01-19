/*
 * BUFR_MaxVal - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_MaxVal - Return maximum value, given bit width.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

uint_t BUFR_MaxVal( int BitWidth )

#else

uint_t BUFR_MaxVal( BitWidth )
int BitWidth;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    int    i;
    uint_t n;

    if( BitWidth < 1 )
    {
        BUFR_Err_Set( "BUFR_MaxVal", "Bit width < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_MaxVal", 
            "Bit width < 1" );
        return 0;
    }

    /* 102097 LAH; Added uint-t cast */
    if( (uint_t) BitWidth > BITS_IN_WORD )
    {
        BUFR_Err_Set( "BUFR_MaxVal", "Bit width > # bits in a word" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_MaxVal", 
            "Bit width > # bits in a word" );
        return 0;
    }

    for( i=0, n=0; i < BitWidth; i++ )
        n = (n << 1) | 1;

    return n;
}
