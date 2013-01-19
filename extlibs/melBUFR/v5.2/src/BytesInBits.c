/*
 * BytesInBits - VERSION: %I%  %E% %T%
 */
/*
 * BytesInBits - Return # bytes needed to store given number of bits.
 * Return 0 on error.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BytesInBits( int nBits )

#else

int BytesInBits( nBits )
int nBits;

#endif
{
    int nbytes;
    extern BUFR_Cntl_t BUFR_Cntl;

    if( nBits < 1 )
    {
        BUFR_Err_Set( "BytesInBits", "Number of bits < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BytesInBits", 
	     "Number of bits < 1" );
        return 0;
    }

    nbytes = nBits/BITS_IN_BYTE + ( (nBits % BITS_IN_BYTE) != 0 );

    return nbytes;
}
