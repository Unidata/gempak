#include "geminc.h"
#include "cpgcmn.h"

#define HEADER_SIZE 48


void pg_getsix ( char *outl, int sixnum, char *ch, int *iret )
/************************************************************************
 * pg_getsix								*
 *									*
 * This function extracts a 6-bit entity from a plane of 8-bit bytes	*
 * at a requested location (6-bit number offset).			*
 *									*
 * pg_getsix ( outl, sixnum, ch, iret )					*
 *									*
 * Input parameters:							*
 *	*outl	char 	Plane of packed 6-bit entities			*
 *	sixnum	int	6-bit number to be returned			*
 *									*
 * Output parameters:							*
 *	*ch	char 	Returned 6-bit value, left justified		*
 *	*iret	int	Return code					*
 **									*
 * Log:									*
 *	E. Wehner/EAi	6/96	Created				 	*
 *	T. Piper/GSC	10/98	Prolog update				*
 ***********************************************************************/
{
    int byte_start;
    int byte_stop;
    int bit_start;
    int total_bits;
    *ch = '\0';   /* temporary character */
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    total_bits = sixnum * 6;

    /* calculate where in relations to bytes this 6 bit will be retrieved.
       This number could be split over two bytes, or it could occur 
       completely in a single byte. */
    byte_start = ((int)total_bits/8) + HEADER_SIZE;
    byte_stop = ((int)(total_bits+5)/8) + HEADER_SIZE;
    bit_start = (int)total_bits%8;

    switch (bit_start)
    {
      /* Interested in XXXXXX** ********   of two byte sequence  */
      case 0:
      case 1:
        *ch = (char)(outl[byte_start] & 0xfc);
        break;
      /* Interested in **XXXXXX ********    of two byte sequence */
      case 2:
      case 3:
        *ch = (char)((outl[byte_start] << 2) & 0xfc);
        break;
      /* Interested in ****XXXX XX****      of two byte sequence */
      case 4:
      case 5:
        *ch = (char)((outl[byte_start] << 4) & 0xf0);
        *ch |= ((outl[byte_stop] >> 6) & 0x03) << 2;
        break;
      /* Interested in ******XX XXXX****     of two byte sequence */
      case 6:
      case 7:
        *ch = (char)(((outl[byte_start] & 0x03) << 6) & 0xc0);
        *ch |= (((outl[byte_stop] & 0xf0) >> 4) & 0x0f) << 2; 
      default: 
        /* reject the 6 bit...something is wrong */
        break;
    }
    return;

}
