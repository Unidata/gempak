/*
 * HexStr_GetBit - VERSION: %I%  %E% %T%
 */

/*
 * HexStr_GetBit - Get given bit within a HexStr_t.  Bits are numbered in a
 * left-justified fashion as follows:
 *
 *          Byte  0                Byte  2        Byte  3
 * +-----------------------+-----------------------+------------+---
 * | 7| 6| 5| 4| 3| 2| 1| 0| 7| 6| 5| 4| 3| 2| 1| 0|| 7| 6|...| 0|...
 * +-----------------------+-----------------------+------------+---
 *
 *  Bit Numbers
 * +-----------------------+-----------------------+------------+---
 * | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9| A| B| C| D| E| F|10|11|...|1F|...
 * +-----------------------+-----------------------+------------+---
 *
 * Return 1 if the given bit is set, 0 if not or in case of error.
 *
 * CHANGE LOG
 *
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int HexStr_GetBit( HexStr_t HS, int BitNum )

#else

int HexStr_GetBit( HS, BitNum )
HexStr_t HS;
int BitNum;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    int byte_index;
    int bit_index;

    int bit_setting;

    if( HS == NULL )
    {
        BUFR_Err_Set( "HexStr_GetBit", "NULL HexStr_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_GetBit", 
	     "NULL HexStr_t pointer" );
        return 0;
    }

    if( BitNum < 0 )
    {
        BUFR_Err_Set( "HexStr_GetBit", "Bit number < 0" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "HexStr_GetBit", 
	     "Bit number < 0" );
        return 0;
    }

    byte_index = BitNum / BITS_IN_BYTE;
    bit_index  = BitNum % BITS_IN_BYTE;

    /* Flip the bit index so it runs R->L instead of L->R */

    bit_index = (BITS_IN_BYTE-1) - bit_index;

    bit_setting = (HS[byte_index] >> bit_index);
    bit_setting &= 1;

    return bit_setting;
}

