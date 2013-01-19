/*
 * BitStream_Put - VERSION: %I%  %E% %T%
 */
/*
 * BitStream_Put - Write a value to a bit stream.  Return 1 on error, else 0.
 *
 * POSSIBLY ENDIAN SENSITIVE
 */
/*
 * CHANGE LOG
 *
 * 101097  LAH: Made hex constants 32 bits
 * 022498  LAH:  Added prints to bufr_log file.
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BitStream_Put( BitStream_t* BS, EncVal_t EV )

#else

int BitStream_Put( BS, EV )
BitStream_t* BS;
EncVal_t     EV;

#endif
{
    static uchar_t mask[8]={ 0x00000080, 0x00000040, 0x00000020, 
			     0x00000010, 0x00000008, 0x00000004,
			     0x00000002, 0x00000001 };

    int i, n;

    extern BUFR_Cntl_t BUFR_Cntl;

    if( BS == NULL )
    {
        BUFR_Err_Set( "BitStream_Put", "NULL BitStream_t pointer" );
        fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Put: NULL BitStream_t pointer\n");
        return 1;
    }

    if( EncVal_IsBad( EV ) )
    {
        BUFR_Err_Set( "BitStream_Put", "Invalid EncVal_t" );
        fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Put: Invalid EncVal_t\n");
        return 1;
    }

    while( (BS->byte_num + BytesInBits(EV.nbits)) >= BS->size )
    {
        /*
         * This value will overflow the buffer so increase the buffer size.
         */

        if( BitStream_Increase( BS ) )
              return 1;
    }

    for( i=0; i < EV.nbits; i++ )
    {
        n = HexStr_GetBit( EV.value, i );

        if( n == 1 )
            *BS->bp |= mask[ BS->bit_num ];

        BS->bit_num++;

        while( BS->bit_num >= BITS_IN_BYTE )
        {
            BS->bit_num -= BITS_IN_BYTE;
            BS->byte_num++;
            BS->bp++;
        }
    }

    return 0;
}
