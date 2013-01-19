/*
 * BitStream_Position - VERSION: %I%  %E% %T%
 */
/*
 * BitStream_Position - Adjust a bit stream's pointers relative to the current
 * positon.  Return 1 on error, else 0.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 * 
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BitStream_Position( BitStream_t* BS, int offset )

#else

int BitStream_Position( BS, offset )
BitStream_t* BS;
int          offset;    /* Number of bits (not bytes) to seek. */

#endif
{
    BitStream_t tmp_bs;
    int         old_bit_num, new_bit_num, last_bit;
    extern BUFR_Cntl_t BUFR_Cntl;

    if( BS == NULL )
    {
        BUFR_Err_Set( "BitStream_Position", "NULL BitStream_t pointer" );
        fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Position: NULL BitStream_t pointer\n");
        return 1;
    }

    tmp_bs = *BS;   /* Don't disturb BS in case of error. */

    old_bit_num = tmp_bs.byte_num*BITS_IN_BYTE + tmp_bs.bit_num;
    last_bit    = tmp_bs.size*BITS_IN_BYTE - 1;

    new_bit_num = old_bit_num + offset;

    if( new_bit_num < 0 )
    {
      BUFR_Err_Set( "BitStream_Position",
          "Attempt to seek before beginning of bit stream" );
      fprintf(BUFR_Cntl.bufr_log, 
          "BitStream_Position: Attempt to seek before beginning of bit stream\n");
      return 1;
    }

    /*
     * Don't fail if pointer reaches one bit past last bit.
     * This amounts to seeking to the end and is legal.
     */

    if( new_bit_num > (last_bit+1 ) )
    {
        BUFR_Err_Set( "BitStream_Position",
            "Attempt to seek past end of bit stream" );
        fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Position: Attempt to seek past end of bit stream\n");
        return 1;
    }

    tmp_bs.byte_num = new_bit_num / BITS_IN_BYTE;
    tmp_bs.bit_num  = new_bit_num % BITS_IN_BYTE;
    tmp_bs.bp       = tmp_bs.buffer + tmp_bs.byte_num;

    *BS = tmp_bs;

    return 0;
}
