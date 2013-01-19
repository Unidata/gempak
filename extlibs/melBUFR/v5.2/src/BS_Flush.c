/*
 * BitStream_Flush - VERSION: %I%  %E% %T%
 */
/*
 * BitStream_Flush - Terminate a bit stream.
 * Returns 1 on error, else 0.
 * 
 * CHANGE LOG
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BitStream_Flush( BitStream_t* BS )

#else

int BitStream_Flush( BS )
BitStream_t* BS;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;

    if( BS == NULL )
    {
        BUFR_Err_Set( "BitStream_Flush", "NULL BitStream_t pointer" );
        fprintf(BUFR_Cntl.bufr_log, 
           "BitStream_Flush: NULL BitStream_t pointer\n");
        return 1;
    }

    /* If the bit stream is empty, do nothing. */

    if( BS->byte_num == 0 && BS->bit_num == 0 )
        return 0;

    if( BS->bit_num != 0 )
    {
        /*
         * Bit index is within a byte, move indices and pointer to next byte.
         */

        BS->bp++;
        BS->byte_num++;
        BS->bit_num = 0;
    }

    return 0;
}
