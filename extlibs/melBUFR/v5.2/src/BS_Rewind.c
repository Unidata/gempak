/*
 * BitStream_Rewind - VERSION: %I%  %E% %T%
 */
/*
 * BitStream_Rewind - Rewind a bit stream.  Return 1 on error, else 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BitStream_Rewind( BitStream_t* BS )

#else

int BitStream_Rewind( BS )
BitStream_t* BS;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;

    if( BS == NULL )
    {
        BUFR_Err_Set( "BitStream_Rewind", "NULL BitStream_t pointer" );
        fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Rewind: NULL BitStream_t pointer\n");
        return 1;
    }

    BS->bp       = BS->buffer;
    BS->byte_num = 0;
    BS->bit_num  = 0;

    return 0;
}
