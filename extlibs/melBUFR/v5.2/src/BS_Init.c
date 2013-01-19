/*
 * BitStream_Init - VERSION: %I%  %E% %T%
 */
/*
 * BitStream_Init - Initialize a bit stream.  Return 1 on error, else 0.
 */
/*
 * CHANGE LOG
 *
 * 100897  LAH: Added uint_t cast 
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BitStream_Init( BitStream_t* BS, int Size )

#else

int BitStream_Init( BS, Size )
BitStream_t* BS;
int          Size;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;

    if( BS == NULL )
    {
        BUFR_Err_Set( "BitStream_Init", "NULL BitStream_t pointer" );
        fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Init: NULL BitStream_t pointer\n");
        return 1;
    }

    if( Size < 0 )
    {
        BUFR_Err_Set( "BitStream_Init", "Requested size < 0" );
        fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Init: Requested size < 0\n");
        return 1;
    }

    BS->buffer   = NULL;
    BS->size     = 0;
    BS->bp       = BS->buffer;
    BS->byte_num = 0;
    BS->bit_num  = 0;

    if( Size > 0 )
    {
        /* 100897  LAH: Added uint_t cast */
        if( (BS->buffer = (uchar_t*) malloc( (uint_t) Size )) == NULL )
        {
            BUFR_Err_Set( "BitStream_Init", "Can't allocate buffer" );
            fprintf(BUFR_Cntl.bufr_log, 
                   "BitStream_Init: Can't allocate buffer\n");
            return 1;
        } else
        {
            BS->size = Size;
            BS->bp   = BS->buffer;
        }
    }

    return BitStream_Rewind( BS );
}
