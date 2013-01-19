/*
 * BitStream_Increase - VERSION: %I%  %E% %T%
 */
/*
 * BitStream_Increase - Increase a bit stream.  Return 1 on error, else 0.
 */
/*
 * CHANGE LOG
 *
 * 100897 LAH: Added uint_t cast 
 * 022498 LAH:  Added prints to bufr_log file.
 * 070102 LAH: Changed memset range to be sure the memory block is cleared correctly
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BitStream_Increase( BitStream_t* BS )

#else

int BitStream_Increase( BS )
BitStream_t* BS;

#endif
{
    BitStream_t old_bs;

    extern BUFR_Cntl_t BUFR_Cntl;

    if( BS == NULL )
    {
        BUFR_Err_Set( "BitStream_Increase", "NULL BitStream_t pointer" );
        fprintf(BUFR_Cntl.bufr_log, 
            "BitStream_Increase: NULL BitStream_t pointer\n");
        return 1;
    }

    if( BS->buffer == NULL )
    {
        /*
         * No data has been allocated for the bit stream.
         * Use malloc() instead of BUFR_realloc().
         */

        if( (BS->buffer=(uchar_t*)malloc(BIT_STREAM_BLOCK_SIZE)) == NULL)
        {
            BUFR_Err_Set( "BitStream_Increase", "Can't allocate buffer" );
            BS->size = 0;
            fprintf(BUFR_Cntl.bufr_log, 
                 "BitStream_Increase: Can't allocate buffer\n");
            return 1;
        } else
        {
            BS->size = BIT_STREAM_BLOCK_SIZE;
            BS->bp   = BS->buffer;

            /* Clear the buffer -- there's no telling what's in it. */

            /* 100897 LAH: Added uint_t cast */
            (void) memset( (char*)BS->bp, 0, (uint_t) BS->size );

            return 0;
        }
    }

    /* Save a copy of the bit stream in case reallocation fails. */

    old_bs = *BS;

    BS->size += BIT_STREAM_BLOCK_SIZE;

    /* 100897 LAH: Added uint_t cast */
    if( (BS->buffer=(uchar_t*)realloc( BS->buffer, (uint_t)BS->size )) == NULL )
    {
        BUFR_Err_Set( "BitStream_Increase", "Can't reallocate buffer" );
        *BS = old_bs;
        fprintf(BUFR_Cntl.bufr_log, 
	        "BitStream_Increase: Can't reallocate buffer\n");
        return 1;
    }

    /*
     * Reallocated buffer may have been moved.  Reset the current buffer
     * pointer.
     */

    BS->bp = BS->buffer + BS->byte_num;

    /* Clear the newly allocated memory after the buffer pointer. */
/*  070102  LAH  Changed memset range to be sure the memory block is cleared correctly */
/*    (void) memset( (char*)BS->bp+1, 0, BIT_STREAM_BLOCK_SIZE ); */
    (void) memset( (char*)(BS->buffer + (BS->size - BIT_STREAM_BLOCK_SIZE)), 0, BIT_STREAM_BLOCK_SIZE );


    return 0;
}
