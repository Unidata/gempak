/*
 * BUFR_Position_Msg - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Position_Msg() - Position a file at the beginning of a BUFR message
 * (the file must already be opened for reading).  Return 1 if the beginning
 * cannot be found otherwise return 0.
 */
/*
 * CHANGE LOG
 *
 * 100997 LAH: Added int and long casts
 * 022498 LAH:  Added prints to bufr_log file.
 * 031798 LAH: Corrected problem with positional file, when header
 *             contained a B, BU, or BUF.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

/* Maximum number of bytes to search for "BUFR" before giving up. */

#define MAX_SEARCH_LEN 1000

#if PROTOTYPE_NEEDED

int BUFR_Position_Msg( void )

#else

int BUFR_Position_Msg()

#endif
{

    int    i, c, n, offset;
    long   original_position;   /* File position before "BUFR" search. */
    long   starting_position;   /* Beginning of BUFR message. */
    Int3_t tmp;
    int    message_size;        /* Length of BUFR message, in bytes. */
    char   buf7[4];

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Positioning file to start of BUFR message\n" );
#endif

    if( BUFR_Msg.FilePtr == NULL )
    {
        BUFR_Err_Set( "BUFR_Position_Msg", "Input file has not been opened" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
            "BUFR_Position_Msg: Input file has not been opened");
        return 1;
    }

    /* Save file location in case search fails. */

    original_position = ftell( BUFR_Msg.FilePtr );

    /*
     * Search for the characters 'B', 'U', 'F', and 'R' which signal the
     * beginning of a BUFR message.
     */
     /*
      * 031798 LAH:  Corrected problem caused when a header contined
      * a B, BU, or BUF.  This caused a misscount in the number of 
      * characters read because the i index was not incremented.  Created
      * the variable offset which is incremented by the appropiate amount
      * each time one of these sequences is encountered.  The offset is 
      * then added whne calculating the starting position.
      */
    starting_position = original_position;
    offset =0;

    for( i=0; i < MAX_SEARCH_LEN; i++ )
    {
        if( (c = getc( BUFR_Msg.FilePtr )) == EOF )
            break;
        else if( (char) c != 'B' )
            continue;

        if( (c = getc( BUFR_Msg.FilePtr )) == EOF )
            break;
        else if( (char) c != 'U' ) 
        {
            offset = offset + 1;
            continue;
        }

        if( (c = getc( BUFR_Msg.FilePtr )) == EOF )
            break;
        else if( (char) c != 'F' )
        {
            offset = offset + 2;
            continue;
        }

        if( (c = getc( BUFR_Msg.FilePtr )) == EOF )
            break;
        else if( (char) c != 'R' )
        {
            offset = offset + 3;
            continue;
        }

        /*
         * Found the "BUFR" string.  Make sure that this is actually a BUFR
         * message -- not a tar file which happens to contain a file name
         * of BUFR-something -- by checking the message size and the existence
         * of a trailing "7777" where it should be (based on message size).
         */

        /* 100997 LAH: Added long cast */
        starting_position = original_position + (long)i + offset;

        /* Get the size of the BUFR message */

        if( fread( (char*) &tmp, 1, 3, BUFR_Msg.FilePtr ) != (size_t)3 )
        {
            BUFR_Err_Set( "BUFR_Position_Msg", "Can't read BUFR file" );
            fprintf(BUFR_Cntl.bufr_log,"%s\n", 
                "BUFR_Position_Msg: Can't read BUFR file");
            return 1;
        }
        else
            message_size = Int3ToInt( tmp );

        /* Seek to where "7777" (the end of the BUFR message) should be. */

        /* 100997 LAH: Added int and long casts */
        n = (int) (starting_position + (long ) message_size - 4);

        /* 100997 LAH: Added long cast */
        if( fseek( BUFR_Msg.FilePtr, (long) n, SEEK_SET ) )
        {
            BUFR_Err_Set( "BUFR_Position_Msg",
                "Can't seek to end of BUFR message" );
            fprintf(BUFR_Cntl.bufr_log,"%s\n", 
                "BUFR_Position_Msg: Can't seek to end of BUFR message");
            return 1;
        }

        (void) fread( buf7, 4, 1, BUFR_Msg.FilePtr );

        if( strncmp( buf7, "7777", 4 ) != 0 )
        {
            /*
             * There is no "777" stringat the end of the message.  This may be
             * a tar file that happens to contain a "BUFR" string followed by
             * a binary number.
             */

            continue;
        } else
        {
            /*
             * This is a BUFR message.  Reposition file pointer to start of
             * the "BUFR" string.
             */

            if( fseek( BUFR_Msg.FilePtr, starting_position, SEEK_SET ) )
            {
                BUFR_Err_Set( "BUFR_Position_Msg",
                    "Can't seek to start of BUFR message" );
                fprintf(BUFR_Cntl.bufr_log,"%s\n", 
                    "BUFR_Position_Msg: Can't seek to start of BUFR message");
                return 1;
            }

            return 0;
        }
    }

    /*
     * "BUFR" string was not found.  Since this isn't actually an error,
     * return 1 to indicate EOF but do not call BUFR_Err_Set().
     */

    /* JRA012897: Reset the file pointer to its position before searching. */

    if( fseek( BUFR_Msg.FilePtr, original_position, SEEK_SET ) )
    {
        BUFR_Err_Set( "BUFR_Position_Msg", "Can't reposition file pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
            "BUFR_Position_Msg: Can't reposition file pointer");
        return 1;
    }

    return 1;
}
