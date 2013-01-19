/*
 * main - VERSION: %I%  %E% %T%
 */
/*
 * Skeleton program for decoding a BUFR message.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int main( int argc, char* argv[] )

#else

int main( argc, argv )
int   argc;
char* argv[];

#endif
{
    BUFR_Info_t BInfo;

    int  i, n;
    char BUFR_File[256];    /* Big enough to hold any file name. */

    BUFR_Val_t bv;

    /* Flags for BUFR_Val_Print(). */

    int print_des = 1;      /* Print description. */
    int print_af =  0;      /* Print associated fields. */

    /*
     * Get the BUFR message file from the command line.  If there are
     * no arguments on the command line, prompt for the file name.
     */

    BUFR_File[0] = '\0';

    if( argc > 1 )
    {
        /*
         * Search for file name in argument list.  If a "-dnn" or "-tnn"
         * (where "nn" is an integer) appears, set the debug or trace
         * flag to the given number.
         */

        for( i=1; i < argc; i++ )
        {
            if( argv[i][0] == '-' )
            {
                if( (n = atoi( argv[i] )) < 1 )     /* No number given */
                    n = 1;


                switch( argv[i][1] )
                {
                    case 'D':
                    case 'd':
                        BUFR_Debug( n );    /* Set debug flag level */
                        continue;           /* Get next argument    */

                    case 'T':
                    case 't':
                        BUFR_Trace( n );    /* Set trace flag level */
                        continue;           /* Get next argument    */

                    case 'A':               /* Print associated fields */
                    case 'a':               /* for decoded value. */
                        print_af = 1;
                        continue;
                }
            }

            if( BUFR_File[0] == '\0' )
            {
                /*
                 * If this argument is a valid file name, copy the argument to
                 * "BUFR_File" and continue parsing in case any remaining args
                 * are flags.
                 */

                if( FileExists( argv[i] ) )
                    strcpy( BUFR_File, argv[i] );
                else
                    printf( "%s: No such file\n", argv[i] );
            }
        }
    }

    if( BUFR_File[0] == '\0' )
    {
        printf( "Enter BUFR message file name: " );
        scanf( "%s", BUFR_File );
        if( feof( stdin ) )         /* User pressed ^D */
        {
            printf( "\nABORTED\n" );
            return 1;
        }
    }

DECODE_START:

    /*
     * Initialize the BUFR message structure and store information about
     * the message in "BInfo."
     */

    if ( BUFR_Info_Init(&BInfo) )
    {
        printf(" >>>> Could not initilize BUFR info structure >>>>>\n");
        BUFR_perror( "main" );
        return 1;
    }

    if( BUFR_Init( &BInfo, BUFR_File, DECODING ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

    /*
     * To print BUFR information call one of the following functions:
     *
    BUFR_Info_Print( BInfo, stdout );
    BUFR_Print( 0, 0, stdout );
     *
     * BUFR_Info_Print() prints the contents of BInfo and BUFR_Print()
     * prints the contents of the internal BUFR message structure.  Since
     * BUFR_Print() calls BUFR_Info_Print() there's no need to call both
     * functions.
     *
     */

    if( BUFR_DebugLevel() > 0 )
        BUFR_Print( 0, 0, stdout );

    /* Get each value (and any associated fields) from the BUFR message. */

    while( (n=BUFR_Get_Value( &bv, 1 )) != BUFR_EOM && n != BUFR_EOF )
    {
        if( n == BUFR_ERROR )   /* same as if( BUFR_IsError() ) */
        {
            printf( "Error getting decoded value!\n" );

            /* Print the reason for the error and exit. */

            BUFR_perror( "main" );
            BUFR_Destroy(1);
            return 1;
        }

        if( n == BUFR_EOD )     /* same as if( BUFR_AtEOD() ) */
        {
            /*
             * The end of the dataset has been reached.  The decoded
             * value is garbage but more data remains to be decoded.
             */

            /* This decoded value is the last one in the dataset. */

            printf( "--- END OF DATASET ---\n" );
            continue;
        }

        /*********************************************************************
         *                      PROCESS VALUE
         *
         * A value has been successfully retrieved from the BUFR message.
         * This is the point in the code where one should do something useful
         * with the value.  Since this is a skeleton program, just print the
         * contents of the decoded value and get the next one.
         *
         ********************************************************************/

        BUFR_Val_Print( bv, print_des, print_af, stdout );
        /* fflush( stdout ); */
    }

    if( n == BUFR_EOM )     /* same as if( BUFR_AtEOM() ) */
    {
        /* There's another message to read. */

        printf( "*** END OF MESSAGE ***\n" );
        BUFR_Destroy(1);
        goto DECODE_START;
    }

    if( n == BUFR_EOF )     /* same as if( BUFR_AtEOF() ) */
        printf( "*** END OF FILE ***\n" );
    else
        printf( "Unknown value (%d) returned by BUFR_Get_Value()\n", n );

    BUFR_Destroy(1);

    return 0;
}
