/*
 * main - VERSION: %I%  %E% %T%
 */
/*
 * program for decoding a BUFR message.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         bufr_dump
 
  DESCRIPTION:         Program for decoding BUFR message(s).
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    V.L. Pastor, modified from mb_dump 
				written by: J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int main( int argc, char* argv[] )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  arg			int	    in		Number of arguments in the
						command line
  argc			char	    in		Command line arguments
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Could not initialize	Write error message and exit program
  BUFR message struct
  Could not get		Write error message, nullify pointers, close
  decoded value		files, and exit the program
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Debug		Sets the debug flag and level of debug
  BUFR_Trace		Sets the trace flag and level of trace
  FileExists		Checks to see if a file exists
  BUFR_Info_Init	Initialize a BUFR_Info_t structure with default values
  BUFR_Init		Initializes and reads a BUFR message
  BUFR_Get_Value	Gets the next decoded value from a BUFR message
  BUFR_perror		Makes sure all error messages have been printed and
			flushes the buffer
  BUFR_Destroy		Frees pointers
  BUFR_Val_Print	Prints the contents of a BUFR_Val_t
  BUFR_Err_Print	Prints BUFR error messages to standard error
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BInfo		BUFR_Info_t	Structure holding section 0 & 1 information
  i		int		loop variable
  n		int		BUFR error flag
  jcount	int		Number of data sets processed
  range1	int		lower range of data sets
  range2	int		upper range of data sets
  no_sets	int		Total number of data sets in a message
  BUFR_File	char		BUFR message file name
  bv		BUFR_Val_t	Structure for a data values
 
  METHOD:
	If the user entered a command line argument then
	  Loop on the number of arguments
 	    If the argument is a switch then
	      If the argument is a -d the perform BUFR_Debug
	      If the argument is a -t the perform BUFR_Trace
	    Endif
	    If there a file name has not been filled then
	      Perform FileExists to check to see if the argument is a filename
	      If this is a filename then 
		copy the argument to a variable
	      Else
		Print warning message
	      Endif
	    Endif
	  End Loop
	Endif

	If no file name was given then
	  Request a file name from the user
	Endif

	DECODE_START		(Goto place mark)

	Perform BUFR_Info_Init to initialize the BUFR message structure
	If the structure did not initialize then print warning message
	Perform BUFR_Init to read a message and store the information in BInfo
	If the  message was not read then
	  Print error message
	  Exit the program
	Endif

	Tell user how many data sets there are in the message
	Ask the user for a range of data sets to print out

	Loop while there are data points
	  If the data point is an error then
	    Print error message
	    Free all of the pointers
	    Exit the program
	  Endif
	  If we are in the user requested data set range then
	    PerformBUFR_Val_Print to print out the data value
	  Endif

	  If the end of a dataset has been reached then
	    Print out End of dataset only if in the user requested range
	    increment the range counter
	  Endif
	End loop

	If there is another message in the file then
	  Print some end of message banners
	  Perform BUFR_Destroy to free pointers
	  Go to DECODE_START
	Endif

	If the message is at the end of file then
	  Print end of file message
	Else
	  Print warning message
	Endif

	Perform BUFR_Err_Print to flush the buffers

	Perform BUFR_Destroy to free the pointers

	Exit the program

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    052198 LAH:  Added calls to set flag
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int main( int argc, char* argv[] )

#else

int main( argc, argv )
int   argc;
char* argv[];

#endif
{
    BUFR_Info_t BInfo;
    char iyn;
    int DONE;
    int  i, interactive = 0, n, out_flag;
    int  data_count, range1, range2;
    int no_sets;
    char BUFR_File[256];    /* Big enough to hold any file name. */
    BUFR_Val_t bv;

    /*
     * Get the BUFR message file from the command line.  If there are
     * no arguments on the command line, prompt for the file name.
     */

    BUFR_File[0] = '\0';
    out_flag = 0;

    if( argc > 1 )
    {
        /*
         * Search for file name in argument list.  If a "-dnn" or "-tnn"
         * (where "nn" is an integer) appears, set the debug or trace
         * flag to the given number.  If -O[s|f] set the output, it directs 
         * the output to either stdout [s] or to the log file [f].  The default
         * is to a log file.  The log file name is created in the program output
         * stdout.
         */

        for( i=1; i < argc; i++ )
        {
            if( argv[i][0] == '-' )
            {
                switch( argv[i][1] )
                {
                    case 'D':
                    case 'd':
			if( (n = atoi( &argv[i][2] )) < 1 )    /* No number given */
			    n = 1;
                        BUFR_Debug( n );    /* Set debug flag level */
                        continue;           /* Get next argument    */

		    case 'I':
		    case 'i':
			interactive = 1;
			continue;
                    case 'T':
                    case 't':
			if( (n = atoi( &argv[i][2] )) < 1 )    /* No number given */
			    n = 1;
                        BUFR_Trace( n );    /* Set trace flag level */
                        continue;           /* Get next argument    */

                    case 'O':    /* set output */
                    case 'o':
                       switch(argv[i][2])
                       {
                          case 'S':    /* output goes to stdout */
                          case 's':
                            out_flag = 1; 
                            continue;
                          case 'F':   /* output goes to bufr[pid].log */
                          case 'f':
                            out_flag = 0; 
                            continue;
                       }
                }
            }

            if( BUFR_File[0] == (char)NULL )
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

    if( BUFR_File[0] == (char)NULL ) {
        printf( "Enter BUFR message file name: " );
        scanf( " %s", BUFR_File );
    }
    /*
     * Initialize the BUFR message structure and store information about
     * the message in "BInfo."
     */

    if ( BUFR_Info_Init(&BInfo) ) { 
       printf(" >>>> Could not initilize BUFR info structure >>>>>\n");
    }
DECODE_START:

    /*
     * LAH 022398 Added to set corresponding flags
     */
    Set_Flag(NoAuto_FTP);
    Set_Flag(Allow_Dup_Tab_Entry);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    if ( out_flag == 1) {
       Set_Flag(BUFR_LOG_TO_stdout);   
    }
    else {
       Set_Flag(BUFR_LOG_TO_bufr_log_pid); 
    }
    Set_Flag(Print_New_Table_Entries);
    Set_Flag(Dump_Data_Type_11_Msg);  
/*
    Set_Flag(Do_Not_Print_New_Table_Entries);
    Set_Flag(Do_Not_Dump_Data_Type_11_Msg);
*/

    if ( BUFR_Init( &BInfo, BUFR_File, DECODING ) ) {
        BUFR_perror( "main" );
        return 1;
    }

    if ( interactive == 1 )  printf("log file = %s\n", Get_BUFR_Log_File());
/*  Print the Section header information and the Section 3 fxy info */
/*  If you do not want the header info to be dumped comment out the */
/*  next line */
/*    dump_print( interactive, 0, 0, stdout, &no_sets ); */
    dump_print( interactive, 0, 0, BUFR_Cntl.bufr_log, &no_sets ); 
    no_sets = BUFR_NumDatasets();
/*	Ask the user what range of data sets he wishes to look at */
/*  LAH 112000 - changed code to run from 1 to number of data sets */
/*      instead of from zero to number of data sets - 1 */
    if ( interactive == 0 ) {
	range1 = 1;
	range2 = no_sets;
    }
    else {
	printf(" There are %d data sets.  Range 1 to %d.\n",no_sets, no_sets);
	printf(" Enter the range you wish.\n");
	printf(" Enter lower range:  ");
	scanf(" %d",&range1);
	printf(" Enter higher range:  ");
	scanf(" %d",&range2);
    }
/* initialize the data count */
    data_count = 1;

    /* Get each value (and any associated fields) from the BUFR message. */
    while( (n=(int)BUFR_Get_Value( &bv, 1 )) != BUFR_EOM && n != BUFR_EOF ) {
        if ( n == BUFR_ERROR || n == BUFR_EOF ) { /* same as if( BUFR_IsError() ) */
	    printf( "bd-Error getting decoded value!\n" );

/* Print the reason for the error and exit. */

	    BUFR_perror( "main" );
	    BUFR_Destroy(1);
	    return 1;
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

/*  Unless the dataset is within the user requested range 
     	don't print anything */

	if ( data_count >= range1 && data_count <= range2 ) {
/* need to test for IGNORE_FXY.  This is used to indicate that */
/* information that was returned should be ignored.  This happens */
/* when a local descriptor that is not defined in Table B is */
/* the last descriptor in the data set.  */
	    if ( bv.FXY_Val != (FXY_t)IGNORE_FXY) {
		BUFR_Val_Print( bv, 1, 0, BUFR_Cntl.bufr_log ); 
	    }
	}
	if( n == BUFR_EOD ) {     /* same as if( BUFR_AtEOD() ) */
	    if ( data_count >= range1 && data_count <= range2 ) { 
	        if ( interactive == 1 ) 
		    printf( "--- END OF DATASET ---  %d\n\n",data_count);
	    }
	    if ( BUFR_Cntl.bufr_log != stdout ) {
	        fprintf( BUFR_Cntl.bufr_log, "--- END OF DATASET ---  %d\n\n",data_count);
	    }
/* This decoded value is the last one in the dataset. */
	    data_count++;
	    if ( data_count > range2 ) {
	         n = Set_Status();
	         break; 
	    }
	}
    }

    if( n == BUFR_EOM )     /* same as if( BUFR_AtEOM() ) */
    {
        n = Set_Status();
        /* There's another message to read. */

	if ( interactive == 1 ) {
	    printf( "*** END OF MESSAGE ***\n" );
	    printf( "*** END OF MESSAGE ***\n\n" );
	}
        if ( BUFR_Cntl.bufr_log != stdout ) {
           fprintf( BUFR_Cntl.bufr_log, "--- END OF MESSAGE ---  %d\n\n",data_count);
        }
 
	n = BUFR_Find_End();
	if ( n == 0 ) {
	    if ( interactive == 0 ) {
		iyn = 'y';
	    }
	    else {
		printf(" There is another data set in the file.\n");
		printf(" Do you want to dump it?  (y/n):  ");
		DONE = 0;
		while ( !DONE ) {
		    scanf(" %c",&iyn);
		    if ( iyn == 'y' || iyn == 'n' )
			DONE = 1;
		    else
			printf(" wrong answer %c must be y or n ", iyn);
	  	}
	    }
	    if ( iyn == 'y') {
		goto DECODE_START;
	    }
	    else {
		BUFR_Destroy(1);
		fclose(BUFR_Cntl.bufr_log);
		return 0;
	    }
	}
	else
	    n = BUFR_EOF;
    }

    if( n == BUFR_EOF )     /* same as if( BUFR_AtEOF() ) */
    {
 
      fprintf( BUFR_Cntl.bufr_log, "--- END OF FILE ---  %d\n\n",data_count);
    } else {
      printf( "Unknown value (%d) returned by BUFR_Get_Value()\n", n );
    }
    BUFR_Err_Print( NULL );

    BUFR_Destroy(1);
    fclose(BUFR_Cntl.bufr_log);

    return 0;
}
