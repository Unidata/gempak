/*
 * TableD_Read - VERSION: %I%  %E% %T%
 */
/*
 * TableD_Read - Read descriptors for Code Table D from the given filename.
 * Return 1 on error, else 0.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;
extern TableD_t* TableD;  /* Make sure you always have the front end */
			  /* of the table available */

#define MAX_BUF 1024

#define SEQUENCE_TERMINATOR -1



/*
..............START PROLOGUE....................................
 
  MODULE NAME:         TableD_Read
 
  DESCRIPTION:         Read descriptors for Code Table D from the given 
			filename.  Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int TableD_Read( char* FileName )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FileName              Char        In          Input file name
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Filename is NULL      Write error message to buffer
                        Return with an error
  File doesn't open     Write error message to buffer
                        Return with an error
  Not enough descriptors	Write error message to buffer
  Not valid FXY         Return with an error
  Not TableD FXY
  Duplicate descriptor	Write error message to buffer
                        Return with an error
  Bad return from	Write error message to buffer
  TableD_Sequence_Init  Return with an error 
  Bad return from	Write error message to buffer
  TableD_Sequence_Put   Return with an error 

  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  BUFR_IsComment        Check to see if line is a BUFR table comment line
  FXY_Pack              Returns packed values of F, X, and Y
  FXY_IsTableD          Returns a 1 if given descriptor is in TableD
  TableD_Match		Checks for descriptor duplication
  TableD_Sequence_Init	Creates new descriptor sequence list
  TableD_Sequence_Put	Adds a new entry to a give descriptor sequence
  TableD_Sequence_Destroy
			Destroy a Table D sequence
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  fp            FILE            File pointer
  buf           char            Character array for diagnostic messages
  line          int             loop counter
  f             int             The F value of an FXY
  x             int             The x value of an FXY
  y             int             The y value of an FXY
  n		int		Number of items scanned
  FXY_Val	FXY_t		Packed FXYs
  DSeq		TableD_Sequence_t	New Table D sequence list
  counter	int		Informative counter 
  TableD	TableD_t	External Table D
  DS		TableD_Sequence_t	Found duplicate sequence
  tde		TableD_Sequence_t	Pointer to after the duplicate
  tde_prev	TableD_Sequence_t	Pointer to before the duplicate
 
  METHOD:
        If the input filename is empty then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
        If there is an open error then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
        Loop while reading the file until the file is empty
          If the current line is a comment then
            drop to the bottom of the loop
          Endif
	  Perform sscanf to get f, x, and y value
	  Take the absolute value of x and y
	  If 3 values were not scanned or performing FXY_Pack creates a bad
		FXY_Value or the value is not a Table D value then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Endif
	  If TableD_Match finds a duplicate descriptor then
          if redefinition allowed then
            Loop to find the Table D entry that needs to be redefined
            End loop
            Remove the Table D entry and perfrom free to free up memory
            Deallocate the descriptor
	    if warning flag set then
	        print warning to log file
	    Endif
          else
               Perform BUFR_Err_Set to write error message
               Close the file
               Return with error
          Endif
	  If TableD_Sequence_Init cann't create a descriptor sequence list then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Endif
	  Loop until file is read
            If the current line is a comment then
              drop to the bottom of the loop
            Endif
	    Perform sscanf to get f, x, and y value
	    Take the absolute value of x and y
	    If 3 values were not scanned or performing FXY_Pack creates a bad
		FXY_Value or the value is not a Table D value then
              Perform BUFR_Err_Set to write error message
              Close the file
              Return with error
            Endif
	    If TableD_Sequence_Put cann't put the list in the sequence then
              Perform BUFR_Err_Set to write error message
              Close the file
              Return with error
            Endif
	  End Loop
	End Loop
	Close file
	Return with no error
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:

    092997  LAH: Added char cast to correct Linux warning 
    121997  VLP: Add capability to redefine Table D Descriptors
    022398  LAH:  - Add reference to BUFR_Cntl to control printing of 
                  of warning messages.
                  - Modified to remove conditional compile to 
                  allow redefinition of table entries.  Now controlled by
                  BUFR_Cntl.Dup_Tab_Entry.  1 = allow duplication, 
                  0 = disallow duplication
    022498 LAH:  Added prints to bufr_log file.

..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int TableD_Read( char* FileName )

#else

int TableD_Read( FileName )
char*       FileName;

#endif
{
    FILE* fp;
    int   line;
    char  buf[MAX_BUF];
    int   n;

    int                f, x, y, counter;
    FXY_t              FXY_Val;
    TableD_Sequence_t* DSeq;


    TableD_Sequence_t* DS; 	/* Use this to find the descriptor sequence */
    TableD_Sequence_t *tde, *tde_prev;  /* Before and after the sequence */

    /*  092997  LAH: Added cast to correct Linux warning */
    if( FileName == NULL && *FileName == (char) NULL )
    {
        BUFR_Err_Set( "TableD_Read", "NULL Table D filename given" );
        fprintf(BUFR_Cntl.bufr_log, 
           "TableD_Read: NULL Table D filename given\n");
        return 1;
    }

    if( (fp = fopen( FileName, "r" )) == NULL )
    {
        sprintf( buf, "Can't open Table D file \"%s\"", FileName );
        BUFR_Err_Set( "TableD_Read", buf );
        fprintf(BUFR_Cntl.bufr_log, "TableD_Read (%s)\n", FileName );
       return 1;
    }
    fprintf(BUFR_Cntl.bufr_log, "Reading Table D (%s)\n", FileName );

    /****************************/
    /* Collect FXY descriptors. */
    /****************************/

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
    {
        fprintf(BUFR_Cntl.bufr_log, "Collecting Table D FXY values\n" );
    }
#endif

    for( line=1; fgets( buf, MAX_BUF, fp ) != NULL; line++ )
    {
        if( BUFR_IsComment( buf ) )
            continue;

        /* Get F, X, and Y portion and compute table location */

        n = sscanf( buf, "%d%d%d", &f, &x, &y );

        /* Negate X and Y values if delimited with '-' (e.g. 3-02-001) */

        x = abs( x );
        y = abs( y );

        if( n != 3 || (FXY_Val=FXY_Pack(f,x,y)) == (FXY_t)BAD_FXY_VAL ||
            !FXY_IsTableD(FXY_Val) )
        {
            sprintf( buf, "\t%s, Line %d: Invalid Table D descriptor",
                FileName, line );

            BUFR_Err_Set( "TableD_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableD_Read (%s)\n", FileName );
            fclose( fp );
            return 1;
        }

#if DEBUG_PRINT
        if( BUFR_DebugLevel() > 8 )
            fprintf(BUFR_Cntl.bufr_log, "%s\n", FXY_String( FXY_Val ) );
#endif

        /* Check for descriptor duplication. */

        if( TableD_Match( FXY_Val ) != NULL )
        {
           /*
	   * LAH 022398:  Modified to remoce conditional compile to 
	   * allow redefinition of table entries.  Now controlled by
	   * BUFR_Cntl.Dup_Tab_Entry.  1 = allow duplication, 
	   * 0 = disallow duplication
	   */
           if (BUFR_Cntl.Dup_Tab_Entry)
	   {
 	     counter = 0;
    	     for( DS=TableD->head; DS != TableD->tail; DS=DS->next )
    	     {
	        counter++;
                if( DS->next->head->fxy_value == FXY_Val )
                break;
    	     }
	     if (BUFR_Cntl.Dup_Tab_Entry_Warn)
	     {
	       fprintf(BUFR_Cntl.bufr_log,
	           " found a duplicate Table D at %d with %s \n",
	           counter, FXY_String(FXY_Val));
	     }

             /**********************************************************
             * Remove this Table D entry so that the redefined one will
	     * be added later.
             **********************************************************/

             /* Find Table D linked list entry and the one before it. */

             tde_prev = DS;
             tde      = DS->next;

             /*
              * Remove the Table D entry so that the redefined one will be
              * added later.
              */

             tde_prev->next = tde->next;
             (void) TableD_Sequence_Destroy( tde );  
             free( (void*) tde );
           } else {
             sprintf( buf,
             "%s, Line %d: Duplicate Table D descriptor %s",
             FileName, line, FXY_String( FXY_Val ) );

             BUFR_Err_Set( "TableD_Read", buf );
             fprintf(BUFR_Cntl.bufr_log, "TableD_Read (%s)\n", FileName );
             (void) fclose( fp );
             return 1;
	  }
	}
        /*
         * Found the start of a descriptor sequence.  Create a new
         * descriptor sequence list (with this FXY value stored in
         * the head and tail) and gobble FXY values until F == -1.
         * Since an FXY value can be an index to a Table D descriptor,
         * don't expand 3-XX-YYY values until all Table D descriptors have
         * been read.
         */

        if( (DSeq = TableD_Sequence_Init( FXY_Val )) == NULL )
        {
            sprintf( buf, "Can't create sequence for descriptor %s",
                FXY_String( FXY_Val ) );

            BUFR_Err_Set( "TableD_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableD_Read (%s)\n", FileName );
            fclose( fp );
            return 1;
        }

        for( line++; fgets( buf, MAX_BUF, fp ) != NULL; line++ )
        {
            if( BUFR_IsComment( buf ) )
                continue;

            n = sscanf( buf, "%d%d%d", &f, &x, &y );

            if( f == SEQUENCE_TERMINATOR )
                break;

            /* Negate X and Y values if delimited with '-' (e.g. 3-02-001) */

            x = abs( x );
            y = abs( y );

            if( n !=3 || (FXY_Val=FXY_Pack(f,x,y)) == (FXY_t)BAD_FXY_VAL )
            {
                sprintf( buf, "\t%s, Line %d: Invalid descriptor",
                    FileName, line);

                BUFR_Err_Set( "TableD_Read", buf );
                fprintf(BUFR_Cntl.bufr_log,
		    "TableD_Read (%s)\n", FileName );
                fclose( fp );
                return 1;
            }

#if DEBUG_PRINT
            if( BUFR_DebugLevel() > 8 )
            {
                fprintf(BUFR_Cntl.bufr_log, "\t" );
                FXY_Print( FXY_Val, BUFR_Cntl.bufr_log );
                fprintf(BUFR_Cntl.bufr_log, "\n" );
            }
#endif

            if( TableD_Sequence_Put( DSeq, FXY_Val ) )
            {
                BUFR_Err_Set( "TableD_Read",
		      "Can't add descriptor to list" );
                fprintf(BUFR_Cntl.bufr_log,
		    "TableD_Read (%s)\n", FileName );
                (void) fclose( fp );
                return 1;
            }
        }
    }

    (void) fclose( fp );

    return 0;
}
