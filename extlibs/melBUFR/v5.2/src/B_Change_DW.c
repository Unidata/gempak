/*
 * BUFR_Change_DataWidth - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Change_DataWidth - Use descriptor 2-01-YYY to change data width.
 * Return 1 on error, else 0.
 *
 * Changes remain in effect until cancelled by calling this function again
 * with a deltaDW value of 0.
 *
 * Changes do not affect Table B CCITT_IA5, code table, or flag table
 * descriptors.
 *
 * CHANGE LOG
 *  022498 LAH:  Added prints to bufr_log file.
 *
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         BUFR_Change_DataWidth
 
  DESCRIPTION:         Use descriptor 2-01-YYY to change data width.
			Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int BUFR_Change_DataWidth( int deltaDW )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  deltaDW		int	   input	Data width change
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Wrong Process Type	Write error message to buffer
                        Return with an error
  Wrong Process Method	Write error message to buffer
                        Return with an error
  Wrong Data width	Write error message to buffer
  Operator              Return with an error
  Too large change 	Write error message to buffer
  value                 Return with an error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  FXY_PtrInc		Increment internal expanded FXY pointer
  FXY_IsTableC		Checks if FXY is a Table C value
  FXY_X_Value		Get X value from FXY
  FXY_Y_Value		Get Y value from FXY
  ValStack_Pop		Pops top-most entry off of a value stack
  ValStack_Push		Pushes an entry onto the value stack
  FXY_Pack		Packs individual F, X, and Ys into a FXY
  DataList_Put          Adds the given value to the data list
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg      BUFR_Msg_t      External message structure
  errbuf        char            error message character string
  y_val		int		Y value of a FXY
  fxy		FXY_t		Packed FXY
  FXY_Val	FXY_t		Next FXY from defined dataset
  X_Val		int		X value of FXY_Val
  Y_Val		int		Y value of FXY_Val
 
  METHOD:
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int BUFR_Change_DataWidth( int deltaDW )

#else

int BUFR_Change_DataWidth( deltaDW )
int         deltaDW;

#endif
{

    int   y_val;
    FXY_t fxy;
    FXY_t FXY_Val;
    int   X_Val, Y_Val;
    char  errbuf[80];


    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Change_DataWidth",
            "Encoding function called while decoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Change_DataWidth", 
	     "Encoding function called while decoding data" );
        return 1;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Change_DataWidth",
                "Can't mix raw and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Change_DataWidth", 
                "Can't mix raw and value-based processing methods." );
            return 1;

        case METHOD_TEMPLATE:

            /*
             * A dataset has already been defined.  Most likely, this function
             * is being called from the same loop that created the dataset in
             * the first place and will be called repeatedly.
             *
             * Ignore the data width specified and get the next FXY from the
             * defined dataset.  If not a change datawidth operator (2-01-yyy)
             * indicate error.
             */

             /* the following code was added by Louis Hembree to correctly
              * handle templates.  Previous version was not doing anything
              * and data values and FXYs got out of sync.
              */

            FXY_Val = BUFR_Msg.exp_ptr->fxy;
            FXY_PtrInc();

            /* test if table C */
            if ( FXY_IsTableC(FXY_Val) )
            {
               X_Val = FXY_X_Value( FXY_Val );
               Y_Val = FXY_Y_Value( FXY_Val );

               /* test if change data width operator, i.e., X val == 1 */
               if ( X_Val != 1 )
               {
                   BUFR_Err_Set("BUFR_Change_DataWidth",
                       "Template Method: Next FXY not 2-01-yyy.");
                   fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                     "BUFR_Change_DataWidth", 
                     "Template Method: Next FXY not 2-01-yyy." );
                   return 1;
                }

                if ( Y_Val != 0 )
                  deltaDW = Y_Val - 128;
            }
            break;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Put function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_ENCODE;
            BUFR_Msg.MethFlag = METHOD_VALUES;
    }

    if( deltaDW < -127 || deltaDW > 127  )
    {
        sprintf( errbuf, "Change value of %d is too large", deltaDW );
        BUFR_Err_Set( "BUFR_Change_DataWidth", errbuf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
           "BUFR_Change_DataWidth", errbuf );
        return 1;
    }

    if( deltaDW == 0 )      /* Cancel previous change */
    {
        /*
         * Pop deltaDW off of DataWidth stack.  If ValStack_Pop() returns
         * a non-zero value it means that the stack is empty.  This isn't
         * a serious error so just return and act as if nothing happened.
         */

#if TRACE_PRINT
        if( BUFR_TraceLevel() )
            fprintf(BUFR_Cntl.bufr_log, "Popping DataWidth stack\n" );
#endif

        if( ValStack_Pop( &BUFR_Msg.DataWidthStack ) )
            return 0;

        y_val = 0;
    } else
    {
        /* Push deltaDW onto DataWidth stack. */

#if TRACE_PRINT
        if( BUFR_TraceLevel() )
            fprintf(BUFR_Cntl.bufr_log,
	             "Pushing %d onto DataWidth stack\n", deltaDW );
#endif

        if( ValStack_Push( &BUFR_Msg.DataWidthStack, deltaDW ) )
            return 1;

        y_val = deltaDW + 128;
    }

    /*
     * Add Table C operator to data list (there is no corresponding
     * encoded value to be put in Section 4).
     */

    fxy = FXY_Pack( 2, 1, y_val );

    return DataList_Put( BUFR_Msg.data_list, &fxy, 1, NULL, 0 );
}
