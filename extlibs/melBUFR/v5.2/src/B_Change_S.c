/*
 * BUFR_Change_Scale - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Change_Scale - Use descriptor 2-02-YYY to change scale.
 * Return 1 on error, else 0.
 *
 * Changes remain in effect until cancelled by calling this function again
 * with a deltaS value of 0.
 *
 * Changes do not affect Table B CCITT_IA5, code table, or flag table
 * descriptors.
 *
 * CHANGE LOG
 *
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int BUFR_Change_Scale( int deltaS )

#else

int BUFR_Change_Scale( deltaS )
int         deltaS;

#endif
{

    int   y_val;
    FXY_t fxy;
    FXY_t FXY_Val;
    int   X_Val, Y_Val;
    char  errbuf[80];

#if TRACE_PRINT
#endif

    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Change_Scale",
            "Encoding function called while decoding data" );
        return 1;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Change_Scale",
                "Can't mix raw and value-based processing methods." );
            return 1;

        case METHOD_TEMPLATE:

            /*
            * A dataset has already been defined.  Most likely, this function
            * is being called from the same loop that created the dataset in
            * the first place and will be called repeatedly.
            *
            * Ignore the scale specified and get the next FXY from the
            * defined dataset.  If not a change scale operator (2-01-yyy)
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

                /* test if change scale operator, i.e., X val == 2 */

                if ( X_Val != 2 )
                {
                    BUFR_Err_Set("BUFR_Change_DataWidth",
                        "Template Method: Next FXY not 2-01-yyy.");
                    return 1;
                }

                if ( Y_Val != 0 )
                    deltaS = Y_Val - 128;
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

    if( deltaS < -127 || deltaS > 127  )
    {
        sprintf( errbuf, "Change value of %d is too large", deltaS );
        BUFR_Err_Set( "BUFR_Change_Scale", errbuf );
        return 1;
    }

    if( deltaS == 0 )      /* Cancel previous change */
    {
        /*
         * Pop deltaS off of Scale stack.  If ValStack_Pop() returns
         * a non-zero value it means that the stack is empty.  This isn't
         * a serious error so just return and act as if nothing happened.
         */

#if TRACE_PRINT
        if( BUFR_TraceLevel() )
            fprintf(BUFR_Cntl.bufr_log, "Popping Scale stack\n" );
#endif

        if( ValStack_Pop( &BUFR_Msg.ScaleStack ) )
            return 0;

        y_val = 0;
    }
    else
    {
        /* Push deltaS onto Scale stack. */

#if TRACE_PRINT
        if( BUFR_TraceLevel() )
            fprintf(BUFR_Cntl.bufr_log, "Pushing %d onto Scale stack\n", deltaS );
#endif

        if( ValStack_Push( &BUFR_Msg.ScaleStack, deltaS ) )
            return 1;

        y_val = deltaS + 128;
    }

    /*
     * Add Table C operator to data list (there is no corresponding
     * encoded value to put in Section 4).
     */

    fxy = FXY_Pack( 2, 2, y_val );

    return DataList_Put( BUFR_Msg.data_list, &fxy, 1, NULL, 0 );
}
