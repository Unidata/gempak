/*
 * FXY_PtrInc - VERSION: %I%  %E% %T%
 */
/*
 * FXY_PtrInc - Increment internal expanded FXY pointer used for decoding BUFR
 * messages or encoding templates.
 *
 * If decoding, return 0 on success, 1 on failure, and -1 on bit stream
 * exhaustion.
 *
 * If encoding templates, just return 0.
 */

#include <mel_bufr.h>
extern TableB_t*  TableB;
extern BUFR_Msg_t BUFR_Msg;
#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_PtrInc
 
  DESCRIPTION:         Increment internal expanded FXY pointer used for 
			decoding BUFR messages or encoding templates.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int FXY_PtrInc( void )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
	If decoding, return 0 on success, 1 on failure, and -1 on bit stream
	exhaustion.  If encoding templates, just return 0.

 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
 
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
 
 CHANGE LOG
 
  022498 LAH:  Added prints to bufr_log file.
  
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int FXY_PtrInc( void )

#else

int FXY_PtrInc()

#endif
{

    TableB_Entry_t* BE;

    if( BUFR_ProcType() == TYPE_ENCODE )
    {
        BUFR_Msg.exp_ptr = BUFR_Msg.exp_ptr->next;

        if( BUFR_Msg.exp_ptr == BUFR_Msg.exp_fxy_list->tail )
        {
            if( BUFR_Msg.rebuild_exp_fxy_list )
            {
                /*
                 * TEMPLATE PROCESSING
                 *
                 * The expanded FXY list contained delayed replicators which
                 * were overwritten.  Destroy the expanded list and recreate
                 * it from subset_fxys.
                 */

                FXY_List_Destroy( BUFR_Msg.exp_fxy_list );

                BUFR_Msg.exp_fxy_list = FXY_List_Expand( BUFR_Msg.subset_fxys,
                    BUFR_Msg.subset_size );

                if( BUFR_Msg.exp_fxy_list == NULL )
                {
                    BUFR_Err_Log( "FXY_PtrInc" );
                    return 1;
                }
            }

            BUFR_Msg.exp_ptr = BUFR_Msg.exp_fxy_list->head->next;
            BUFR_Msg.subset_index++;
        }

        return 0;
    }

    BUFR_Msg.exp_ptr = BUFR_Msg.exp_ptr->next;

    if( BUFR_Msg.exp_ptr == BUFR_Msg.exp_fxy_list->tail )
    {
        /*
         * Expanded FXY list is exhausted. See if all data subsets have been
         * processed.  If so, return -1 to indicate input exhaustion.  If not,
         * clear all stacks (to reset any redefined FXY values), rewind the
         * Section 3 bit stream, and call BUFR_Decode() to rebuild the
         * expanded FXY list (in case there were delayed replicators contained
         * within it).
         */

        BUFR_Msg.subset_index++;

/*        if( BUFR_Msg.subset_index > (int) BUFR_Msg.Section3.data_subsets ) */
        if( BUFR_Msg.subset_index > Int2ToInt(BUFR_Msg.Section3.data_subsets) )
        {
            /*
             * No more input to decode.  Set file status flag to indicate
             * EOM if more BUFR messages remain in the file, otherwise set
             * it to EOF.
             */

            if( BUFR_Msg.FileStatus == BUFR_EOF )
                BUFR_Msg.MsgStatus = BUFR_EOF;
            else
                BUFR_Msg.MsgStatus = BUFR_EOM;

#if TRACE_PRINT
            if( BUFR_TraceLevel() > 1 )
            {
                fprintf(BUFR_Cntl.bufr_log, "End of message reached, " );
                printf("End of message reached, " );

                if( BUFR_Msg.MsgStatus == BUFR_EOM ){
                    fprintf(BUFR_Cntl.bufr_log,
                        "more messages remain in the file.\n" );
                    printf( "more messages remain in the file.\n" );
                } else {
                    printf( "there are no more messages in the file.\n" );
                    fprintf(BUFR_Cntl.bufr_log,
                        "there are no more messages in the file.\n" );
                }
            }
#endif

            return -1;
        }

        /*
         * Set the message flag to indicate that the end of the dataset was
         * passed.
         */

        BUFR_Msg.MsgStatus = BUFR_EOD;

#if TRACE_PRINT
        if( BUFR_TraceLevel() > 1 )
            fprintf(BUFR_Cntl.bufr_log, "Passed end of dataset.  Rewinding bitstream\n" );
#endif

        (void) BitStream_Rewind( &BUFR_Msg.Section3_Data );

        /*
         * Restore all data widths, scales, and reference values
         * to their original values.
         */

        ValStack_Clear( &BUFR_Msg.DataWidthStack );
        ValStack_Clear( &BUFR_Msg.ScaleStack );

        /* Reset all reference values. */

        for( BE=TableB->head->next; BE != TableB->tail; BE=BE->next )
            ValStack_Clear( &BE->item->RefValStack );

        /* Call BUFR_Decode() to rebuild expanded FXY list. */

        FXY_List_Destroy( BUFR_Msg.exp_fxy_list );

        if( BUFR_Decode() )
        {
            BUFR_Err_Log( "FXY_PtrInc" );
            return 1;
        }

        /* BUFR_Msg.MsgStatus = BUFR_OK; JRA082896 - Erases EOD status */
    } else
    {
        BUFR_Msg.MsgStatus = BUFR_OK;
    }

    return 0;
}
