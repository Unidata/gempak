/*
 * BUFR_Decode - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
#if TRACE_PRINT || DEBUG_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

/* CHANGE LOG
*
* 100897 LAH: Added int cast in section 3 size calculation
*             Added uint_t cast in malloc
* 022498 LAH:  Added prints to bufr_log file.
* 050798 LAH: Extracted code that created FXY list and put in 
*             FXY_List_Get.
*
*/

#if PROTOTYPE_NEEDED

int BUFR_Decode( void )

#else

int BUFR_Decode()

#endif
{

    FXY_t*       fxy_array;     /* Holds unexpanded S3 FXY values. */
    int          fxy_array_len;

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
       fprintf(BUFR_Cntl.bufr_log,
         "BUFR_Decode: Decoding BUFR Message\n" );
#endif

    /**********************************************************
    * Convert Section 3's bitstream into an expanded array of
    * FXY values.
    **********************************************************/
    /*
    * 050798 LAH: Code to vreat FXY list extracted and put in 
    *             FXY_List_Get
    */
    fxy_array_len = FXY_List_Get(&fxy_array);
    
    /* Create an expanded FXY list. */

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Creating expanded FXY list\n" );
#endif

    BUFR_Msg.exp_fxy_list = FXY_List_Expand( fxy_array, fxy_array_len );

    if( BUFR_Msg.exp_fxy_list == NULL )
    {
        BUFR_Err_Log( "BUFR_Decode" );
        free( (void*) fxy_array );
        return 1;
    }

    free( (void*) fxy_array );    /* array is no longer needed. */

#if DEBUG_PRINT
    /* Int2ToInt added 10June 97- Hembree */
    if( BUFR_DebugLevel() > 1 )
    {
        fprintf(BUFR_Cntl.bufr_log, 
          "Expanded S3 FXY List (# datasets=%d, # FXY values=%d)\n",
           Int2ToInt(BUFR_Msg.Section3.data_subsets),
        FXY_List_Size( BUFR_Msg.exp_fxy_list ) );
        fprintf(BUFR_Cntl.bufr_log,
           "------------------------------------\n" );
        FXY_List_Print( BUFR_Msg.exp_fxy_list, BUFR_Cntl.bufr_log );
        fprintf(BUFR_Cntl.bufr_log, "\n" );
    }
#endif

    BUFR_Msg.exp_ptr = FXY_List_First( BUFR_Msg.exp_fxy_list );

    return 0;
}
