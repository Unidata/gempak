/*
 * BUFR_Get_Dataset - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Get_Dataset - Allocate space for and return an array containing
 * data corresponding to the requested dataset number.  Return 0 on error,
 * otherwise the number of values allocated or EOD, EOM, or EOF.
 *
 * NOTES:
 *
 * 1) Dataset numbering starts at 1.
 *
 * 2) Do not use this function with other BUFR_Get functions.
 *
 * 3) This function is NOT a random-access dataset retriever.  Datasets must
 *    be retrieved sequentially.  For example, if a BUFR message contains 7
 *    datasets and the first dataset requested is 3, datasets 1 and 2 cannot
 *    later be retrieved -- only datasets 4 to 7 can be.
 *
 * 4) Memory allocated to each BUFR_Val_t in the given array must be freed
 *    manually or with a call to BUFR_Val_Array_free().
 *
 **********************************************************************
 *  CHANGE LOG
 *
 *  092997 LAH: corrected print statement
 *
 *  100897 LAH: Added int and uint_t casts
 *
 *  121297 LAH:  Modified code tocorrectly determine and adjust
 *                   size of allocated ValArray.  Required modifications
 *                   to BUFR_Vat_t and FXY expand,  put and insert
 *                   functions.
 * 022498 LAH:  Added prints to bufr_log file.
 * 020500 LAH:  Changed dataset from an array to a linked list.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Get_Dataset( int DatasetNum, BUFR_DataSet_t *DataSet, int IgnoreAFs )

#else

int BUFR_Get_Dataset( DatasetNum, DataSet, IgnoreAFs )
int          DatasetNum;    /* INPUT:  The dataset number to get. */
BUFR_DataSet_t* DataSet;      /* OUTPUT: Pointer to allocated array. */
int          IgnoreAFs;     /* INPUT:  Don't process associated fields. */

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    BUFR_Val_t  bv;
    int vals_assigned;

    int i, n;
    char errbuf[80];    /* buffer for error messages. */

    if( BUFR_Msg.Section4_Data.buffer == NULL )
    {
        BUFR_Err_Set( "BUFR_Get_Dataset",
           "BUFR_Decode() hasn't been called" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset", 
           "BUFR_Decode() hasn't been called" );
        return 0;
    }

    if( BUFR_ProcType() == TYPE_ENCODE )
    {
      BUFR_Err_Set( "BUFR_Get_Dataset",
            "Decoding function called while encoding data" );
      fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset", 
            "Decoding function called while encoding data" );
      return 0;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Get_Dataset",
                "Can't mix raw and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset", 
                "Can't mix raw and value-based processing methods.");
            return 0;

        case METHOD_TEMPLATE:
            BUFR_Err_Set( "BUFR_Get_Dataset",
              "Can't mix template and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset:", 
              "Can't mix template and value-based processing methods.");
            return 0;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Get function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_DECODE;
            BUFR_Msg.MethFlag = METHOD_VALUES;
    }

    /* Check arguments. */

    if( DatasetNum < 1 )
    {
        sprintf( errbuf, "Dataset number of %d is invalid", DatasetNum );
        BUFR_Err_Set( "BUFR_Get_Dataset", errbuf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset:", errbuf);
        return 0;
    }

    /*  092997  LAH:  corrected error in format   */
    if( DatasetNum > BUFR_NumDatasets() )
    {
        /* 092997 LAH:  Corrected number of arguments & format missmatch */
        sprintf( errbuf, "%s %d %s (%d) %s.",
            "Dataset number of",              DatasetNum,
            "exceeds the number of datasets", BUFR_NumDatasets(),
            "in BUFR message" );

        BUFR_Err_Set( "BUFR_Get_Dataset", errbuf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset", 
                errbuf);
        return 0;
    }

    if( DatasetNum < BUFR_Msg.subset_index )
    {
        sprintf( errbuf, "Dataset %d has already been retrieved",
            DatasetNum );
        BUFR_Err_Set( "BUFR_Get_Dataset", errbuf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset:", 
                errbuf);
         return 0;
    }

    if( DataSet == NULL )
    {
        BUFR_Err_Set( "BUFR_Get_Dataset", "NULL DataSet pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset:", 
                "NULL DataSet pointer");
        return 0;
    }

    if ( DataSet->head != DataSet->last) BUFR_DataSet_Empty(DataSet);

    /* Position BUFR message to beginning of requested dataset. */

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Positioning to dataset %d\n",
	    DatasetNum );
#endif

    n = BUFR_EOF;
    for( i=BUFR_Msg.subset_index; i != DatasetNum; i++ )
    {
        while( 1 )
        {
            n = (int) BUFR_Get_Value( &bv, 1 );

            if( n == BUFR_EOD || n == BUFR_EOF )
                break;
        }
    }

    if(n == BUFR_EOF ) return n;

    /* Fill DataSet */

    vals_assigned = 0;
    
    /* 12/12/97 LAH:  replaced for loop with while loop */
    while ( 1 )
    {
        if( (n= (int)BUFR_Get_Value( &bv, IgnoreAFs )) != BUFR_OK && n != BUFR_EOD )
        {
            if( n == BUFR_ERROR )
            {
                BUFR_Err_Log( "BUFR_Get_Dataset Flag:  BUFR_ERROR" );
                return vals_assigned;
            }
            else    /* Must be EOM */
            {
                return vals_assigned;
            }
        } 
        BUFR_DataSetEntryPut(DataSet, bv, IgnoreAFs);        
        vals_assigned++;
        if ( n == BUFR_EOD ) return vals_assigned;
    } 
 
}
