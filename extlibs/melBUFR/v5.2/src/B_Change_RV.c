/*
 * BUFR_Change_RefVal - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Change_RefVal - Use descriptor 2-03-YYY to change the reference value
 * for the given Table B descriptor. Return 1 on error, else 0.
 *
 * Reference value changes remain in effect until this function is called
 * with a different reference value or the default value is restored by
 * calling BUFR_Reset_RefVals().
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

int BUFR_Change_RefVal( FXY_t FXY_Val, int newRV )

#else

int BUFR_Change_RefVal( FXY_Val, newRV )
FXY_t       FXY_Val;
int         newRV;

#endif
{

    Descriptor_t* dp;
    int           newRVbits;

    FXY_t    fxys[3];
    EncVal_t ev;

    char  errbuf[80];

    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Change_RefVal",
            "Encoding function called while decoding data" );
        return 1;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Change_RefVal",
                "Can't mix raw and value-based processing methods." );
            return 1;

        case METHOD_TEMPLATE:

            /*
             * A dataset has already been defined.  Most likely, this function
             * is being called from the same loop that created the dataset in
             * the first place and will be called repeatedly.  Instead of
             * repeatedly printing warnings, just return as if the function
             * successfully completed.
             */

            return 0;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Put function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_ENCODE;
            BUFR_Msg.MethFlag = METHOD_VALUES;
    }
    
    /* if new reference value is zero (0), cancel previous Refence Value change */
    if ( newRV == 0 )
    {
        BUFR_Reset_RefVals();
        return 0;
    }

    /* Get pointer to Table B descriptor */

    if( !FXY_IsTableB( FXY_Val ) )
    {
        sprintf( errbuf, "Non-Table B FXY value (%s)", FXY_String(FXY_Val) );
        BUFR_Err_Set( "BUFR_Change_RefVal", errbuf );
        return 1;
    }
    else if( (dp = TableB_Get( FXY_Val )) == NULL )
        return 1;

    switch( dp->units_type )
    {
        case CCITT_IA5:
        case CODE_TABLE:
        case FLAG_TABLE:
            BUFR_Err_Set( "BUFR_Change_RefVal",
                "Only NUMERIC Reference values may be changed" );
            return 1;

        case NUMERIC:
        default:
            break;
    }

    /* Push newRV onto Table B descriptor's Reference Value stack. */

#ifdef TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Pushing %d onto RefVal stack\n",
	    newRV );
#endif

    if( ValStack_Push( &dp->RefValStack, newRV ) )
        return 1;

    /* Determine the minimum number of bits needed to store newRV. */

    newRVbits = BUFR_BitWidth( (int)newRV );

    /* Add Table C operators and newRV to data list */

    fxys[0] = FXY_Pack( 2, 3, newRVbits );
    fxys[1] = FXY_Val;
    fxys[2] = FXY_Pack( 2, 3, 255 );

    if( EncVal_RVSet( &ev, (double) newRV, newRVbits ) )
    {
        BUFR_Err_Log( "BUFR_Change_RefVal" );
        return 1;
    }

    if( DataList_Put( BUFR_Msg.data_list, fxys, 3, &ev, 1 ) )
        return 1;

    /* Deallocate hex string member of structure. */

/*
    free( (void*)ev.value );
*/

    return 0;
}
