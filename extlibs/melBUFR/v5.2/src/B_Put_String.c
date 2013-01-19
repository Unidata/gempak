/*
 * BUFR_Put_String - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Put_String - Use Table C descriptor (2-05-YYY) to add 'String'
 * in segments of MAX_Y_VAL (255) bytes or less.
 *
 * Return 1 on error, else 0.
 */
/*
 * CHANGE LOG 
 *
 * 100897 LAH: Added uint_t and int casts
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int BUFR_Put_String( char* String )

#else

int BUFR_Put_String( String )
char*       String;

#endif
{

    int    len;
    int    num_fxys;

    int    i;
    char*  s;

    FXY_t* fxy_list;
    FXY_t* fxy;
    int    y;

    EncVal_t* ev_list;
    EncVal_t* ev;

    if( String == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_String", "NULL string pointer passed" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_String", 
            "NULL string pointer passed" );
        return 1;
    }

    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Put_String",
            "Encoding function called while decoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_String", 
            "Encoding function called while decoding data" );
        return 1;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Put_String",
                "Can't mix raw and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_String", 
                "Can't mix raw and value-based processing methods." );
            return 1;

        case METHOD_TEMPLATE:

/*****************************************************************************
            BUFR_Err_Set( "BUFR_Put_String",
                "Can't mix template and value-based processing methods." );
            return 1;
******************************************************************************/
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

    /* Create list of string operator (2-05-YYY) descriptors. */

    /* 100897 LAH: Added int cast */
    if( (len= (int) strlen( String )) == 0 )
    {
        BUFR_Err_Set( "BUFR_Put_String", "Empty string passed" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_String", 
            "Empty string passed" );
        return 1;
    }

    num_fxys = len / MAX_Y_VAL;

    if( (len % MAX_Y_VAL) != 0 )
        num_fxys++;

    /* 100897 LAH: Added uint-t cast */
    if( (fxy_list=(FXY_t*)malloc( (uint_t)num_fxys * sizeof(FXY_t))) == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_String", "Can't allocate FXY list" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_String", 
            "Can't allocate FXY list" );
        return 1;
    }

    for( i=0, fxy=fxy_list, y=len; i < num_fxys; i++, fxy++, y-=MAX_Y_VAL )
    {
        if( y >= MAX_Y_VAL )
            *fxy = FXY_Pack( 2, 5, MAX_Y_VAL );
        else
            *fxy = FXY_Pack( 2, 5, y );
    }

    /* Create list of encoded values. */
    /* 100897 LAH: Added uint-t cast */
    if( (ev_list=(EncVal_t*)malloc( (uint_t)len * sizeof(EncVal_t))) == NULL )
    {
        BUFR_Err_Set("BUFR_Put_String", "Can't allocate encoded value list");
        free( (void*) fxy_list );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_String", 
            "Can't allocate encoded value list" );
        return 1;
    }

    for( i=0, s=String, ev=ev_list; i < len; i++, s++, ev++ )
    {
        if( EncVal_Set( ev, (double) *s, 0, 0, BITS_IN_BYTE ) )
        {
            BUFR_Err_Log( "BUFR_Put_String" );
            free( (void*) fxy_list );

            /* Deallocate hex string in structure */

            HexStr_Destroy( ev_list, i );
            free( (void*) ev_list  );
            return 1;
        }
    }

    i = DataList_Put( BUFR_Msg.data_list, fxy_list, num_fxys, ev_list, len );

    free( (void*) fxy_list );

    /* Deallocate hex string in structure */

    /*
     * JRA012497 - Don't deallocate the encoded values!  They are part
     * of the datalist and will be freed by DL_Destroy.
     *
    HexStr_Destroy( ev_list, len );
     *
     *
     * However, DO free space allocated to "ev_list."
     */

    free( (void*) ev_list  );

    return i;
}
