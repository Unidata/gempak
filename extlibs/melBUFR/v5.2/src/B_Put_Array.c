/*
 * BUFR_Put_Array - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Put_Array - Add array of values to a BUFR message.
 * Return 1 on error, else 0.
 *
 * The length of the FXY array, when expanded, must match NumVals.  For the
 * sake of convenience, the calling routine may specify only one FXY value, in
 * which case the single FXY value given is applied to the entire array of
 * data values.
 *
 * If BUFR_Define_Dataset() has been called, then FXY_Vals and NumFXYs
 * is ignored.
 */
/*
 * CHANGE LOG
 *
 * 080797  LAH: Modified handling of change of reference value
 * 100897  LAH: Added uint_t cast
 * 101097  LAH: Made hex constants 32 bits
 * 102097  LAH: Removed unused variable NumBits
 *              Added double cast of missing value
 * 121597  LAH: Modified FXY_List_Insert call to allow increment of FXY 
 *              counter
 * 022498  LAH:  Added prints to bufr_log file.
 * 083198  VLP:  Check to see if fxy is a local table B fxy.  If so put a
		 206(data width) in front of the fxy.
 * 100898  VLP:  When a non-existant FXY is preceded by a 206(data width)
 *		 refuse to use the FXY and exit.
 * 100898  VLP:  Check FXY data width against the 206(data width).  If the
 *		 two are different, use the 206 data width.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;
extern TableB_t* TableB;

#if PROTOTYPE_NEEDED

int BUFR_Put_Array( void* ValArray, int NumVals,
                    DataType_t ValType, FXY_t* FXY_Vals, int NumFXYs )

#else

int BUFR_Put_Array( ValArray, NumVals, ValType, FXY_Vals, NumFXYs )
void*       ValArray;   /* Array of data values                      */
int         NumVals;    /* Length of ValArray                        */
DataType_t  ValType;    /* Data type for array (e.g. int, double)    */
FXY_t*      FXY_Vals;   /* Array of FXY values describing 'ValArray' */
                        /* Ignored if processing dataset templates.  */
int         NumFXYs;    /* Length of FXY_Vals                        */

#endif
{

    int using_template = 0;

    BUFR_Msg_t* BM;

    AF_List_t*  AL;                 /* Associated field list */
    AF_Entry_t* AE;

    FXY_t* FXY_Array;
    int    FXY_Array_Len;

    FXY_t  repl_ray[14];            /* Used for replication if NumVals == 1 */
    int    MaxY_1, MaxY_2, MaxY_3;  /* Powers of 255 */
    int    C, S, M, R;
    FXY_t  fxy;

    FXY_List_t*  ExpList = NULL;
    FXY_Entry_t* exp_ent = NULL;

    EncVal_t* EncVals;
    EncVal_t* enc_val;
    int       NumEncVals;           /* Number of encoded values */
    int       NumAFs;               /* Number of associated fields */

    void* val_ptr;
    int   val_index;

    int    n;
    double d;
    FXY_t new_fxy;
    int dw;

    Descriptor_t* descriptor;
    int           fxy_is_ascii;

    FXY_Entry_t* rep_beg;   /* Beginning of replicated sequence (1-XX-000). */
    FXY_Entry_t* rep_end;   /* End of replicated sequence */
    FXY_Entry_t* rep_ent;   /* Replicated FXY pointer */
    FXY_Entry_t* ins_ent;   /* Insertion point in ExpList */

    int          rep_fxys, rep_factor;
    FXY_t*       rep_array;
    FXY_List_t*  rep_list;

    int X_val, Y_val;
    int i;

    TableB_Entry_t* BE;
    ValStack_t*     vp;

    Descriptor_t *dtp;

    char buf[80];

    BM = &BUFR_Msg;

    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Put_Array",
           "Encoding function called while decoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
           "BUFR_Put_Array: Encoding function called while decoding data");
        return 1;
    }

/*  VLP 60497 if the user has sent through short data and has not set a 
    missing value indicator, make sure that the missing value is of size
    for a short integer */

    if(BUFR_Cntl.Missing_User_Set != 2)
    {
      if(ValType == DT_SHORT)
         BM->Missing_Value = (double) BUFR_MISSING_VALUE_SHORT;
           /* 102097 LAH: Added double cast */
    } else {
        BM->Missing_Value = BUFR_Cntl.User_Missing_Value;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Put_Array",
                "Can't mix raw and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                "BUFR_Put_Array:", 
                "Can't mix raw and value-based processing methods.");
            return 1;

        case METHOD_TEMPLATE:

            /*
             * A dataset has been defined.  Ignore the given array of FXY
             * values and get them from the dataset.
             */

            using_template = 1;
            goto EXAMINE_EXP_LIST;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Put function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_ENCODE;
            BUFR_Msg.MethFlag = METHOD_VALUES;
    }

    if( ValArray == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_Array", "NULL pointer for value" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
           "BUFR_Put_Array:", "NULL pointer for value.");
        return 1;
    }

    if( NumVals < 1 )
    {
        BUFR_Err_Set( "BUFR_Put_Array", "Number of array values < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
            "BUFR_Put_Array:", "Number of array values < 1.");
        return 1;
    }

    if( FXY_Vals == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_Array", "NULL pointer for FXY array" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
            "BUFR_Put_Array:", "NULL pointer for FXY array");
         return 1;
    }

    if( NumFXYs < 1 )
    {
        BUFR_Err_Set( "BUFR_Put_Array", "Number of FXY values < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
            "BUFR_Put_Array:", "Number of FXY values < 1");
        return 1;
    }

    /*
     * If NumFXYs is 1, and the FXY is not for a CCITT_IA5 value, create
     * an array of FXY values which, when expanded, has a length equal
     * to NumVals.
     */

    if( ValType == DT_CHAR )
    {
        if( (descriptor=TableB_Get( *FXY_Vals )) != NULL )
            fxy_is_ascii = ( descriptor->units_type == CCITT_IA5 );
        else
            fxy_is_ascii = 0;
    }
    else
        fxy_is_ascii = 0;

    if( NumFXYs == 1 && FXY_IsTableB( *FXY_Vals ) && fxy_is_ascii == 0 )
    {
#if TRACE_PRINT
        if( BUFR_TraceLevel() )
            fprintf(BUFR_Cntl.bufr_log, 
                "Replicating descriptor %s\n", FXY_String( *FXY_Vals ) );
#endif

        /* Use data replication to create FXY_Array. */

        /*
         * The computations below attempt to satisfy the equation
         *
         *     NumVals = C*(255**3) + S*(255**2) + M*255 + R
         *
         * where
         *
         *     C = Number of sets of 255**3
         *     S = Number of sets of 255**2
         *     M = Number of sets of 255
         *     R = Remainder
         */

        MaxY_1 = MAX_Y_VAL;         /* Should be 255 */
        MaxY_2 = MaxY_1 * MaxY_1;
        MaxY_3 = MaxY_1 * MaxY_1 * MaxY_1;

        C = NumVals / MaxY_3;
        S = (NumVals - C*MaxY_3) / MaxY_2;
        M = (NumVals - C*MaxY_3 - S*MaxY_2) / MaxY_1;
        R = NumVals - C*MaxY_3 - S*MaxY_2 - M*MaxY_1;

        FXY_Array_Len = 0;
        FXY_Array_Len += ( C > 0 ? (C > 1 ? 5 : 4) : 0 );
        FXY_Array_Len += ( S > 0 ? (S > 1 ? 4 : 3) : 0 );
        FXY_Array_Len += ( M > 0 ? (M > 1 ? 3 : 2) : 0 );
        FXY_Array_Len += ( R > 0 ? (R > 1 ? 2 : 1) : 0 );

        fxy = *FXY_Vals;
        n = 0;

        if( C > 0 )
        {
            if( C > 1 ) repl_ray[n++] = FXY_Pack( 1, 4, C );
            repl_ray[n++] = FXY_Pack( 1, 3, MAX_Y_VAL );
            repl_ray[n++] = FXY_Pack( 1, 2, MAX_Y_VAL );
            repl_ray[n++] = FXY_Pack( 1, 1, MAX_Y_VAL );
            repl_ray[n++] = fxy;
        }

        if( S > 0 )
        {
            if( S > 1 ) repl_ray[n++] = FXY_Pack( 1, 3, S );
            repl_ray[n++] = FXY_Pack( 1, 2, MAX_Y_VAL );
            repl_ray[n++] = FXY_Pack( 1, 1, MAX_Y_VAL );
            repl_ray[n++] = fxy;
        }

        if( M > 0 )
        {
            if( M > 1 ) repl_ray[n++] = FXY_Pack( 1, 2, M );
            repl_ray[n++] = FXY_Pack( 1, 1, MAX_Y_VAL );
            repl_ray[n++] = fxy;
        }

        if( R > 0 )
        {
            if( R > 1 ) repl_ray[n++] = FXY_Pack( 1, 1, R );
            repl_ray[n++] = fxy;
        }

        FXY_Array     = &repl_ray[0];
        FXY_Array_Len = n;

#if DEBUG_PRINT
        if( BUFR_DebugLevel() > 3 )
        {
            printf( "Replication FXYs:" );

            for( i=0; i < FXY_Array_Len; i++ )
            {
                printf( " " );
                FXY_Print( FXY_Array[i], stdout );
            }

            printf( "\n" );
        }
#endif
    } else
    {
        FXY_Array     = FXY_Vals;
        FXY_Array_Len = NumFXYs;
    }

    /* Expand FXY_Array. */

#ifdef TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Expanding FXY Array\n" );
#endif

    if( (ExpList = FXY_List_Expand( FXY_Array, FXY_Array_Len )) == NULL )
    {
        BUFR_Err_Log( "BUFR_Put_Array" );
        return 1;
    }

#if DEBUG_PRINT
    if( BUFR_DebugLevel() )
    {
        fprintf(BUFR_Cntl.bufr_log, "Expanded FXY list is:\n" );
        FXY_List_Print( ExpList, BUFR_Cntl.bufr_log );
    }
#endif

EXAMINE_EXP_LIST:

    /*
     * The number of entries in ExpList doesn't necessarily indicate the
     * number of encoded values it will produce.  Scan ExpList to determine
     * how many encoded values are needed.  While scanning, resolve delayed
     * replication (1-XX-000/0-31-001 or 1-XX-000/0-31-002 pairs), process
     * Table C operators, and perform error-checking.
     */

#ifdef TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Examining expanded list\n" );
#endif

    val_ptr   = ValArray;
    val_index = 0;

    if( using_template )
    {
        ExpList = BUFR_Msg.exp_fxy_list;
        exp_ent = BUFR_Msg.exp_ptr;
    } else
        exp_ent = FXY_List_First( ExpList );

    NumEncVals = 0;
    AL         = BM->af_list;
    NumAFs     = AF_List_Size( AL );    /* Get existing number of AFs. */

    while( 1 )
    {
        if( using_template )
        {
            if( val_index >= NumVals )
            {
                /*
                 * The array of data values is exhausted.  If the end of the
                 * dataset has not been reached, and any FXY values of
                 * 2-01-YYY or 2-02-YYY remain in the dataset, process them.
                 */

                if( exp_ent == BM->exp_fxy_list->tail )
                    break;

                if( !FXY_IsTableC( exp_ent->fxy ) )
                    break;

                X_val = FXY_X_Value( exp_ent->fxy );

                if( X_val != 1 && X_val != 2 )
                    break;
            }

            /* Check for dataset wrap-around. */

            if( exp_ent == BM->exp_fxy_list->tail )
                exp_ent = FXY_List_First( BM->exp_fxy_list );
        } else
        {
            if( exp_ent == ExpList->tail )
                break;

            if( (val_index >= NumVals) && !(FXY_IsTableC(exp_ent->fxy)) )
            {
                sprintf( buf, "%s%s%d%s%d%s",
                    "Size of expanded FXY list exceeds length of ",
                    "value array (", val_index, " >= ", NumVals, ")" );

                BUFR_Err_Set( "BUFR_Put_Array", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_Array:", 
                     buf);
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                return 1;
            }
        }

        if( exp_ent->fxy == (FXY_t)FXY_IGNORE )
        {
            /* Ignore this FXY and the value in ValArray. */

            exp_ent = exp_ent->next;
            VoidInc( &val_ptr, ValType );
            val_index++;
        } else if( FXY_IsTableB( exp_ent->fxy ) )
        {
            if( NumAFs > 0 && exp_ent->fxy != (FXY_t)AF_SIG_FXY )
            {
                /*
                 * This is a data value and not an associated field
                 * significance value.  There MUST be NumAFs number of
                 * associated fields preceding this data value in ValArray.
                 * We don't need to process the AFs yet so just skip over the
                 * associated field values in ValArray and concentrate on
                 * the data value.
                 */

                for( i=0; i < NumAFs; i++ )
                {
                    VoidInc( &val_ptr, ValType );
                    val_index++;
                    NumEncVals++;
                }
            }

            if( (descriptor=TableB_Get( exp_ent->fxy )) == NULL )
            {
                sprintf( buf, "Non-Table B descriptor(%s)",
                    FXY_String( exp_ent->fxy ) );
                BUFR_Err_Set( "BUFR_Put_Array", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_Array:", 
                    buf);
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                return 1;
            }

            if( descriptor->units_type == CCITT_IA5 )
            {
                i = descriptor->data_width;

                n = i/BITS_IN_BYTE + (i%BITS_IN_BYTE ? 1 : 0);

                /* Skip past 'n' number of character values. */

                for( i=0; i < n; i++ )
                {
                    VoidInc( &val_ptr, ValType );
                    val_index++;
                    NumEncVals++;
                }

                exp_ent = exp_ent->next;
            } else
            {
                exp_ent = exp_ent->next;

                VoidInc( &val_ptr, ValType );
                val_index++;

                NumEncVals++;
            }
        } else if( FXY_IsReplicator( exp_ent->fxy ) )
        {
            /*
             * Since FXY_List_Expand() does not resolved delayed replication,
             * this must be a delayed replicator (1-XX-000).  Get the number
             * of FXYs to be replicated (rep_fxys) stored in *val_ptr and
             * make sure that the next FXY value is 0-31-001 or 0-31-002.
             */

#if DEBUG_PRINT
            if( BUFR_DebugLevel() > 3 )
            {
                PrintDivider( '-', 72, stdout );
                fprintf(BUFR_Cntl.bufr_log, 
                   "Sequence before delayed replication:\n" );
                FXY_List_Print( BUFR_Msg.exp_fxy_list, BUFR_Cntl.bufr_log);
            }
#endif

            rep_fxys = FXY_X_Value( exp_ent->fxy );

            if( VoidVal( val_ptr, ValType, (void*) &rep_factor, DT_INT ) )
            {
                BUFR_Err_Log( "BUFR_Put_Array" );
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                return 1;
            } else
            {
                VoidInc( &val_ptr, ValType );
                val_index++;
            }

            /* Get 0-31-001 (0x1F01) or 0-31-002 (0x1F02) descriptor */

            exp_ent = exp_ent->next;

            if( exp_ent->fxy != (FXY_t)0x00001F01 && exp_ent->fxy != (FXY_t)0x00001F02  &&
                exp_ent->fxy != (FXY_t)0x00001F00)
            {
                sprintf( buf, "Expected FXY of 0-31-001 or 0-31-002, got %s",
                    FXY_String( exp_ent->fxy ) );
                BUFR_Err_Set( "BUFR_Put_Array", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_Array:", 
                       buf);
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                return 1;
            }

            /*
             * SPECIAL CASE
             * If the replication factor is 0, there will be no data values in
             * ValArray but rep_fxys number of FXYs still need to be encoded.
             * Leave the unexpanded and unreplicated FXY values alone (but
             * increment NumEncVals) and move on.
             */

            if( rep_factor == 0 )
            {
             /*   NumEncVals++; */  /* Include 1-XX-000 */
                NumEncVals++;   /* Include 0-32-001/0-32-002 */

                for( i=0; i < rep_fxys; i++ )
                {
                    exp_ent = exp_ent->next;

                    if( exp_ent == ExpList->tail )
                    {
                        BUFR_Err_Set( "BUFR_Put_Array",
                            "Premature end of FXY array during replication" );
                        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                            "BUFR_Put_Array:", 
                            "Premature end of FXY array during replication");
                        if( !using_template )
                            (void) FXY_List_Destroy( ExpList );
                        return 1;
                    }

                 /*   NumEncVals++; */
                }

                /* Position pointer to FXY after replicated sequence. */

                exp_ent = exp_ent->next;

                continue;
            }

            /*
             * Set exp_ent to beginning of sequence to be replicated and save
             * it in rep_beg for later removal.  Ordinarily the
             * 1-XX-000/0-31-001 or 1-XX-000/0-31-002 pair and the FXYs to
             * be replicated would be removed and replaced with the expanded
             * replication sequence but I want to save the pair in order to
             * properly encode the replication factor.
             */

            NumEncVals++;   /* replication factor must be encoded. */

            exp_ent = exp_ent->next;
            rep_beg = exp_ent;

            /* Create an array of the next rep_fxys number of FXYs. */

            /* 100897 LAH:  Added uint_t cast */
            rep_array = (FXY_t*) malloc( (uint_t)rep_fxys * sizeof(FXY_t*) );

            if( rep_array == NULL )
            {
                BUFR_Err_Set( "BUFR_Put_Array",
                    "Can't create array of replicated FXYs" );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_Array:", 
                    "Can't create array of replicated FXYs" );
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                return 1;
            }

            for( i=0; i < rep_fxys; i++ )
            {
                rep_array[i] = exp_ent->fxy;
                rep_end = exp_ent;
                exp_ent = exp_ent->next;
            }

            /*
             * Remove the FXY sequence to be replicated from ExpList. It's
             * easier to remove it in reverse order but be sure to save the
             * entry appearing before rep_beg first.
             */

            exp_ent = FXY_List_Prev( ExpList, rep_beg );

            /* exp_ent now points to the 0-31-001 or 0-31-002 entry. */

            do
            {
                rep_ent = rep_end;
                rep_end = FXY_List_Remove( ExpList, rep_ent );
            } while( rep_end != exp_ent );

            /*
             * Replication sequence is removed from ExpList.  Insert expanded
             * replication sequence from rep_array into ExpList.
             */

            if( (rep_list=FXY_List_Expand( rep_array, rep_fxys )) == NULL )
            {
                BUFR_Err_Log( "BUFR_Put_Array" );
                free( (void*) rep_array );
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                return 1;
            } else
            {
                /* rep_array is no longer needed. */
                free( (void*) rep_array );
            }

            /*
             * Since FXY_List_Insert() inserts an FXY entry AFTER the
             * given entry, set the insertion point to the entry
             * before rep_beg.
             */
            ins_ent = exp_ent;  /* 0-31-001 or 0-31-002 entry. */

            /* Repeat replication rep_factor number of times. */

            for( i=0; i < rep_factor; i++ )
            {
                rep_ent = FXY_List_First( rep_list );

                while( rep_ent != rep_list->tail )
                {
		    /* added argument to allow updateing of modified */
		    /* FXY_List structure */
                    if( FXY_List_Insert( ins_ent, rep_ent->fxy, ExpList ) )
                    {
                        BUFR_Err_Log( "BUFR_Put_Array" );
                        (void) FXY_List_Destroy( rep_list );
                        if( !using_template )
                            (void) FXY_List_Destroy( ExpList );
                        return 1;
                    }

                    rep_ent = rep_ent->next;
                    ins_ent = ins_ent->next;
                }
            }

            /*
            * Expanded replication list has been inserted and is no longer
            * needed.
            */

            (void) (void) FXY_List_Destroy( rep_list );

#if DEBUG_PRINT
            if( BUFR_DebugLevel() > 3 )
            {
                fprintf(BUFR_Cntl.bufr_log, "Sequence after delayed replication:\n" );
                FXY_List_Print( BUFR_Msg.exp_fxy_list, BUFR_Cntl.bufr_log);
                PrintDivider( '-', 72, BUFR_Cntl.bufr_log );
            }
#endif

            /*
             * Position exp_ent to the first FXY in the sequence
             * just replicated in order to process it.
             */

            exp_ent = exp_ent->next;
        } else if( FXY_IsTableC( exp_ent->fxy ) )
        {
            X_val = FXY_X_Value( exp_ent->fxy );
            Y_val = FXY_Y_Value( exp_ent->fxy );

            switch( X_val )
            {
                case 1:     /* Increase DataWidth by (Y_val-128). */
                case 2:     /* Increase Scale     by (Y_val-128). */
                    exp_ent = exp_ent->next;
                    break;

                case 3:     /* Change Reference Values. */

                    if( Y_val == 0 )
                    {
                        exp_ent = exp_ent->next;
                        break;
                    } else
                    {
                        /* Gobble FXY values until 2-03-255 (0x83FF). */
                        /* step over 2-03-yyy descriptor  LAH 07/08/97 */
                        exp_ent = exp_ent->next;   /* LAH 7-08-97 */
                        do
                        {
                            exp_ent = exp_ent->next;
                            VoidInc( &val_ptr, ValType );
                            val_index++;
                            NumEncVals++;  /* reference values must be encoded */
                        } while( exp_ent->fxy != (FXY_t)0x00000083FF );
                        /*  step over 2-03-255 descriptor */
                        exp_ent = exp_ent->next;  /* lah 7-8-97 */
                    }

                    break;

                case 4:     /* Add associated field. */

                    if( Y_val == 0 )
                        NumAFs--;       /* AF cancelled */
                    else
                    {
                        NumAFs++;       /* AF added */

                        /* Make sure the next FXY is for AF significance. */

                        if( exp_ent->next->fxy != (FXY_t)AF_SIG_FXY )
                        {
                            sprintf( buf, "%s%s%s%s%s%s",
                                "Add Associated Field operator must be ",
                                "followed by ",
                                FXY_String(AF_SIG_FXY),
                                ".  Encountered ",
                                FXY_String( exp_ent->next->fxy ),
                                " instead." );

                            BUFR_Err_Set( "BUFR_Put_Array", buf );
                            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                                "BUFR_Put_Array:", buf);
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            return 1;
                        }
                    }

                    /*
                     * There is no corresponding data value for an associated
                     * field so just advance exp_ent, not val_ptr
                     */

                    exp_ent = exp_ent->next;
                    break;

                case 5:     /* Signify Y_val number of characters. */

                    /* Skip over Y_val number of characters. */

                    for( i=0; i < Y_val; i++ )
                    {
                        VoidInc( &val_ptr, ValType );
                        val_index++;
                        NumEncVals++;
                    }

                    exp_ent = exp_ent->next;

                    break;

                case 6:     /* Signify data with for the immediately
                               following local (Table B) descriptor. */

                    /*
                     * Get the immediately following local descriptor and,
                     * if it isn't already there, add it to Table B.
                     */

                    if( !FXY_IsTableB( exp_ent->next->fxy ) )
                    {
                        sprintf( buf,
                        "%s followed by %s instead of a Table B descriptor",
                        FXY_String( exp_ent->fxy ),
                        FXY_String( exp_ent->next->fxy ) );

                        BUFR_Err_Set( "BUFR_Put_Array", buf );
                        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                            "BUFR_Put_Array:", buf);
                        return 1;
                    }

                    exp_ent = exp_ent->next;    /* local descriptor. */

                    if( (dtp=TableB_Get( exp_ent->fxy )) == NULL )
                    {
                        /* IF this descriptor does not exist why do  */
                        /* you want to use it?  */
                        sprintf( buf,
                        "%s does not exist as a Table B descriptor",
                        FXY_String( exp_ent->next->fxy ) );
                        BUFR_Err_Set( "BUFR_Put_Array", buf );
                        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                            "BUFR_Put_Array:", buf);
                        return 1;

                    }

                    /*
                     * Continue processing.  This local descriptor will
                     * be processed as if it had been read from the
                     * Table B file.
                     */

                    NumEncVals++;
                    break;

                default:
                    sprintf( buf, "Invalid Table C descriptor (%s)",
                        FXY_String( exp_ent->fxy ) );
                    BUFR_Err_Set( "BUFR_Put_Array", buf );
                    fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                        "BUFR_Put_Array:", buf);
                    if( !using_template )
                        (void) FXY_List_Destroy( ExpList );
                    return 1;
            }
        }
        else
        {
            sprintf( buf, "Invalid FXY value (%s) in expanded FXY array",
                FXY_String( exp_ent->fxy ) );
            BUFR_Err_Set( "BUFR_Put_Array", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Put_Array:", buf);
            if( !using_template )
                (void) FXY_List_Destroy( ExpList );
            return 1;
        }
    }       /* End of expanded FXY list examination */

#if DEBUG_PRINT
    if( BUFR_DebugLevel() )
    {
        fprintf(BUFR_Cntl.bufr_log, "Expanded list length=%d\n", FXY_List_Size( ExpList ) );
        fprintf(BUFR_Cntl.bufr_log, "# encoded values=%d\n", NumEncVals );
        fprintf(BUFR_Cntl.bufr_log, "Processed expanded FXY list is:\n" );
        FXY_List_Print( ExpList, BUFR_Cntl.bufr_log);
    }
#endif

    /* Allocate array to store encoded values. */
    /* 100897 LAH:  Added uint_t cast */
    EncVals = (EncVal_t*) malloc( (uint_t)NumEncVals * sizeof(EncVal_t) );

    if( EncVals == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_Array", "Can't allocate data" );
        if( !using_template )
            (void) FXY_List_Destroy( ExpList );
        return 1;
    } else
    {
        /* Initialize the encoded values array. */

        for( i=0; i < NumEncVals; i++ )
            EncVal_Init( &EncVals[i] );
    }

    /*******************************************/
    /* Process (expanded) array of FXY values. */
    /*******************************************/

    /*
     * If processing templates, add values to the data list one-at-a-time.
     * If not, values are added all at once after processing.
     */

#define INCREMENT_EXP_ENT                                                   \
    if( using_template )                                                    \
    {                                                                       \
        if( DataList_Put( BM->data_list, &exp_ent->fxy, 1, enc_val, 1 ) )   \
        {                                                                   \
            BUFR_Err_Log( "BUFR_Put_Array" );                               \
            free( (void*) EncVals );                                 \
            return 1;                                                       \
        }                                                                   \
        FXY_PtrInc();                                                       \
        exp_ent = BUFR_Msg.exp_ptr;                                         \
    } else                                                                    \
        exp_ent = exp_ent->next;

    val_ptr   = ValArray;
    val_index = 0;

    if( using_template )
    {
        ExpList = BUFR_Msg.exp_fxy_list;
        exp_ent = BUFR_Msg.exp_ptr;
    } else
        exp_ent = FXY_List_First( ExpList );

    enc_val = EncVals;
    NumAFs  = AF_List_Size( AL );       /* Get existing number of AFs. */

    while( 1 )
    {
        if( using_template )
        {
            if( val_index >= NumVals )
            {
                /*
                 * The array of data values is exhausted.  If the end of the
                 * dataset has not been reached, and any FXY values of
                 * 2-01-YYY or 2-02-YYY remain in the dataset, process them.
                 */

                if( exp_ent == BM->exp_fxy_list->tail )
                    break;

                if( exp_ent == BM->exp_fxy_list->head->next )
                    break;          /* Dataset wrapped around. */

                if( !FXY_IsTableC( exp_ent->fxy ) )
                    break;

                X_val = FXY_X_Value( exp_ent->fxy );

                if( X_val != 1 && X_val != 2 )
                    break;
            }

            /*
             * JRA103096:  Since FXY_PtrInc() takes care of this wrap-around
             * business, the if() statement below probably never evaluates
             * to false.
             */

            /* Check for dataset wrap-around. */

            if( exp_ent == BM->exp_fxy_list->tail )
                exp_ent = FXY_List_First( BM->exp_fxy_list );
        } else
        {
            if( exp_ent == ExpList->tail )
                break;

            if( (val_index >= NumVals) && !(FXY_IsTableC( exp_ent->fxy)) )
            {
                sprintf( buf, "%s%s",
                    "Length of value array not equal to ",
                    "number of expanded FXYs" );
                BUFR_Err_Set( "BUFR_Put_Array", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                    "BUFR_Put_Array:", buf);
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                free( (void*) EncVals );
                return 1;
            }
        }

        if( exp_ent->fxy == (FXY_t)FXY_IGNORE )
        {
            /* Ignore this FXY and the value in ValArray. */

            INCREMENT_EXP_ENT;
            VoidInc( &val_ptr, ValType );
            val_index++;
        } else if( FXY_IsTableB( exp_ent->fxy ) )
        {
            if( NumAFs > 0 && exp_ent->fxy != (FXY_t)AF_SIG_FXY )
            {
                /*
                 * Encode each associated field preceding the data value
                 * with an FXY value of FXY_IGNORE.
                 * EncodeValue() won't work for AFs so encode them manually.
                 */

                for( i=0, AE=AF_List_First(AL); i < NumAFs; i++, AE=AE->next )
                {
                    EncVal_Init( enc_val );

                    VoidVal( val_ptr, ValType, &d, DT_DOUBLE );

                    if( EncVal_Set( enc_val, d, 0, 0, AE->nbits ) )
                    {
                        BUFR_Err_Log( "BUFR_Put_Array" );
                        if( !using_template )
                            (void) FXY_List_Destroy( ExpList );
                        free( (void*) EncVals );
                        return 1;
                    }

                    /* Increment pointers. */

                    VoidInc( &val_ptr, ValType );
                    val_index++;

                    enc_val++;
                }
            }

            if( (descriptor=TableB_Get( exp_ent->fxy )) == NULL )
            {
                sprintf( buf, "Non-Table B descriptor(%s)",
                    FXY_String( exp_ent->fxy ) );
                BUFR_Err_Set( "BUFR_Put_Array", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                    "BUFR_Put_Array:", buf);
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                free( (void*) EncVals );
                return 1;
            }
/* If the descriptor is a local table descriptor, put a 206(dw) fxy before
   it so another location can at least read the data correctly.  */
            if(FXY_IsLocal(exp_ent->fxy)){
                dw = FXY_Get_DataWidth(exp_ent->fxy);
                new_fxy = FXY_Pack(2,6,dw);
                DataList_Put( BUFR_Msg.data_list, &new_fxy,1,NULL,0);
            }

            if( descriptor->units_type == CCITT_IA5 )
            {
                i = descriptor->data_width;

                n = i/BITS_IN_BYTE + (i%BITS_IN_BYTE ? 1 : 0);

                /*
                 * Encode 'n' number of character values.  Only the first FXY
                 * value should be put on the data list.  The remaining FXY
                 * values should be stored as FXY_IGNORE.
                 */

                fxy = exp_ent->fxy;

                for( i=0; i < n; i++ )
                {
                    if( val_index < NumVals )
                    {
                        VoidVal( val_ptr, ValType, &d, DT_DOUBLE );
                        EncVal_Set( enc_val, d, 0, 0, BITS_IN_BYTE );
                        VoidInc( &val_ptr, ValType );
                        val_index++;
                    } else    /* ValArray exhausted, blank-fill remainder. */
                    {
                        EncVal_Set(enc_val, (double) ' ', 0, 0, BITS_IN_BYTE);
                    }

                    if( using_template )
                    {
                        if( DataList_Put(BM->data_list,&fxy,1,enc_val,1) )
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            free( (void*) EncVals );
                            return 1;
                        }
                    }

                    fxy = FXY_IGNORE;
                    enc_val++;
                }

                if( using_template )
                {
                    FXY_PtrInc();
                    exp_ent = BUFR_Msg.exp_ptr;
                } else
                    exp_ent = exp_ent->next;
            } else
            {
                /* Encode value. */

                *enc_val = EncodeValue( val_ptr, ValType, exp_ent->fxy );

                if( EncVal_IsBad( *enc_val ) )
                {
                    BUFR_Err_Log( "BUFR_Put_Array" );
                    if( !using_template )
                        (void) FXY_List_Destroy( ExpList );
                    free( (void*) EncVals );
                    return 1;
                }

                /* Increment pointers. */

                INCREMENT_EXP_ENT;

                VoidInc( &val_ptr, ValType );
                val_index++;

                enc_val++;
            }
        } else if( FXY_IsReplicator( exp_ent->fxy ) )
        {
            /*
             * This FXY value is 1-XX-000 (delayed replication).  Save the
             * number of FXYs to replicate (in case that the replication
             * factor is 0) and get the (following) FXY value of 0-31-001 or
             * 0-31-002.
             */

            rep_fxys = FXY_X_Value( exp_ent->fxy );
            INCREMENT_EXP_ENT;

            /* enc_val++;   JRA111296 - Don't store bad enc val. */

            /* Encode the replication factor. */

            (void) VoidVal( val_ptr, ValType, (void*) &rep_factor, DT_INT );
            *enc_val = EncodeValue( &rep_factor, DT_INT, exp_ent->fxy );

            INCREMENT_EXP_ENT;
            enc_val++;

            VoidInc( &val_ptr, ValType );
            val_index++;

            if( rep_factor == 0 )
            {
                /*
                 * SPECIAL CASE
                 * If the replication factor is 0, the FXY values still need
                 * to be encoded but there are no corresponding data values.
                 * Skip over the next rep_fxys number of FXY values.
                 */

                for( i=0; i < rep_fxys; i++ )
                {
                    INCREMENT_EXP_ENT;
                    /* enc_val++;   JRA111296 - Don't store bad enc val. */
                }
            }
        } else if( FXY_IsTableC( exp_ent->fxy ) )
        {
            X_val = FXY_X_Value( exp_ent->fxy );
            Y_val = FXY_Y_Value( exp_ent->fxy );

            switch( X_val )
            {
                case 1:     /* Increase DataWidth by (Y_val-128). */

                    if( Y_val == 0 )
                    {
                        if( ValStack_Pop( &BM->DataWidthStack ) )
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            free( (void*) EncVals );
                            return 1;
                        }
                    } else
                    {
                        Y_val -= 128;

                        if( ValStack_Push( &BM->DataWidthStack, Y_val ) )
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            free( (void*) EncVals );
                            return 1;
                        }
                    }

                    if( using_template )
                    {
                        if(DataList_Put(BM->data_list,&exp_ent->fxy,1,NULL,0))
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            free( (void*) EncVals );
                            return 1;
                        }
                        FXY_PtrInc();
                        exp_ent = BUFR_Msg.exp_ptr;
                    } else
                        exp_ent = exp_ent->next;

                    break;

                case 2:     /* Increase Scale by (Y_val-128). */

                    if( Y_val == 0 )
                    {
                        if( ValStack_Pop( &BM->ScaleStack ) )
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            free( (void*) EncVals );
                            return 1;
                        }
                    } else
                    {
                        Y_val -= 128;

                        if( ValStack_Push( &BM->ScaleStack, Y_val ) )
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            free( (void*) EncVals );
                            return 1;
                        }
                    }

                    if( using_template )
                    {
                        if(DataList_Put(BM->data_list,&exp_ent->fxy,1,NULL,0))
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            free( (void*) EncVals );
                            return 1;
                        }
                        FXY_PtrInc();
                        exp_ent = BUFR_Msg.exp_ptr;
                    } else
                        exp_ent = exp_ent->next;

                    break;

                case 3:     /* Change Reference Values. */

                    if( Y_val == 0 )
                    {
                        /* Reset all reference values. */

                        BE = TableB->head->next;

                        for( ; BE != TableB->tail; BE=BE->next )
                        {
                            vp = &BE->item->RefValStack;

                            while( ValStack_Pop( vp ) == 0 )
                                ;
                        }
                        /*  increment past 2-03-000 descriptor - LAH 07-09-97*/
                        INCREMENT_EXP_ENT;
                        
                        break;
                    }

                    /*
                     * Get new reference values and the FXY to which they
                     * apply until encountering 2-03-255 (0x83FF).
                     */

                    INCREMENT_EXP_ENT;

                    while( exp_ent->fxy != (FXY_t)0x000083FF )
                    {
                        if( (descriptor=TableB_Get( exp_ent->fxy )) == NULL )
                        {
                            sprintf( buf, "Non-Table B FXY value between " );
                            strcat( buf, "2-03-YYY and 2-03-255" );
                            BUFR_Err_Set( "BUFR_Put_Array", buf );
                            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                                "BUFR_Put_Array:", buf);
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            free( (void*) EncVals );
                            return 1;
                        }

                        switch( descriptor->units_type )
                        {
                            case CCITT_IA5:
                            case CODE_TABLE:
                            case FLAG_TABLE:
                                continue;

                            case NUMERIC:
                            default:
                                break;
                        }

                        /* Get new reference value . */

                        VoidVal( val_ptr, ValType, &n, DT_INT );

                        /* Push new RefVal onto Table B descriptor's stack. */

                        if( ValStack_Push( &descriptor->RefValStack, n ) )
                        {
                            BUFR_Err_Log( "BUFR_Put_Array: 2-03-yyy pushing new RefVal" );
                            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                               "BUFR_Put_Array:",
                               "2-03-yyy pushing new RefVal" );
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            free( (void*) EncVals );
                            return 1;
                        }
			
                        /* test if givern bitwidth is large enough - LAH 07/08/97 */
                        if ( Y_val < BUFR_BitWidth(n))
                        {
                           BUFR_Err_Log("BUFR_Put_Array: 2-03-yyy - bitwidth too small");
                           fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                              "BUFR_Put_Array:",
                              "2-03-yyy - bitwidth too small" );
                           if ( !using_template)
                               (void) FXY_List_Destroy( ExpList);
                           free( (void*) EncVals );
                           return 1;
                        }

                        if ( EncVal_RVSet(enc_val, (double) n, Y_val) )
                        {
                          BUFR_Err_Log("BUFR_Put_Array: bad change reference value");
                          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                              "BUFR_Put_Array:",
                              "bad change reference value" );
                          return 1;
                        }


                        INCREMENT_EXP_ENT;  

                        VoidInc( &val_ptr, ValType );
                        val_index++;
                        enc_val++;
                    }
                     /* increment past 2-03-255 descriptor - LAH 07/08/97 */
                    INCREMENT_EXP_ENT;

                    break;

                case 4:     /* Associated field operator. */

                    if( Y_val == 0 )    /* Cancel AF */
                    {
                        if( AF_List_Remove( AL ) )
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            free( (void*) EncVals );
                            return 1;
                        }

                        NumAFs--;
                    } else                /* Add AF */
                    {
                        /*
                         * Get AF significance from next descriptor.
                         * We know it's there because it was checked
                         * before creating the expanded FXY array.
                         */

                        VoidVal( val_ptr, ValType, &n, DT_INT );

                        if( AF_List_Put( AL, Y_val, n ) )
                        {
                            BUFR_Err_Log( "BUFR_Put_Array" );
                            if( !using_template )
                                (void) FXY_List_Destroy( ExpList );
                            free( (void*) EncVals );
                            return 1;
                        }

                        NumAFs++;
                    }

                    /*
                     * There's no corresponding data value for this FXY
                     * so just advance the expanded FXY array pointer.
                     */

                    INCREMENT_EXP_ENT;
                    break;

                case 5:     /* Signify Character. */

                    /* Skip over Y_val number of characters. */

                    for( i=0; i < Y_val; i++ )
                    {
                        /*
                         * Store character value in enc_val instead of
                         * encoding it. (This will need to change when
                         * EncVal_t is no longer
                         */

                        VoidVal( val_ptr, ValType, &d, DT_DOUBLE );
                        EncVal_Set( enc_val, d, 0, 0, BITS_IN_BYTE );

                        VoidInc( &val_ptr, ValType );
                        val_index++;

                        enc_val++;
                    }

                    INCREMENT_EXP_ENT;

                    break;

                case 6:     /* Signify data with for the immediately
                               following local (Table B) descriptor. */

                    /*
                     * Add this FXY to the datalist (with a bad enc_val).
                     * The next local descriptor was added during the
                     * FXY expansion process. Make sure that the
                     * data width for this descriptor matches the one
                     * stored in Table B.  If not, change it.
                     */

                    /* Point to descriptor referenced by 2-06-YYY. */

                    INCREMENT_EXP_ENT;
                    enc_val++;

                    dtp = TableB_Get( exp_ent->fxy );

/*  When there is a conflict between the data width of a FXY and the data
    width given in a 206 (table C), use the Table C data width */
                    if( dtp->data_width != Y_val ) {
                      fprintf( BUFR_Cntl.bufr_log," BUFR_Put_Array: ");
                      fprintf( BUFR_Cntl.bufr_log,
                             " Width of Table C value %d is different from Table B value %d \n",
                             Y_val, dtp->data_width);
                      fprintf( BUFR_Cntl.bufr_log, " using Table C value \n");
                      dtp->data_width = Y_val;
                    }

                    break;

                default:
                    return 1;
            }
        }
    }

    if( !using_template )
    {
        /* Expanded FXY list is no longer needed, free it. */

        (void) FXY_List_Destroy( ExpList );

        /* Add FXY_Array and encoded array values to the data list. */

        n = DataList_Put( BM->data_list, FXY_Array, FXY_Array_Len,
            EncVals, NumEncVals );
    }
    else
        n = 0;

    free( (void*) EncVals );

    if( n != 0 )
    {
        BUFR_Err_Log( "BUFR_Put_Array" );
        return 1;
    }
    else
        return 0;
}
