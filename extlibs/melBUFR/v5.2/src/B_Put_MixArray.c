/*
 * BUFR_Put_MixArray - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Put_MixArray - Add array of values to a BUFR message.
 * Return 1 on error, else 0.
 *
 * The length of the FXY array, when expanded, must match NumVals plus the
 * number of characters in a character string.  For the sake of 
 * convenience, the calling routine may specify only one FXY value, in
 * which case the single FXY value given is applied to the entire array of
 * data values.
 *
 * If BUFR_Define_Dataset() has been called, then FXY_Vals and NumFXYs
 * is ignored.
 * 
 * NOTE:  If character strings are used, include the number of characters that
 * BUFR expects for the fxy.  (i.e. if fxy 0 01 026 (storm name) is used,
 * use 8 for the number of characters that BUFR expects, instead of the
 * actual number of characters in the string).
 */
/* 
 * CHANGE LOG
 *
 * 102097  LAH: Removed unused variables NumBits, dum_int, dum_float,
 *              dum_dub, dum_short, val_extra
 *              Made hex constants 32 bits
 *              Added uint_t casts in mallocs
 *              Added double cast for missing value
 *
 * 102297  VLP:	Removed print statements and commented out code
 *		Casted to the rep_factor by type
 *
 * 121697  LAH: Added argument to FXY_List_Insert to allow updating of 
 *              new elements of FXY_List_t structure
 *  022498 LAH:  Added prints to bufr_log file.
 * 083198  VLP:  Check to see if fxy is a local table B fxy.  If so put a
                 206(data width) in front of the fxy.
 * 100898  VLP:  When a non-existant FXY is preceded by a 206(data width)
 *               refuse to use the FXY and exit.
 * 100898  VLP:  Check FXY data width against the 206(data width).  If the
 *               two are different, use the 206 data width.
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;
extern TableB_t* TableB;

#if PROTOTYPE_NEEDED

int BUFR_Put_MixArray( Data_MixVal_t* ValArray, int NumVals, 
                    FXY_t* FXY_Vals, int NumFXYs )

#else

int BUFR_Put_MixArray( ValArray, NumVals, FXY_Vals, NumFXYs )
Data_MixVal_t*       ValArray;   /* Array of data values                      */
int         NumVals;    /* Length of ValArray                        */
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

    FXY_t  fxy;

    FXY_List_t*  ExpList = NULL;
    FXY_Entry_t* exp_ent = NULL;

    EncVal_t* EncVals;
    EncVal_t* enc_val;
    int       NumEncVals;           /* Number of encoded values */
    int       NumAFs;               /* Number of associated fields */

    Data_MixVal_t* val_ptr;
    int   val_index;

    int    n;
    int    num_char;
    double d;

    Descriptor_t* descriptor;

    FXY_Entry_t* rep_beg;   /* Beginning of replicated sequence (1-XX-000). */
    FXY_Entry_t* rep_end;   /* End of replicated sequence */
    FXY_Entry_t* rep_ent;   /* Replicated FXY pointer */
    FXY_Entry_t* ins_ent;   /* Insertion point in ExpList */

    int          rep_fxys, rep_factor;
    FXY_t*       rep_array;
    FXY_List_t*  rep_list;

    int X_val, Y_val;
    int i;
    FXY_t new_fxy;
    int dw;

    TableB_Entry_t* BE;
    ValStack_t*     vp;

    Descriptor_t *dtp;
    /* DataType_t ValType; */

    char buf[80];

    BM = &BUFR_Msg;

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, ">>> Entering BUFR_Put_MixArray\n");
#endif

    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Put_MixArray",
            "Encoding function called while decoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
            "BUFR_Put_MixArray: ",
            "Encoding function called while decoding data");
        return 1;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Put_MixArray",
                "Can't mix raw and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                "BUFR_Put_MixArray: ",
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
        BUFR_Err_Set( "BUFR_Put_MixArray", "NULL pointer for value" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
            "BUFR_Put_MixArray: ", "NULL pointer for value");
        return 1;
    }

    if( NumVals < 1 )
    {
        BUFR_Err_Set( "BUFR_Put_MixArray", "Number of array values < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
           "BUFR_Put_MixArray: ", "Number of array values < 1");
        return 1;
    }

    if( FXY_Vals == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_MixArray", "NULL pointer for FXY array" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
            "BUFR_Put_MixArray: ", "NULL pointer for FXY array");
        return 1;
    }

    if( NumFXYs < 1 )
    {
        BUFR_Err_Set( "BUFR_Put_MixArray", "Number of FXY values < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
           "BUFR_Put_MixArray: ", "Number of FXY values < 1");
        return 1;
    }
    /* Expand FXY_Array. */

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Expanding FXY Array\n" );
#endif

    FXY_Array     = FXY_Vals;
    FXY_Array_Len = NumFXYs;

    if( (ExpList = FXY_List_Expand( FXY_Array, FXY_Array_Len )) == NULL )
    {
        BUFR_Err_Log( "BUFR_Put_MixArray" );
        return 1;
    }

#if DEBUG_PRINT
    if( BUFR_DebugLevel() )
    {
        fprintf(BUFR_Cntl.bufr_log, "Expanded FXY list is:\n" );
        FXY_List_Print( ExpList, BUFR_Cntl.bufr_log);
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

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Examining expanded list\n" );
#endif

    FXY_Array     = FXY_Vals;
    FXY_Array_Len = NumFXYs;

    val_ptr   = ValArray;
    val_index = 0;

    if( using_template )
    {
        ExpList = BUFR_Msg.exp_fxy_list;
        exp_ent = BUFR_Msg.exp_ptr;
    }
    else
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

                BUFR_Err_Set( "BUFR_Put_MixArray", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                    "BUFR_Put_MixArray: ", buf);
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                return 1;
            }
        }

        if( exp_ent->fxy == (FXY_t)FXY_IGNORE )
        {
            /* Ignore this FXY and the value in ValArray. */

            exp_ent = exp_ent->next;
            val_ptr++;
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
              	    val_ptr++;
                    val_index++;
                    NumEncVals++;
                }
            }

            if( (descriptor=TableB_Get( exp_ent->fxy )) == NULL )
            {
                sprintf( buf, "Non-Table B descriptor(%s)",
                    FXY_String( exp_ent->fxy ) );
                BUFR_Err_Set( "BUFR_Put_MixArray", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                    "BUFR_Put_MixArray: ", buf);
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                return 1;
            }

            if( descriptor->units_type == CCITT_IA5 )
            {
                i = descriptor->data_width;

                n = i/BITS_IN_BYTE + (i%BITS_IN_BYTE ? 1 : 0);

                /*  Add number of characters to number of encoded values
                 * because each character counts as a singel encoded value. 
                 */

                val_ptr++;
                NumEncVals += n;
                val_index++;

                exp_ent = exp_ent->next;
            } else
            {
                exp_ent = exp_ent->next;
              	val_ptr++;
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
                printf( "Sequence before delayed replication:\n" );
                FXY_List_Print( BUFR_Msg.exp_fxy_list, BUFR_Cntl.bufr_log);
            }
#endif

            rep_fxys = FXY_X_Value( exp_ent->fxy );

/*  VLP  10/22/97  Check the type of the replication factor and cast it to
		   type int. */
            if(val_ptr->Val_Type == DT_SHORT){
              rep_factor = (int)val_ptr->Val.short_num;
            } else if(val_ptr->Val_Type == DT_INT){
              rep_factor = (int)val_ptr->Val.int_number;
            } else if(val_ptr->Val_Type == DT_DOUBLE){
              rep_factor = (int)val_ptr->Val.number;
            }  else {
              rep_factor = (int)val_ptr->Val.ffloat;
            }
            val_ptr++;
            val_index++;

            /* Get 0-31-001 (0x1F01) or 0-31-002 (0x1F02) descriptor */

            exp_ent = exp_ent->next;

            /* Made hex constants 32 bits */
            if( exp_ent->fxy != (FXY_t)0x00001F01 && exp_ent->fxy != (FXY_t)0x00001F02 &&
                exp_ent->fxy != (FXY_t)0x00001F00)
            {
                sprintf( buf, "Expected FXY of 0-31-001 or 0-31-002, got %s",
                    FXY_String( exp_ent->fxy ) );
                BUFR_Err_Set( "BUFR_Put_MixArray", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                    "BUFR_Put_MixArray: ", buf);
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
                /* NumEncVals++;*/   /* Include 1-XX-000 */
                NumEncVals++;   /* Include 0-32-001/0-32-002 */

                for( i=0; i < rep_fxys; i++ )
                {
                    exp_ent = exp_ent->next;

                    if( exp_ent == ExpList->tail )
                    {
                        BUFR_Err_Set( "BUFR_Put_MixArray",
                            "Premature end of FXY array during replication" );
                        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                            "BUFR_Put_MixArray: ", 
                        	  "Premature end of FXY array during replication");
                        if( !using_template )
                            (void) FXY_List_Destroy( ExpList );
                        return 1;
                    }

                    /* NumEncVals++; */
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

            /* 102097 LAH: Added uint-t cast */
            rep_array = (FXY_t*) malloc( (uint_t) rep_fxys*sizeof(FXY_t*) );

            if( rep_array == NULL )
            {
                BUFR_Err_Set( "BUFR_Put_MixArray",
                    "Can't create array of replicated FXYs" );
                fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                    "BUFR_Put_MixArray: ", 
                    "Can't create array of replicated FXYs");
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
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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
                    if( FXY_List_Insert( ins_ent, rep_ent->fxy, ExpList) )
                    {
                        BUFR_Err_Log( "BUFR_Put_MixArray" );
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
                    		    val_ptr++;
                            val_index++;
                            NumEncVals++;  /* reference values must be encoded */
                        /* 102097 LAH: Made hex constant 32 bits */
                        } while( exp_ent->fxy != (FXY_t)0x000083FF );
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

                            BUFR_Err_Set( "BUFR_Put_MixArray", buf );
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

                   /* add number of characters to NumEncValues because
                    * each character requires a n encoded byte value
                    */
                    val_index++;
                    val_ptr++;
                    NumEncVals+= Y_val;

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

                        BUFR_Err_Set( "BUFR_Put_MixArray", buf );
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

                        BUFR_Err_Set( "BUFR_Put_MixArray", buf );
                        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                               "BUFR_Put_MixArray:", buf);

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
                    BUFR_Err_Set( "BUFR_Put_MixArray", buf );
                    fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                        "BUFR_Put_MixArray:",  buf);
                    if( !using_template )
                        (void) FXY_List_Destroy( ExpList );
                    return 1;
            }
        } else
        {
            sprintf( buf, "Invalid FXY value (%s) in expanded FXY array",
                FXY_String( exp_ent->fxy ) );
            BUFR_Err_Set( "BUFR_Put_MixArray", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                "BUFR_Put_MixArray:",  buf);
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

    /* 102097 LAH: Added uint_t cast */
    EncVals = (EncVal_t*) malloc( (uint_t) NumEncVals*sizeof(EncVal_t) );

    if( EncVals == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_MixArray", "Can't allocate data" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
            "BUFR_Put_MixArray:", "Can't allocate data" );
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
         BUFR_Err_Log( "BUFR_Put_MixArray" );                               \
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
    AL         = BM->af_list;
    NumAFs  = AF_List_Size( AL );       /* Get existing number of AFs. */

    while( 1 )
    {

      /*  VLP 60497 if the user has sent through short data and has not set a 
      missing value indicator, make sure that the missing value is of size
      for a short integer */

      if(BUFR_Cntl.Missing_User_Set != 2)
      {
	if  ( val_index < NumVals )
      	{
	  if(val_ptr->Val_Type == DT_SHORT)
	  {
	     BM->Missing_Value = (double) BUFR_MISSING_VALUE_SHORT;
	  }
	}
        /* 102097 LAH: Added double cast (above) */
      } else {
        BM->Missing_Value = BUFR_Cntl.User_Missing_Value;
      }

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
          BUFR_Err_Set( "BUFR_Put_MixArray", buf );
          fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
               "BUFR_Put_MixArray:", buf);
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
        val_ptr++;
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
            d = val_ptr->Val.number;
            if( EncVal_Set( enc_val, d, 0, 0, AE->nbits ) )
            {
              BUFR_Err_Log( "BUFR_Put_MixArray" );
              if( !using_template )
                  (void) FXY_List_Destroy( ExpList );
              free( (void*) EncVals );
              return 1;
            }

            /* Increment pointers. */
            val_ptr++;
            val_index++;
            enc_val++;
          }
        }

        if( (descriptor=TableB_Get( exp_ent->fxy )) == NULL )
        {
          sprintf( buf, "Non-Table B descriptor(%s)",
             FXY_String( exp_ent->fxy ) );
             BUFR_Err_Set( "BUFR_Put_MixArray", buf );
          fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
             "BUFR_Put_MixArray:", buf);
          if( !using_template )
             (void) FXY_List_Destroy( ExpList );
          free( (void*) EncVals );
          return 1;
        }
        /* If the descriptor is a local table descriptor, put a 206(dw) fxy before
        it so another location can at least read the data correctly.  */
        if(FXY_IsLocal(exp_ent->fxy))
        {
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
          num_char = strlen(val_ptr->Val.string);

          for( i=0; i < n; i++ )
          {
            if( i < num_char )
            {
              /* 102097 LAH: Made float cast double */
          	  d = (double)val_ptr->Val.string[i];
              EncVal_Set( enc_val, d, 0, 0, BITS_IN_BYTE );
            } else    /* ValArray exhausted, blank-fill remainder. */
            {
              EncVal_Set(enc_val, (double) ' ', 0, 0, BITS_IN_BYTE);
            }
            if( using_template )
            {
              if( DataList_Put(BM->data_list,&fxy,1,enc_val,1) )
              {
                BUFR_Err_Log( "BUFR_Put_MixArray" );
                free( (void*) EncVals );
                return 1;
              }
            }

            fxy = FXY_IGNORE;
            enc_val++;
          } /* for loop */

          if( using_template )
          {
            FXY_PtrInc();
            exp_ent = BUFR_Msg.exp_ptr;
          } else 
          {
            exp_ent = exp_ent->next;
          }

          val_ptr++; 
          val_index++;
        } else
        { 
            /* Encode value. */
          if(val_ptr->Val_Type == DT_SHORT)
          {
            *enc_val = EncodeValue( (void *)&val_ptr->Val.short_num,
                   DT_SHORT, exp_ent->fxy );
          } else if(val_ptr->Val_Type == DT_INT){
            *enc_val = EncodeValue((void *)&val_ptr->Val.int_number,
                   DT_INT, exp_ent->fxy );
          } else if(val_ptr->Val_Type == DT_DOUBLE){
            *enc_val = EncodeValue((void *)&val_ptr->Val.number,
                   DT_DOUBLE, exp_ent->fxy );
          } else if(val_ptr->Val_Type == DT_FLOAT){ 
            *enc_val = EncodeValue( (void *)&val_ptr->Val.ffloat,
                   DT_FLOAT, exp_ent->fxy );
          } else {
            BUFR_Err_Log( "BUFR_Put_MixArray: incorect data type" );
            return 1;
          }    
		
          if( EncVal_IsBad( *enc_val ) )
          {
            BUFR_Err_Log( "BUFR_Put_MixArray" );
            if( !using_template )
                 (void) FXY_List_Destroy( ExpList );
            free( (void*) EncVals );
            return 1;
          }

          /* Increment pointers. */

          INCREMENT_EXP_ENT;
          val_ptr++;
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
        /* VLP  10/23/97  Check for value type and explicitly cast the rep_factor */

        if(val_ptr->Val_Type == DT_SHORT)
        {
          rep_factor = (int)val_ptr->Val.short_num;
        } else if(val_ptr->Val_Type == DT_INT){
          rep_factor = (int)val_ptr->Val.int_number;
        } else if(val_ptr->Val_Type == DT_DOUBLE){
          rep_factor = (int)val_ptr->Val.number;
        } else {
          rep_factor = (int)val_ptr->Val.ffloat;
        }
          *enc_val = EncodeValue( &rep_factor, DT_INT, exp_ent->fxy );  

        INCREMENT_EXP_ENT;
        enc_val++;

        val_ptr++;
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
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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
                while( ValStack_Pop( vp ) == 0 );
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

            /* 102097 LAH: MAde hex constant 23 bits */
            while( exp_ent->fxy != (FXY_t)0x000083FF )
            {
              if( (descriptor=TableB_Get( exp_ent->fxy )) == NULL )
              {
                sprintf( buf, "Non-Table B FXY value between " );
                strcat( buf, "2-03-YYY and 2-03-255" );
                BUFR_Err_Set( "BUFR_Put_MixArray", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                      "BUFR_Put_MixArray:", buf);
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

              n = (int) val_ptr->Val.number;

              /* Push new RefVal onto Table B descriptor's stack. */

              if( ValStack_Push( &descriptor->RefValStack, n ) )
              {
                BUFR_Err_Log( "BUFR_Put_MixArray: 2-03-yyy pushing new RefVal" );
                fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                        "BUFR_Put_MixArray:",
                        " 2-03-yyy pushing new RefVal");
                if( !using_template )
                    (void) FXY_List_Destroy( ExpList );
                free( (void*) EncVals );
                    return 1;
              }
			
              /* test if givern bitwidth is large enough - LAH 07/08/97 */
              if ( Y_val < BUFR_BitWidth(n))
              {
                BUFR_Err_Log("BUFR_Put_MixArray: 2-03-yyy - bitwidth too small");
                fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                       "BUFR_Put_MixArray:",
                      " 2-03-yyy - bitwidth too small");
                if ( !using_template)
                       (void) FXY_List_Destroy( ExpList);
                free( (void*) EncVals );
                return 1;
              }
              if ( EncVal_RVSet(enc_val, (double) n, Y_val) )
              {
                BUFR_Err_Log("BUFR_Put_MixArray: bad change reference value");
                fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                      "BUFR_Put_MixArray:",
                        " bad change reference value");
                return 1;
              }


              INCREMENT_EXP_ENT;  

              val_ptr++;
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
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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

              n = (int) val_ptr->Val.number;
              if( AF_List_Put( AL, Y_val, n ) )
              {
                BUFR_Err_Log( "BUFR_Put_MixArray" );
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

          case 5:
            /* Signify Character. */
            /* Skip over Y_val number of characters. */

            fxy = exp_ent->fxy;
            num_char = strlen(val_ptr->Val.string);

            for( i=0; i < Y_val; i++ )
            {
              if( i < num_char )
              {
                /* 102097 LAH: Made float cast double */
          	  d = (double)val_ptr->Val.string[i];
                  EncVal_Set( enc_val, d, 0, 0, BITS_IN_BYTE );
              } else    /* ValArray exhausted, blank-fill remainder. */
              {
                EncVal_Set(enc_val, (double) ' ', 0, 0, BITS_IN_BYTE);
              }
              if( using_template )
              {
                if( DataList_Put(BM->data_list,&fxy,1,enc_val,1) )
                {
                  BUFR_Err_Log( "BUFR_Put_MixArray" );
                  free( (void*) EncVals );
                  return 1;
                }
              }

              fxy = FXY_IGNORE;
              enc_val++;
            } /* for loop */
            INCREMENT_EXP_ENT;

            val_ptr++;
            val_index++;
            break;

          case 6:
            /* Signify data with for the immediately
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

            if( dtp->data_width != Y_val )
            {
              fprintf( BUFR_Cntl.bufr_log," BUFR_Put_MixArray: ");
              fprintf( BUFR_Cntl.bufr_log," Width of Table C value %d is different from Table B value %d \n", Y_val, dtp->data_width);
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
    } else
      n = 0;

    free( (void*) EncVals );


#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "<<< Exiting BUFR_Put_MixArray\n");
#endif

    if( n != 0 )
    {
        BUFR_Err_Log( "BUFR_Put_MixArray" );
        return 1;
    } else
        return 0;
}
