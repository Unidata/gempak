/*
 * BUFR_Get_Value - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Get_Value - Get the next (decoded) value from a BUFR message.
 *
 * Values returned by this function have the following meanings:
 *
 *
 * BUFR_OK    A value was successfully decoded and there are more data
 *            data values in the BUFR message.
 *
 * BUFR_ERROR There is something wrong with the BUFR message and decoding
 *            cannot proceed.
 *
 * BUFR_EOD   The last value in the current data set has been reached but
 *            there are more values in the next data set.  The value
 *            returned is garbage so call this function again to get the
 *            next value.
 *
 * BUFR_EOM   There are no more data values in the BUFR message but
 *            there are more BUFR messages in the file.  Call function
 *            BUFR_Init() to get the next BUFR message.
 *
 * BUFR_EOF   There are no more BUFR messages in the file.  Call function
 *            BUFR_Destroy() to free up memory.
 *
 *****************************************************************************
 * NOTE: Memory allocated to 'BV' by this routine will be freed on the
 *       next call so be sure and save any allocated values.
 *****************************************************************************
 */
/*
 * Change LOG 
 *
 * 092997  LAH: Added cast to correct Linux warning 
 * 100897  LAH: Added casts of int, uint_t, and char
 * 101097  LAH: Mdes hex constants 32 bits
 * 121297  LAH: Modified FXY insertion to increment FXY list counter
 * 012398  VLP: Modified Table C FXY_PtrInc if statement
 * 022498  LAH:  Added prints to bufr_log file.
 * 051298  VLP:  Added the missing flag line to the string definition.
 *
 */

#include <mel_bufr.h>
extern TableB_t*  TableB;
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Val_t LastVal;
extern BUFR_Cntl_t BUFR_Cntl;

#define PREMATURE_END   "Premature end of expanded FXY list"

#if PROTOTYPE_NEEDED

Status_t BUFR_Get_Value( BUFR_Val_t* BV, int IgnoreAFs )

#else

Status_t BUFR_Get_Value( BV, IgnoreAFs )
BUFR_Val_t* BV;
int         IgnoreAFs;

#endif
{

    BUFR_Val_t return_val;

    BitStream_t*  S4BS;
    EncVal_t      enc_val;
    double        d;
    char*         cp;
    Descriptor_t* descriptor;

    int         num_afs;
    AF_Entry_t* af_ent;

    int i, j, len;
    int dw, scale, ref_val;
    int nbits, sig;
    int X_val, Y_val;
    int m_flag;  /* missing value flag */

    TableB_Entry_t* BE;

    /* Values used for delayed replication expansion. */

    FXY_Entry_t *rep_beg, *rep_end, *rep_tmp;

    int      rep_fxys;      /* Number of FXY values to replicate. */
    int      rep_factor;    /* Replication factor. */

    FXY_t*       fxy_array;
    int          fxy_array_len;
    FXY_List_t*  fxy_list;
    FXY_Entry_t* fxy_ent;
    int hold, test;

    /*
     * Values used, after encountering 2-06-YYY, to process references to
     * local descriptors.
     */

    Descriptor_t *dtp;

    char buf[256];        /* Arbitrary size */
    char str1[9], str2[9]; /* test for zero filled end of message */

    /* check if compressed and if so process it */
    if ( BUFR_Msg.Compress_Flag == 1)
    {

        hold = Int2ToInt(BUFR_Msg.Section3.data_subsets);
        test = hold * BUFR_Msg.dc_numb;
        if ( BUFR_Msg.dc_parm_cnt >= test )
	      {
		      return BUFR_EOM;
	      }

        if ( BUFR_AtEOM() )
	         return BUFR_EOM;
	     
      	*BV = BUFR_Msg.decom_vals[BUFR_Msg.dc_parm_cnt];
      	BUFR_Msg.dc_parm_cnt++;
      	if ( BUFR_Msg.dc_parm_cnt % BUFR_Msg.dc_numb == 0) 
      	{
    	    BUFR_Msg.MsgStatus = BUFR_EOD;
	        BUFR_Msg.subset_index +=1;
	        if ( BUFR_Msg.subset_index > Int2ToInt(BUFR_Msg.Section3.data_subsets) )
	           BUFR_Msg.MsgStatus = BUFR_EOM;
	        return BUFR_EOD;
	      }
  	    return BUFR_OK;
    }
    if( BUFR_Msg.Section4_Data.buffer == NULL )
    {
        BUFR_Err_Set( "BUFR_Get_Value", "BUFR_Decode() hasn't been called" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Value", 
            "BUFR_Decode() hasn't been called" );
        return BUFR_ERROR;
    }
    else
        S4BS = &BUFR_Msg.Section4_Data;

    if( BUFR_ProcType() == TYPE_ENCODE )
    {
        BUFR_Err_Set( "BUFR_Get_Value",
            "Decoding function called while encoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Value", 
           "Decoding function called while encoding data" );
        return BUFR_ERROR;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Get_Value",
                "Can't mix raw and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Value", 
                "Can't mix raw and value-based processing methods.");
            return BUFR_ERROR;

        case METHOD_TEMPLATE:
            BUFR_Err_Set( "BUFR_Get_Value",
                "Can't mix template and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Value:", 
                "Can't mix template and value-based processing methods.");
            return BUFR_ERROR;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Get function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_DECODE;
            BUFR_Msg.MethFlag = METHOD_VALUES;
    }

    /* Don't proceed if we are at EOM. */

    /* Clear return value. */

    return_val = LastVal;

    BUFR_Val_free( &return_val);
    /* LAH 120402  Added next statement to clear return value in */
    /* case of EOF, EOM, or error  */
    *BV = LastVal = return_val;
/*
    if( return_val.AF != NULL )   
    {
        free( (void*) return_val.AF );
        free( (void*) return_val.AF_sig );
    }

    if ( return_val.Val_Type == DT_STRING )
    {
       if ( return_val.Val.string != NULL )
       {
          free((void*) return_val.Val.string);
       }
    }

    return_val.num_AFs    = 0;
    return_val.FXY_Val    = (FXY_t) BAD_FXY_VAL;
    return_val.Val_Type   = DT_UNKNOWN;
    return_val.Val.number = 0.0;
*/

GET_NEW_FXY:

    if( BUFR_AtEOM() )
        return BUFR_EOM;

    if( S4BS->byte_num >= S4BS->size )
        return BUFR_EOF;

    if( BUFR_Msg.exp_ptr == BUFR_Msg.exp_fxy_list->tail )
        return BUFR_EOF;

    if( FXY_IsTableB( BUFR_Msg.exp_ptr->fxy ) )
    {
        /* Get (possible) associated fields. */

        if( !IgnoreAFs && (num_afs=AF_List_Size( BUFR_Msg.af_list )) )
        {
            /* Create AF array. */

            /* 100897 LAH: Added unit_t cast */
            return_val.AF = (double*) malloc( (uint_t)num_afs * sizeof(double) );

            if( return_val.AF == NULL )
            {
                BUFR_Err_Set( "BUFR_Get_Value",
                    "Can't create associated field array" );
                return BUFR_ERROR;
            }

            /* Create AF significance array. */

            /* 100897 LAH: Added unit_t cast */
            return_val.AF_sig = (int*) malloc( (uint_t)num_afs * sizeof(int) );

            if( return_val.AF_sig == NULL )
            {
                BUFR_Err_Set( "BUFR_Get_Value",
                    "Can't create associated field significance array" );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Value:", 
	                  "Can't create associated field significance array");
                return BUFR_ERROR;
            }

            /* Get each AF and AF significance. */

            af_ent = AF_List_First( BUFR_Msg.af_list );

            for( i=0; i < num_afs; i++, af_ent=af_ent->next )
            {
                dw = af_ent->nbits;

                if( BitStream_Get( S4BS, &enc_val, dw ) )
                {
                    BUFR_Err_Log( "BUFR_Get_Value" );
                    return BUFR_ERROR;
                }
                else if( EncVal_Get( &d, enc_val, 0, 0, &m_flag) )
                {
                    BUFR_Err_Log( "BUFR_Get_Value" );
                    return BUFR_ERROR;
                }
                else
                {
                    return_val.AF[i] = d;
                    EncVal_Destroy( enc_val );
                }

                return_val.AF_sig[i] = af_ent->sig;
            }

            return_val.num_AFs = num_afs;
        }
        else
        {
            return_val.AF      = NULL;
            return_val.AF_sig  = NULL;
            return_val.num_AFs = 0;
        }

        /*
         * Get value from Section 4.
         *
         * It would be easier to call the FXY_Get_Scale/RefVal/DataWidth
         * functions but because this current function is heavily used,
         * get the values directly to increase performance.
         */

        if( FXY_Get_Values( BUFR_Msg.exp_ptr->fxy, &scale, &ref_val, &dw ) )
        {
            BUFR_Err_Log( "BUFR_Get_Value" );
            return BUFR_ERROR;
        }

        if( FXY_UnitsType( BUFR_Msg.exp_ptr->fxy ) == CCITT_IA5 )
        {
            /* Determine string length. */

            len = (dw / BITS_IN_BYTE);

            if( (dw % BITS_IN_BYTE) != 0 )
                len++;

            /* Allocate len+1 bytes for string plus NULL terminator. */

            /* 100997 LAH: Added uint_t cast */
            if( (return_val.Val.string = (char*) malloc( (uint_t) (len+1))) == NULL )
            {
                BUFR_Err_Set( "BUFR_Get_Value", "Can't allocate string" );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Value:", 
                   "Can't allocate string");
                return BUFR_ERROR;
            }
            else
                /* 100997 LAH: Added uint_t cast */
                memset( return_val.Val.string, 0, (uint_t) (len+1) );

            /* Get string from bit stream. */

            for( i=0, cp=return_val.Val.string; i < len; i++, cp++ )
            {
                if( BitStream_Get( S4BS, &enc_val, BITS_IN_BYTE ) )
                {
                    BUFR_Err_Log( "BUFR_Get_Value" );
                    free( (void*) return_val.Val.string );
                    return BUFR_ERROR;
                }
                else if( EncVal_Get( &d, enc_val, scale, ref_val,&m_flag) )
                {
                    BUFR_Err_Log( "BUFR_Get_Value" );
                    return BUFR_ERROR;
                }
                else
                {
                    /* 100997 LAH: Added char cast */
                    *cp = (char) d;
                    EncVal_Destroy( enc_val );
                }
            }
/*  VLP 5/12/98  added the missing flag line.  When a file is being decoded
		 and a string is the first item, the string was seen as
		 missing.  This addition fixes the problem */

            return_val.missing_flag = 1;
            return_val.Val_Type = DT_STRING;
            return_val.Val_Scale = 0;
        }
        else    /* value is numeric */
        {
          if( BitStream_Get( S4BS, &enc_val, dw ) )
          {
             BUFR_Err_Log( "BUFR_Get_Value" );
             return BUFR_ERROR;
          }
          else if( EncVal_Get( &d, enc_val, scale, ref_val, &m_flag) )
          {
            BUFR_Err_Log( "BUFR_Get_Value" );
            return BUFR_ERROR;
          }
          else
            EncVal_Destroy( enc_val );

/*   The variable missing_flag is added to the BUFR_Val_t structure so that
	the user can check to see if the value is of type missing and then
	place her own missing value indicator into the number variable if
	desired */

          if ( m_flag == 0 ) Missing_Value_Replace(&d, BUFR_Msg.exp_ptr->fxy);

          /* LAH 052302 modified to return all values with a scale of 0 or less as integers */
          /* LAH 052302 added scale to BV structure */
          if(scale <= 0 && BUFR_Cntl.All_Numbers_Not_Double)
          {
            return_val.Val_Type   = DT_INT;
            return_val.Val_Scale = scale;
	          if(m_flag ==  0 )
            {
	            return_val.missing_flag = 0;
              return_val.Val.int_number = (int)d;
            }else{
  	          return_val.missing_flag = 1;
              return_val.Val.int_number = (int)(d + .5);
            }
          } else {
            return_val.Val.number = d;
            return_val.Val_Scale = scale;
            return_val.Val_Type   = DT_DOUBLE;
            return_val.missing_flag = 1;
            if(m_flag ==  0 )
            {
              return_val.missing_flag = 0;
            }
          }
        }

        return_val.FXY_Val = BUFR_Msg.exp_ptr->fxy;

        (void) FXY_PtrInc();    /* Ignore possible error. */

        *BV = LastVal = return_val;

        /*
         * Don't goto RETURN_MSG_STATUS because BUFR_EOM will be returned if
         * this is the last FXY value.
         *
         * JRA082896 - Make sure value returns EOD.
         */

        if( BUFR_AtEOD() )
            return BUFR_EOD;
        else
            return BUFR_OK;

    }       /* if( FXY_IsTableB( BUFR_Msg.exp_ptr->fxy ) ) */

    if( FXY_IsTableC( BUFR_Msg.exp_ptr->fxy ) )
    {
        /* Process Table C descriptor(s). */

        X_val = FXY_X_Value( BUFR_Msg.exp_ptr->fxy );
        Y_val = FXY_Y_Value( BUFR_Msg.exp_ptr->fxy );

        switch( X_val )
        {
            case 1:     /* Change Data Width */

                if( Y_val == 0 )
                {
                    if( ValStack_Pop( &BUFR_Msg.DataWidthStack ) )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                }
                else
                {
                    Y_val -= 128;

                    if( ValStack_Push( &BUFR_Msg.DataWidthStack, Y_val ))
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                }

                break;

            case 2:     /* Change Scale */

                if( Y_val == 0 )
                {
                    if( ValStack_Pop( &BUFR_Msg.ScaleStack ) )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                }
                else
                {
                    Y_val -= 128;

                    if( ValStack_Push( &BUFR_Msg.ScaleStack, Y_val ))
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                }

                break;

            case 3:     /* Change Reference Value */

                if( Y_val == 0 )
                {
                    /* Reset all reference values. */

                    BE = TableB->head->next;

                    for( ; BE != TableB->tail; BE=BE->next )
                    {
                        while( ValStack_Pop( &BE->item->RefValStack ) == 0 )
                            ;
                    }
        	    if( FXY_PtrInc() )
        	    {
            	      return BUFR_EOM;
        	    }
/*    		    if( BUFR_AtEOD() )
        		    return BUFR_EOD;

    		    if( BUFR_AtEOM() )
    		    {
        		    if( BUFR_AtEOF() )
            		      return BUFR_EOF;
        		    else
            		      return BUFR_EOM;
    		    } */

                    goto GET_NEW_FXY;
                }

                /*
                 * Get bit width for new Reference Value from Y_val and gobble
                 * Table B FXYs until 2-03-255 (0x83FF) is encountered.
                 */

                dw = Y_val;     /* Bit width for new RefVals. */

                if( FXY_PtrInc() )
                {
                    BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
                    return BUFR_ERROR;
                }

                while( BUFR_Msg.exp_ptr->fxy != (FXY_t)0x000083FF )
                {
                    if( FXY_IsTableB( BUFR_Msg.exp_ptr->fxy ) )
                    {
                        /* Read reference value. */

                        if( BitStream_Get( S4BS, &enc_val, dw ) )
                        {
                            BUFR_Err_Log( "BUFR_Get_Value" );
                            return BUFR_ERROR;
                        }
                        else if( EncVal_Get( &d, enc_val, 0, 0, &m_flag) )
                        {
                            BUFR_Err_Log( "BUFR_Get_Value" );
                            return BUFR_ERROR;
                        }
                        else
                            EncVal_Destroy( enc_val );

                        descriptor = TableB_Get( BUFR_Msg.exp_ptr->fxy );

                        if( descriptor == NULL )
                        {
                            sprintf( buf, "Unknown descriptor (%s)",
                                FXY_String( BUFR_Msg.exp_ptr->fxy ) );
                            BUFR_Err_Set( "BUFR_Get_Value", buf );
                            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                               "BUFR_Get_Value:", buf);
                            return BUFR_ERROR;
                        }

                        if( descriptor->units_type != CCITT_IA5 )
                        {
                            /*
                             * Push new reference value onto Table B
                             * descriptor's stack.
                             */

                            if(ValStack_Push(&descriptor->RefValStack,(int)d))
                            {
                                BUFR_Err_Log( "BUFR_Get_Value" );
                                return BUFR_ERROR;
                            }
                        }

                        if( FXY_PtrInc() )
                        {
                            BUFR_Err_Set("BUFR_Get_Value",PREMATURE_END);
                            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                              "BUFR_Get_Value:", PREMATURE_END);
                            return BUFR_ERROR;
                        }

                        continue;
                    }

                    /* This FXY had better be 2-03-255 (0x83FF)! */

                    if( BUFR_Msg.exp_ptr->fxy != (FXY_t)0x000083FF )
                    {
                        sprintf( buf, "Expected 2-03-255, got %s",
                            FXY_String(BUFR_Msg.exp_ptr->fxy) );
                        BUFR_Err_Set( "BUFR_Get_Value", buf );
                        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                           "BUFR_Get_Value:", buf);
                        return BUFR_ERROR;
                    }
                    else
                    {
                        /* RefVal redefinition complete, get next FXY. */

                        break;
                    }
                }           /* while( BUFR_Msg.exp_ptr->fxy != 0x83FF ) */

                break;      /* Get next FXY. */

            case 4:     /* Add associated field */

                if( Y_val == 0 )    /* Cancel associated field. */
                {
                    if( AF_List_Remove( BUFR_Msg.af_list ) )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                }
                else
                {
                    nbits = Y_val;      /* Save AF bit width. */

                    /* Get next FXY.  It better be AF_SIG_FXY. */

                    if( FXY_PtrInc() )
                    {
                        BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
                        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                           "BUFR_Get_Value:", PREMATURE_END);
                        return BUFR_ERROR;
                    }

                    if( BUFR_Msg.exp_ptr->fxy != (FXY_t)AF_SIG_FXY )
                    {
                        sprintf( buf, "%s %s %s",
                            "Associated Field operator not followed",
                            "by AF Significance descriptor",
                            FXY_String( AF_SIG_FXY ) );
                        BUFR_Err_Set( "BUFR_Get_Value", buf );
                        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                            "BUFR_Get_Value:", buf);
                        return BUFR_ERROR;
                    }

                    /* Get AF significance from Section 4. */

                    if( (dw=FXY_Get_DataWidth( BUFR_Msg.exp_ptr->fxy )) == 0 )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                    else if( BitStream_Get( S4BS, &enc_val, dw ) )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                    else if( EncVal_Get( &d, enc_val, 0, 0, &m_flag) )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                    else
                    {
                        /* 100897 LAH: Added int cast */
                        sig = (int) d;
                        EncVal_Destroy( enc_val );
                    }

                    /* Add associated field. */

                    if( AF_List_Put( BUFR_Msg.af_list, nbits, sig ) )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                }

                break;      /* Get next FXY. */

            case 5:     /* Signify character */

                /*
                 * Get 'Y_val' bytes of character data.  Allocate one
                 * extra byte for a NULL string terminator.
                 */

                /* 100897 LAH: Added uint_t cast */

                if((return_val.Val.string=(char*)malloc( (uint_t) (Y_val+1) ))==NULL)
                {
                    BUFR_Err_Set( "BUFR_Get_Value", "Can't allocate string" );
                    fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
		          	    "BUFR_Get_Value:", "Can't allocate string");
                    return BUFR_ERROR;
                }

                return_val.Val_Type = DT_STRING;

                for( i=0, cp=return_val.Val.string; i < Y_val; i++, cp++ )
                {
                    if( BitStream_Get( S4BS, &enc_val, BITS_IN_BYTE ) )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                    else if( EncVal_Get( &d, enc_val, 0, 0, &m_flag) )
                    {
                        BUFR_Err_Log( "BUFR_Get_Value" );
                        return BUFR_ERROR;
                    }
                    else
                    {
                        /* 100897 LAH: Added char cast */
                        *cp = (char) d;
                        EncVal_Destroy( enc_val );
                    }
                }

                /* 092997  LAH: Added cast to correct Linux warning */
                *cp = (char) NULL;     /* Terminate string. */

                /*
                 * JRA012497: There most certainly is an FXY value!
                return_val.FXY_Val = NO_FXY_VAL;
                 */

                return_val.FXY_Val = BUFR_Msg.exp_ptr->fxy;
                return_val.Val_Scale = 0;

/*********************************************************************
                JRA012497: If this string is the last in the message,
                an error will be reported when none exists.

                if( FXY_PtrInc() )
                {
                    BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
                    return BUFR_ERROR;
                }
*********************************************************************/

                (void) FXY_PtrInc();

                *BV = LastVal = return_val;

                goto RETURN_MSG_STATUS;

            case 6:     /* Signify data width for next local descriptor */

                /*
                 * Make sure that the next local descriptor is a Table B
                 * descriptor (in the form 0-XX-YYY).  If it isn't,
                 * complain and ignore this 2-06-YYY descriptor.
                 */

                if( !FXY_IsTableB( BUFR_Msg.exp_ptr->next->fxy ) )
                {
                    fprintf( stderr, "BUFR_Get_Value(), WARNING: " );
                    fprintf( stderr,
                        "2-06-%03d followed by %s instead of\n",
                        Y_val, FXY_String( BUFR_Msg.exp_ptr->next->fxy ));
                    fprintf( stderr, "a Table B descriptor.  Ignoring " );
                    fprintf( stderr, "the %s descriptor.\n",
                        FXY_String( BUFR_Msg.exp_ptr->fxy ) );
                    fprintf( BUFR_Cntl.bufr_log,
                        "BUFR_Get_Value(), WARNING: " );
                    fprintf( BUFR_Cntl.bufr_log,
                        "2-06-%03d followed by %s instead of\n",
                        Y_val, FXY_String( BUFR_Msg.exp_ptr->next->fxy ) );
                    fprintf( BUFR_Cntl.bufr_log, "a Table B descriptor.  Ignoring " );
                    fprintf( BUFR_Cntl.bufr_log, "the %s descriptor.\n",
                        FXY_String( BUFR_Msg.exp_ptr->fxy ) );
                    /* reset Y_val -- no longer needed */
                    /* LAH 11/20/00 */
                     Y_val = 0;
                }
                else
                {
                    dtp = TableB_Get(BUFR_Msg.exp_ptr->next->fxy );
                    if( dtp == NULL )
                    {
                        /*
                         * The local descriptor is not stored in Table B.
                         * Since we don't know how to decode the local
                         * descriptor, skip over it and advance the
                         * Section 4 bitstream pointer by Y_val number
                         * of bits.
                        */
#if TRACE_PRINT
                        fprintf(BUFR_Cntl.bufr_log, 
                            "Skipping %d-bit local descriptor %s\n",
                            Y_val,
                            FXY_String( BUFR_Msg.exp_ptr->next->fxy ) );
#endif

                        /*
                         * Advance FXY pointer.  It will be advanced
                         * again (below) before processing continues.
                         */

                        (void) FXY_PtrInc();

                        if( BitStream_Position(&BUFR_Msg.Section4_Data,Y_val) )
                        {
                            BUFR_Err_Log( "BUFR_Get_Value" );
                            return BUFR_ERROR;
                        }
                        /* reset Y_val -- no longer needed */
                        /* LAH 11/20/00 */
                        Y_val = 0;
                    }
                    else
                    {
                        /*
                         * The local descriptor is stored in Table B.  We know
                         * what the local descriptor is so we can process it.
                         * Complain if Y_val does not match the data width;
                         * the data width in Table B overrides whatever
                         * Y_val is.
                         */

                        if( dtp->data_width != Y_val )
                        {
                            fprintf( BUFR_Cntl.bufr_log, "BUFR_Get_Value(), WARNING: " );

                            fprintf(BUFR_Cntl.bufr_log, "%s %s %s\n",
                                "descriptor ",
                                FXY_String( BUFR_Msg.exp_ptr->fxy ),
                                "indicates that" );

                            fprintf( BUFR_Cntl.bufr_log, "    %s %s %s %d %s\n",
                                "local descriptor ",
                                FXY_String( BUFR_Msg.exp_ptr->next->fxy ),
                                "is",
                                Y_val,
                                "bits wide but the BUFR table" );

                            fprintf( BUFR_Cntl.bufr_log, "    %s %d %s\n",
                                "indicates that it is ",
                                dtp->data_width,
                                "bits wide." );

                            fprintf( BUFR_Cntl.bufr_log, "    %s %s\n",
                                "Ignoring Table C descriptor and using ",
                                "BUFR table data width instead." );
                            /* reset Y_val -- no longer needed */
                            /* LAH 11/20/00 */
                            Y_val =0;

                        }
                    }
                }

                break;

            default:
                sprintf( buf, "Invalid Table C descriptor (%s)",
                    FXY_String( BUFR_Msg.exp_ptr->fxy ) );
                BUFR_Err_Set( "BUFR_Get_Value", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                  "BUFR_Get_Value:", buf);
                return BUFR_ERROR;

        }   /* switch( X_val ) */

        /* Table C value has been processed.  Now go get an actual value. */

/*  Modified by VLP  Jan. 23, 1998 */
/*  Check to see if there is no additional value and the Y value is zero. */
/*  This takes care of the case where the Table C value has been reset and */
/*  there are no additional data in the bitstream */

        if ( BUFR_Msg.MsgStatus != BUFR_OK )
        {
           if (Y_val == 0)
           {
             return_val.FXY_Val = (FXY_t) IGNORE_FXY;
             *BV = LastVal = return_val;
             return BUFR_Msg.MsgStatus;
           } else 
           {
              BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
              fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                 "BUFR_Get_Value:", PREMATURE_END);
              return BUFR_ERROR;
           }
         }

         (void) FXY_PtrInc();    /* Ignore possible error. */

         goto GET_NEW_FXY;

    }       /* end if( FXY_IsTableC( BUFR_Msg.exp_ptr->fxy ) ) */

    if( FXY_IsReplicator( BUFR_Msg.exp_ptr->fxy ) )
    {
        /* Get number of FXY's to replicate. */

        rep_fxys = FXY_X_Value( BUFR_Msg.exp_ptr->fxy );

        /* The next FXY must be 0-31-001 (0x1F01) or 0-31-002 (0x1F02) or */
        /*      0-31-000 (0x1f00) . */

        if( FXY_PtrInc() )
        {
            BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
               "BUFR_Get_Value:", PREMATURE_END);
            return BUFR_ERROR;
        }

        if( BUFR_Msg.exp_ptr->fxy!=(FXY_t)0x00001F01 && BUFR_Msg.exp_ptr->fxy!=(FXY_t)0x00001F02 &&
                BUFR_Msg.exp_ptr->fxy != (FXY_t)0x00001F00)
        {
            sprintf( buf, "Expected delayed descriptor replication" );
            BUFR_Err_Set( "BUFR_Get_Value", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
              "BUFR_Get_Value:", buf);
            return BUFR_ERROR;
        }

        /* Get the replication factor from Section 4. */

        if( (dw=FXY_Get_DataWidth( BUFR_Msg.exp_ptr->fxy )) == 0 )
        {
            BUFR_Err_Log( "BUFR_Get_Value" );
            return BUFR_ERROR;
        }
        else if( BitStream_Get( S4BS, &enc_val, dw ) )
        {
            BUFR_Err_Log( "BUFR_Get_Value" );
            return BUFR_ERROR;
        }
        else if( EncVal_Get( &d, enc_val, 0, 0, &m_flag) )
        {
            BUFR_Err_Log( "BUFR_Get_Value" );
            return BUFR_ERROR;
        }
        else
        {
            rep_factor = (int) d;
            EncVal_Destroy( enc_val );
        }

        if( rep_factor == 0 )
        {
            /*
             * Since the delayed replication factor is 0, there are no data
             * values in Section 4 corresponding to the FXY sequence to
             * replicate.  Increment the expanded FXY list pointer to point
             * past the replication factor FXY (0-31-000, -001, or -002), skip
             * over the next 'rep_fxys' number of FXY values, and get the
             * next (possible) FXY value.
             */

            for( i=0; i < (rep_fxys+1); i++ )
            {
            /*************************************************************
                if( FXY_PtrInc() )
                {
                    BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
                    return BUFR_ERROR;
                }
            **************************************************************/

                /* FXY_PtrInc() return 0 if OK, 1 on error, and -1 at EOF. */

                if( FXY_PtrInc() == 1 )
                {
                    BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
                    fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                       "BUFR_Get_Value:", PREMATURE_END);
                    return BUFR_ERROR;
                }
            }

            /*
             * If the replication sequence is at the end of a dataset,
             * save the state of BUFR_Msg.MsgStatus so that when we
             * get the next FXY value a BUFR_EOD will be returned to
             * indicate that EOD was encountered and this function is
             * returning the first value in the
             */

            if( BUFR_Msg.MsgStatus == BUFR_OK )
            {
                /* Get the FXY value after this non-replicated FXY sequence. */

                goto GET_NEW_FXY;
            }
            else
            {
                *BV = LastVal = return_val;
                return BUFR_Msg.MsgStatus;
            }

        }

#if DEBUG_PRINT
        if( BUFR_DebugLevel() > 4 )
        {
            fprintf(BUFR_Cntl.bufr_log, 
                "Processing delayed descriptor %s (%d repetitions)\n",
                FXY_String( BUFR_Msg.exp_ptr->fxy ), rep_factor );
            fprintf(BUFR_Cntl.bufr_log,  "FXY list before replication:\n");
            FXY_List_Print( BUFR_Msg.exp_fxy_list, BUFR_Cntl.bufr_log );
        }
#endif

        /*
         * Advance expanded FXY list pointer to the beginning of the
         * sequence of FXY values to be replicated.  Save this location
         * in order to reset the BUFR_Msg pointer later on.
         */

        if( FXY_PtrInc() )
        {
            BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
              "BUFR_Get_Value:", PREMATURE_END);
            return BUFR_ERROR;
        }
        else
            rep_beg = BUFR_Msg.exp_ptr;

        /*
         * Remove 1-xx-000 and replication factor (0-31-001 or 0-31-002) from
         * FXY list.  Since FXY_List_Remove() returns the previous entry, do
         * this in reverse order.
         */

        rep_tmp = FXY_List_Prev( BUFR_Msg.exp_fxy_list, rep_beg );

        rep_tmp = FXY_List_Remove( BUFR_Msg.exp_fxy_list, rep_tmp );
        rep_tmp = FXY_List_Remove( BUFR_Msg.exp_fxy_list, rep_tmp );

        /* Collect FXY values to be replicated. */

        fxy_array_len = rep_fxys;

        /* 100897 LAH: Added uint_t cast */
        fxy_array = (FXY_t*) malloc( (uint_t) fxy_array_len * sizeof(FXY_t) );

        if( fxy_array == NULL )
        {
            BUFR_Err_Set( "BUFR_Get_Value",
                "Can't create FXY array for replication" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                "BUFR_Get_Value:",
                "Can't create FXY array for replication");
            return BUFR_ERROR;
        }

        for( i=0; i < fxy_array_len; i++ )
        {
            fxy_array[i] = BUFR_Msg.exp_ptr->fxy;

            /*
             * Don't increment the pointer if this is the last replicated FXY
             * value to get.
             */

            if( i == (fxy_array_len-1) )
                break;

            if( FXY_PtrInc() )
            {
                BUFR_Err_Set( "BUFR_Get_Value", PREMATURE_END );
                free( (void*) fxy_array );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                  "BUFR_Get_Value:", PREMATURE_END);
                return BUFR_ERROR;
            }
        }

        /* Get the end of the replicated sequence. */

        rep_end = BUFR_Msg.exp_ptr;

        /* Create expanded FXY list from fxy_array. */

        if( (fxy_list=FXY_List_Expand( fxy_array, fxy_array_len )) == NULL )
        {
            BUFR_Err_Log( "BUFR_Get_Value" );
            free( (void*) fxy_array );
            return BUFR_ERROR;
        }

        /* FXY array has been expanded and is no longer needed. */

        free( (void*) fxy_array );

        /*
         * Replicate the expanded FXY list.  The first set of fxys in the
         * expanded FXY list did not include expansion of any delayed
         * replicators.  Therefore,  the orginal delayed replication sequence
         * needs to be replaced in case there were any Table D descriptors or
         * replications included within the replicated sequence.  Then the
         * new expanded sequence is replicated rep_factor times and replaces
         * the orginal sequence.
         *
         * Therefore,  the orginal replicated sequence must me removed.
         * The descriptors are deleted in reverse order starting at the
         * location pointed to by rep_end and ending at the location pointed
         * to by rep_beg.
         *
         * Since the location pointed to by rep_beg is delated, the pointer
         * to the previous FXY must be saved.  After the original sequence
         * has been replaced, this pointer is then incremented to point to
         * the correct FXY.
         */

        /* Set BUFR_Msg.exp_ptr to entry prior to rep_beg */
        BUFR_Msg.exp_ptr = FXY_List_Prev( BUFR_Msg.exp_fxy_list, rep_beg);

        /*
         * These changes were made by Louis Hembree on January 10, 1997 at 2230.
         */

        /* remove orginal sequence that was exapnded further.  */
        rep_tmp = rep_end;
        for ( i = 0; i < rep_fxys; i++)
        {
            rep_tmp = FXY_List_Remove(BUFR_Msg.exp_fxy_list,  rep_tmp);
        }
        rep_end = rep_tmp;

        /* for( j=0; j < (rep_factor-1); j++ ) */
        for( j=0; j < rep_factor; j++ )
        {
            rep_tmp = rep_end;

            fxy_ent = FXY_List_First( fxy_list );

            for( ; fxy_ent != fxy_list->tail; fxy_ent=fxy_ent->next )
            {
                /* Insert FXY entry after rep_tmp. */

                if( FXY_List_Insert( rep_tmp, fxy_ent->fxy, BUFR_Msg.exp_fxy_list ) )
                {
                    BUFR_Err_Log( "BUFR_Get_Value" );
                    FXY_List_Destroy( fxy_list );
                }
                else
                    rep_tmp = rep_tmp->next;
            }
        }

        /* Since it's no longer needed, get rid of fxy_list. */

        FXY_List_Destroy( fxy_list );

        /*
         * Reset the BUFR_Msg expanded FXY list pointer to the beginning of
         * the replicated FXY values.
         */

        /* BUFR_Msg.exp_ptr = rep_beg; */
        FXY_PtrInc();

#if DEBUG_PRINT
        if( BUFR_DebugLevel() > 4 )
        {
            fprintf(BUFR_Cntl.bufr_log, 
	        "FXY list after replication:\n" );
            FXY_List_Print( BUFR_Msg.exp_fxy_list, BUFR_Cntl.bufr_log );
        }
#endif

        /* Go get the first (replicated) FXY value. */

        goto GET_NEW_FXY;

    }   /* if( FXY_IsReplicator( BUFR_Msg.exp_ptr->fxy ) ) */

    /****************************************/
    /* Logic should never reach this point! */
    /****************************************/
    sprintf(str1, "3-63-255");
    sprintf(str2, "%s",  FXY_String( BUFR_Msg.exp_ptr->fxy ) );
 
    if(!(strcmp(str1, str2))) return BUFR_EOM;

    sprintf( buf, "Don't know how to handle FXY value %s",
        FXY_String( BUFR_Msg.exp_ptr->fxy ) );

    BUFR_Err_Set( "BUFR_Get_Value", buf );
    return BUFR_ERROR;

RETURN_MSG_STATUS:

    if( BUFR_AtEOD() )
        return BUFR_EOD;

    if( BUFR_AtEOM() )
    {
        if( BUFR_AtEOF() )
            return BUFR_EOF;
        else
            return BUFR_EOM;
    }

    return BUFR_OK;
}
