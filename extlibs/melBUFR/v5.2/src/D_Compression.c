/* De-Compress a bufr file 
 * 
 * D_Compression - VERSION: %I%  %E% %T%
 */
#include <math.h>
#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         D_Compression
 
  DESCRIPTION:         D_Compression takes the binary bufr data that is produced
			by the user and de-compresses the data by the format
			given in the WMO No. 306
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, JULY 98
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, JULY 98
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int D_Compression( )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Unknown FXY Value
  Delayed Replication
  Expanding FXYs Problem
  malloc allocation Problem
  Error from BitStream_Get
  Error from EncVal_Get
  No Table B after Table C
  Invalude Table C Descriptor
  Error from FXY_Get_Values
  Error from BitStream_Put

  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  FXY_IsTableD		Checks to see if a FXY is a Table D FXY
  FXY_Expand		Expands Table D FXYs
  free		Frees malloced memory
  TableB_Get		Returns a descriptor for a given Table B FXY
  FXY_F_Value		Strips out the F value of a FXY
  FXY_X_Value		Strips out the X value of a FXY
  FXY_Y_Value		Strips out the Y value of a FXY
  FXY_IsTableC		Checks to see if a FXY is a Table C FXY
  FXY_List_Expand 	Expands all Table Ds and replicated FXY and places
			the values, allong with Table Bs, into a single list
  BUFR_Err_Log		Places error messages into the Bufr error log
  FXY_List_First	Returns a pointer to the first element in a FXY list
  BUFR_Err_Set		Sets an error message
  BitStream_Get		Gets requested number of bits from a Bit Stream
  EncVal_Destroy	Releases allocated memory
  FXY_Get_Values	Returns scale, reference value, and data width for a FXY
  FXY_UnitsType		Returns the units type for a FXY
  EncVal_Get		Scales and references a value from BitStream_Get
  BitStream_Destroy	Destroys a bit stream
  BitStream_Init	Initializes a new bit stream
  EncVal_Set		Takes a value and encodes it into a hex value
  TenPow		Returns ten raised to a given integer power
  BUFR_BitWidth		Returns the bit width of a given value
  BitStream_Put		Places an encoded value into a bit stream 
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg	BUFR_Msg_t	BUFR messages structure
  BM		BUFR_Msg_t	Pointer to a BUFR messages structure
  d_con		BUFR_Val_T	Array of decoded data values
  fxy		FXY_t		Single FXY
  fxy_list	FXY_t		Table Ds expanded list
  fxy_partial	FXY_t		Array of all the unexpanded FXYs
  fxy_complete	FXY_t		Array of all the FXYs expanded
  ExpList	FXY_List_t	List returned from FXY_List_Expand
  fxy_ent	FXY_Entry_t	First entry of the ExpList
  S4BS		BitStream_t	Section 4 Bit Stream
  num_fxys	int		Number of FXYs from FXY_Expand 
  f, i, j, k, n int		Loop counters
  total_count	int		total number of FXYs in Section 3
  tableb_ct	int		total number of data points for each set
  nsets		int		number of data sets
  inum		int		counter
  dw		int		data width (number of bits)
  scale		int		scale of data
  rv0		int		reference value
  s0		int		scale of data from Table C values
  tc_flag	int		indicates there is a table c value
  dwn		int		bit width of compressed value
  cc_flag	int		indicates that the data is character data
  enc_val	EncVal_t	Encoded hexidecimal value to/from Section 4
  low_num	double		Base value for compressed data
  hi_num	double		Highest possible value to get data width for
  val, xnum	double		data value
  d		Descriptor_t	descriptor for a Table B FXY
  cp		char		character pointer
  temp_string	char		Blank, empty string that is the min or base
				value in the compressed data
  dwc_f		int		Table C data width indicator flag
  scc_f		int		Table C scale indicator flag
  dw6_f		int		Table C data width from 6 indicator flag
  X_val, Y_val	int		Decoded X and Y value from a FXY
  buf		char		character string of error message
  s3_len	int		Section 3 FXY length
 
  METHOD:
    Loop on the length of Section 3 FXY area
      count how many FXYs there are and how many expanded FXYs
    End Loop

    Malloc the data array
    Malloc the FXY arrays

    Loop on the length of Section 3 FXY area
      place the FXYs from that area into the fxy_partial array
    End loop
    
    Expand the fxy_partial array by using FXY_List_Expand
    Perform FXY_List_First to find the first element of the expanded FXY list

    Loop on the expanded list of fxys
      place the expanded fxys into the array fxy_complete
    End Loop

    Loop through the number of data sets
      Loop through the number of fxys
	Read data from Section 4, decode based on the fxy, and place into the
		array d_con
      End Loop
    End Loop

    Perform BitStream_Desctroy to destroy the Section 4 bitstream
    Perform BitStream_Init to reinitialize the Section 4 bitstream

    Loop on number of fxys
      Perfrom FXY_Get_Values to scale, reference value, and data width for each
	FXY
      If the FXY is a Table C value, set the appropriate flag and go to the
	bottom of the loop
      Check to see if the FXY is associated with a character string
      If so then
	Place blank string of data width length in Section 4 bitstream
	Place data width length (in octets) in Section 4 bitstream
      End if

      Loop on number of data sets
	If data is character in nature then
	  Place the character data into the bitstream
	Endif
	Check to see if data value is lower than low_num
	Check to see if data value is higher that hi_num
      End Loop (data sets)

      If data is character string data bounce to bottom of loop

      Rescale hi_num and low_num into integer values
      Perform BUFR_BitWidth to get bit width of hi_num
      Perform EncVal_Set to put the low_num into an encoded value
      Perform BitStream_Put to put the encoded value into the bitstream
      Perform EncVal_Set to put the data width into an encoded value
      Perform BitStream_Put to put the encoded value into the bitstream

      Loop on number of data sets
	get data value, rescale, and subtract low_num from it
      	Perform EncVal_Set to put the data value into an encoded value
      	Perform BitStream_Put to put the encoded value into the bitstream
      End loop
    End loop (number of fxys)
 
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

int D_Compression(void)

#else

int D_Compression()

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    BUFR_Msg_t* BM;
    BUFR_Val_t* d_con;
    Section3_t* s3;
    FXY_t fxy, *fxyc, *fxy_complete;
    FXY_Entry_t* fxy_ent;
    BitStream_t*  S4BS;
    int total_count = 0;
    int tableb_ct = 0;
    int nsets = 0;
    int i, j, k;
    int inum = 0;
    int dw, scale, rv0, s0, ref_val;
    int new_dw;
    int cc_flag;
    EncVal_t      enc_val;
    double min_val, val;
    char* cp;
    int dwc_f, scc_f, dw6_f;
    double dwc, scc, dw6;
    int X_val, Y_val;
    char buf[80];
    char *temp_string;
    int m_flag, min_val_miss;
     
    BM = &BUFR_Msg;
    s3 = &BM->Section3;
    S4BS = &BUFR_Msg.Section4_Data;
    dwc_f = 0;
    scc_f = 0;
    dw6_f = 0;
    
/*  Go through the fxys and count how many total fxys and total data
    elements there will be. (The two are not necessarily the same.) */
    
    total_count = BUFR_Msg.exp_fxy_list->num_fxys;
    fxy_complete = (FXY_t *) malloc(sizeof(FXY_t) * total_count);
    
    for ( fxy_ent = BUFR_Msg.exp_fxy_list->head->next, fxyc = fxy_complete;
          fxy_ent != BUFR_Msg.exp_fxy_list->tail;
	  fxy_ent = fxy_ent->next, fxyc++)
    {
	*fxyc = fxy_ent->fxy;
      if ( FXY_IsTableB(fxy_ent->fxy))
          tableb_ct++;
      if ( FXY_IsTableC(fxy_ent->fxy))
      {
        X_val = FXY_X_Value(fxy_ent->fxy);
        if (X_val >= 22)
           tableb_ct++;
      }
    }

    nsets = Int2ToInt(s3->data_subsets);
    d_con =(BUFR_Val_t *) malloc(sizeof(BUFR_Val_t) * (nsets * (tableb_ct+1)));
    (void *) memset(d_con, 0, (nsets * (tableb_ct+1)));

    inum = 0;
/* Go through list of fxys */

    for(i = 0; i <= (total_count-1); i++)
    {
      fxy = fxy_complete[i];
      if( FXY_IsTableC( fxy ) )
      {
        /* Process Table C descriptor(s). */

        X_val = FXY_X_Value( fxy );
        Y_val = FXY_Y_Value( fxy );

        switch( X_val )
        {
            case 1:     /* Change Data Width */
                if( Y_val == 0 )
                {
		  dwc_f = 0;
            } else {
                    Y_val -= 128;
		    dwc = Y_val;
		    dwc_f = 1;
                }
                continue;

            case 2:     /* Change Scale */
                if( Y_val == 0 )
                {
		    scc_f = 0;
            } else {
                    Y_val -= 128;
		    scc = Y_val;
		    scc_f = 1;
                }
                continue;

            case 5:     /* Signify character */

                /*
                 * Get 'Y_val' bytes of character data.  Allocate one
                 * extra byte for a NULL string terminator.
                 */

            if((temp_string=(char*)malloc( (uint_t) (Y_val+1)))==NULL)
                {
                    BUFR_Err_Set( "D_Compression", "Can't allocate string" );
                    fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                            "BUFR_Get_Value:", "Can't allocate string");
		    free (fxy_complete);
                    return BUFR_ERROR; 
                }
                d_con[j].Val_Type = DT_STRING;
                for( j=0, cp=temp_string; j < Y_val; j++, cp++ )
                {
                    if( BitStream_Get( S4BS, &enc_val, BITS_IN_BYTE ) )
                    {
                        BUFR_Err_Log( "D_Compression" );
		        free (fxy_complete);
                        return BUFR_ERROR; 
              } else if( EncVal_Get( &val, enc_val, 0, 0, &m_flag) )
                    {
                        BUFR_Err_Log( "D_Compression" );
		        free (fxy_complete);
                        return BUFR_ERROR; 
              } else
                    {
                        /* 100897 LAH: Added char cast */
                        *cp = (char) val;
                        EncVal_Destroy( enc_val );
                    }
                }
                for( j=0; j < nsets; j++ )
                {
            	  d_con[inum+(j*tableb_ct)].missing_flag = 1;
                  if((d_con[inum+(j*tableb_ct)].Val.string=
			           (char*)malloc( (uint_t) (Y_val+1)))==NULL)
                  {
                    BUFR_Err_Set( "D_Compression", "Can't allocate string" );
                    fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                            "BUFR_Get_Value:", "Can't allocate string");
		    free (fxy_complete);
                    return BUFR_ERROR; 
                  }
          	  d_con[inum+(j*tableb_ct)].Val_Type   = DT_STRING;
		  strcpy(d_con[inum+(j*tableb_ct)].Val.string, temp_string);
		}

                /* 092997  LAH: Added cast to correct Linux warning */
                *cp = (char) NULL;     /* Terminate string. */
		free(temp_string);     /* Terminate string. */
                continue;

            case 6:     /* Signify data width for next local descriptor */

                /*
                 * Make sure that the next local descriptor is a Table B
                 * descriptor (in the form 0-XX-YYY).  If it isn't,
                 * complain and ignore this 2-06-YYY descriptor.
                 */
                if( !FXY_IsTableB( fxy_complete[(i+1)] ) )
                {
                    fprintf( stderr, "D_Compress(), WARNING: " );
                    fprintf( stderr,
                        "2-06-%03d followed by %s instead of\n",
                        Y_val, FXY_String( fxy_complete[i+1]));
                    fprintf( stderr, "a Table B descriptor.  Ignoring " );
                    fprintf( stderr, "the %s descriptor.\n",
                        FXY_String( fxy_complete[i] ) );
                    fprintf( BUFR_Cntl.bufr_log,
                        "D_Compress(), WARNING: " );
                    fprintf( BUFR_Cntl.bufr_log,
                        "2-06-%03d followed by %s instead of\n",
                        Y_val, FXY_String( fxy_complete[i+1]) );
                    fprintf( BUFR_Cntl.bufr_log, 
			"a Table B descriptor.  Ignoring " );
                    fprintf( BUFR_Cntl.bufr_log, "the %s descriptor.\n",
                        FXY_String( fxy_complete[i] ) );
            } else
                {
		    dw6 = Y_val;
		    dw6_f = 1;
                }
                continue;
          case 22:
          case 23:
          case 24:
          case 25:
          case 26:
          case 27:
          case 28:
          case 29:
          case 30:
          case 31:
          case 32:
          case 33:
          case 34:
          case 35:
          case 36:
          case 37:
            for ( j = 0; j < nsets; j++)
            {
              d_con[inum + (j * tableb_ct)].FXY_Val =  fxy;
              d_con[inum + (j * tableb_ct)].missing_flag =  0;
              d_con[inum + (j * tableb_ct)].Val_Type = DT_UNKNOWN;
              d_con[inum + (j * tableb_ct)].Val.int_number = 0;
            }
            inum++;
            continue;
            default:
                sprintf( buf, "Invalid Table C descriptor (%s)",
                    FXY_String( BUFR_Msg.exp_ptr->fxy ) );
                BUFR_Err_Set( "D_Compression", buf );
                free(fxy_complete);
                return BUFR_ERROR; 
          }   /* switch( X_val ) */
      } else
      {
          if(FXY_Get_Values(fxy, &scale, &rv0, &dw))
	  {
	    BUFR_Err_Log( "Bad FXY in D_Compress" );
	    printf(" bad fxy -- not defined \n");

	    sprintf( buf, " %d %s = Unknown FXY value \n", 
	    total_count, FXY_String(fxy)); 
	    BUFR_Err_Set( "D_Compression", buf );
            for(j = 0; j < nsets; j++){   
              d_con[inum+(j*tableb_ct)].missing_flag = 1;
              d_con[inum+(j*tableb_ct)].Val_Type   = DT_UNKNOWN;
	    }
	    free (fxy_complete);
            return -1; 
	  }
        if(dwc_f == 1) dw += (int) dwc;
        if(dw6_f == 1) dw = (int) dw6;
        if(scc_f == 1) scale += (int) scc;
          cc_flag = 0;
  
          if(FXY_UnitsType(fxy)  == CCITT_IA5 ) cc_flag = 1;

/*  This section is to get the minimum value for the string variable.
        *   The minimum value should be all blanks
        */

        if(cc_flag == 1)
        {
	    dw = (dw / BITS_IN_BYTE);

/*          printf(" string variable of %d bytes\n", dw); */
/*printf("fake char string %d %d %d \n", dw, new_dw, inum);*/
	        if((temp_string = malloc( (uint_t) (dw+1))) == NULL)
            {
              BUFR_Err_Set( "D_Compression", "Can't allocate string" );
	      free (fxy_complete);
              return BUFR_ERROR; 
          } else
                /* 100997 LAH: Added uint_t cast */
              memset( temp_string, 0, (uint_t) (dw+1) );

              /* Get string from bit stream. */

            for( j=0, cp=temp_string; j < dw; j++, cp++ )
            {
              if( BitStream_Get( S4BS, &enc_val, BITS_IN_BYTE ) )
              {
                  BUFR_Err_Log( "D_Compression" );
              free( (void*) temp_string );
		  free (fxy_complete);
                  return BUFR_ERROR; 
            } else if( EncVal_Get( &val, enc_val, scale, rv0, &m_flag) )
              {
                BUFR_Err_Log( "D_Compression" );
              free( (void*) temp_string );
		free (fxy_complete);
                return BUFR_ERROR; 
            } else
              {
                /* 100997 LAH: Added char cast */
                *cp = (char) val;
                EncVal_Destroy( enc_val );
              }
            }
	    free (temp_string);
/*  get useless (for character data) data width */
            new_dw = 6;
            if( BitStream_Get( S4BS, &enc_val, new_dw ) )
            {
              BUFR_Err_Log( "D_Compress" );
              EncVal_Destroy( enc_val );
	      free (fxy_complete);
              return BUFR_ERROR; 
            }
            EncVal_Destroy( enc_val );
          /* end of character stuff */
        } else
          {
          /* getting min value  */
            if( BitStream_Get( S4BS, &enc_val, dw ) )
            {
              BUFR_Err_Log( "D_Compress" );
	      free (fxy_complete);
              return BUFR_ERROR; 
          } else 
          {
            /* since this value needs to be unscale and referenced  */
	  	 /* this next bit of code has to be used rather than */
		 /* EncVal_Get */
    	      if( HexStr_Get( enc_val.value, enc_val.nbits, &val, &m_flag) )
    	      {
                BUFR_Err_Log( "D_Compression" );
                EncVal_Destroy( enc_val );
		free (fxy_complete);
        	return 1; 
    	      }
            if ( m_flag == 0 )
                Missing_Value_Replace(&val, fxy);

            min_val_miss = m_flag;
	      min_val = val;
            }

	    /* LAH 042800 added code to test for missing value */
          /* LAH 112601 - removed because of new approach to flaging missing values */

            EncVal_Destroy( enc_val );
            s0 = scale;
            dw = 6;
            ref_val = 0;
            scale = 0;
          /* get new data width */
            if( BitStream_Get( S4BS, &enc_val, dw ) )
            {
              BUFR_Err_Log( "D_Compress" );
	      free (fxy_complete);
              return BUFR_ERROR; 
          } else if( EncVal_Get( &val, enc_val, scale, ref_val, &m_flag) )
            {
              BUFR_Err_Log( "D_Compression" );
              EncVal_Destroy( enc_val );
	      free (fxy_complete);
              return BUFR_ERROR; 
            }
	    new_dw = (int)val;

            EncVal_Destroy( enc_val );
          }
        for(j = 0; j < nsets; j++)
        {   
          if(cc_flag == 1)
          {

/*printf(" new char string %d %d %d \n", new_dw, dw, j);*/
            if((d_con[inum+(j*tableb_ct)].Val.string = malloc( (uint_t) (dw+1))) == NULL)
            {
              BUFR_Err_Set( "D_Compression", "Can't allocate string" );
	      free (fxy_complete);
              return BUFR_ERROR; 
            } else  /* 100997 LAH: Added uint_t cast */
              memset( d_con[inum+(j*tableb_ct)].Val.string, 0, (uint_t) (dw+1) );

              /* Get string from bit stream. */
            for( k=0, cp=d_con[inum+(j*tableb_ct)].Val.string; k < dw; k++, cp++ )
            {
              if( BitStream_Get( S4BS, &enc_val, BITS_IN_BYTE ) )
              {
                BUFR_Err_Log( "D_Compression" );
		free (fxy_complete);
                return BUFR_ERROR; 
             } else if( EncVal_Get( &val, enc_val, scale, rv0, &m_flag) )
              {
                BUFR_Err_Log( "D_Compression" );
		free (fxy_complete);
                return BUFR_ERROR; 
             } else
              {
                /* 100997 LAH: Added char cast */
                *cp = (char) val;
                EncVal_Destroy( enc_val );
              }
            }
            d_con[inum+(j*tableb_ct)].Val_Type   = DT_STRING;
            d_con[inum+(j*tableb_ct)].missing_flag = 1;
	    d_con[inum+(j*tableb_ct)].FXY_Val = fxy;
           /* end of character stuff */

          } else
	  {
            if (new_dw != 0)
	    {
	      if( BitStream_Get( S4BS, &enc_val, new_dw ) )
              {
                BUFR_Err_Log( "D_Compress" );
	        free (fxy_complete);
                return BUFR_ERROR; 
              } else 
              {
                /* since the scale has to be multiplied and not divided */
	  	/* this next bit of code has to be used rather than */
		/* EncVal_Get */
    	          if( HexStr_Get( enc_val.value, enc_val.nbits, &val, &m_flag) )
    	        {
        	    BUFR_Err_Log( "EncVal_Get" );
                    EncVal_Destroy( enc_val );
                    free(fxy_complete);
        	    return 1; 
    	        }
                EncVal_Destroy( enc_val );
                if ( m_flag == 0 )
                     Missing_Value_Replace(&val, fxy);

                if( m_flag != 0 )
		{
		  if(s0 != 0) {
        	    /* 100997 LAH: Added double cast */
        	    val = (val + (double) min_val + rv0) / TenPow( s0 );
	  	  } else {
		    val = (val + (double) min_val + rv0);
                  }
		} 
              }
            } else
            {
              m_flag = min_val_miss;
              if ( m_flag != 0 )
	      {
	        val = ((double) min_val + rv0)/TenPow(s0);
	      } else {
                val = (double) min_val;
	      }
	    }

	    d_con[inum+(j*tableb_ct)].FXY_Val = fxy;
            if(s0 == 0)
            {
              d_con[inum+(j*tableb_ct)].Val_Type   = DT_INT;
              d_con[inum+(j*tableb_ct)].Val_Scale   = 0;
              if( m_flag == 0 ){
                d_con[inum+(j*tableb_ct)].missing_flag = 0;
                d_con[inum+(j*tableb_ct)].Val.int_number = (int)val;
              }else{
                d_con[inum+(j*tableb_ct)].missing_flag = 1;
                d_con[inum+(j*tableb_ct)].Val.int_number = (int)(val + .5);
              }
            } else
            {
/*
	      if(new_dw == 0) val = min_val;
*/
              d_con[inum+(j*tableb_ct)].Val.number = val;
              d_con[inum+(j*tableb_ct)].Val_Type   = DT_DOUBLE;
              d_con[inum+(j*tableb_ct)].missing_flag = m_flag;
              d_con[inum+(j*tableb_ct)].Val_Scale = s0;
/*
              d_con[inum+(j*tableb_ct)].missing_flag = 1;
              if(val ==  BUFR_Msg.Missing_Value ){
                d_con[inum+(j*tableb_ct)].missing_flag = 0;
              }
*/
            }
          }
        }
      }
      inum++;
    }
    
/*    
    cnt = 0;
    new_array = (BUFR_Val_t*)malloc(nsets*tableb_ct * sizeof(BUFR_Val_t));
    for ( i = 0; i < nsets; i++)
    {
	for ( j = 0; j < tableb_ct; j++)
	{
	    new_array[cnt] = d_con[j*nsets + i + 1];
	    printf(" %d %d %f %f\n", i, j, d_con[j*nsets + i + 1].Val.number,
		d_con[cnt].Val.number);
	    cnt++;
	}
    }
    free (d_con);
*/
    free(fxy_complete);
    BUFR_Msg.decom_vals = d_con;
    BUFR_Msg.dc_numb = tableb_ct;
    BUFR_Msg.dc_parm_cnt = 0;
    return(0);  
}
