/* Compress a bufr file 
 * Compression - VERSION: %I%  %E% %T%
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         Compression
 
  DESCRIPTION:         Compression takes the binary bufr data that is produced
			by the user and compresses the data by the format
			given in the WMO No. 306
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, JULY 98
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, JULY 98
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	Compression()
 
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
  fxy_complete	FXY_t		Array of all the FXYs expanded
  ExpList	FXY_List_t	List returned from FXY_List_Expand
  fxy_ent	FXY_Entry_t	First entry of the ExpList
  S4BS		BitStream_t	Section 4 Bit Stream
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

int Compression(void)

#else

int Compression( )

#endif
{

    BUFR_Msg_t* BM;
    BUFR_Val_t* d_con;
    FXY_t fxy, *fxy_complete;
    FXY_t*       fxy_array;
    FXY_t*   fxy_pr;
    FXY_List_t*  ExpList = NULL;
    FXY_Entry_t* fxy_ent;
    BitStream_t*  S4BS; 
    int total_count = 0;
    int tableb_ct = 0;
    int tablec_ct = 0;
    int num_fxys;
    int nsets = 0;
    int i, j, k, n;
    int inum = 0;
    int dw, scale, rv0, s0;
    int tc_flag;  /* table c flag */
    int dwn;
    int cc_flag;
    EncVal_t      enc_val;
    double val;
    char* cp;
    char* temp_string;
    int dwc_f, scc_f, dw6_f;
    double dwc, scc, dw6;
    double xnum;
    double low_num, hi_num;
    int X_val, Y_val;
    char buf[80];
    int  s3_len;
    int m_flag;
    BM = &BUFR_Msg;
    S4BS = &BUFR_Msg.Section4_Data; 
    S4BS->bp = S4BS->buffer; 
    dwc_f = 0;
    scc_f = 0;
    dw6_f = 0;

    /* LAH 112000 -- Need to rewind the bit stream because the */
    /* internal pointer is pointing to the end after normal */
    /* processing.  Need to start at begining again to compress */
    BitStream_Rewind(S4BS);
/*  Go through the fxys and count how many total fxys and total data
    elements there will be. (The two are not necessarily the same.) */

    s3_len = BM->Section3_Data.bp - BM->Section3_Data.buffer;
    num_fxys = s3_len/2;

    fxy_array = (FXY_t*) malloc(sizeof(FXY_t)*num_fxys);

    fxy_pr = fxy_array;
    for( n=0; n < (s3_len-1); n+=2 )
    {
      fxy = (FXY_t) (BM->Section3_Data.buffer[n] << BITS_IN_BYTE)
            |  (ulong_t) BM->Section3_Data.buffer[n+1];
          total_count++;
      *fxy_pr = fxy;
       fxy_pr++;
    }

    ExpList = FXY_List_Expand( fxy_array, num_fxys);
    fxy_ent = ExpList->head->next;
    for ( n =0; n < ExpList->num_fxys; n++)
    {
      if ( FXY_F_Value(fxy_ent->fxy) == 1 && FXY_Y_Value(fxy_ent->fxy) == 0)
      {
         sprintf(buf, " %d %s = Delayed Replication \n",
               total_count, FXY_String(fxy_ent->fxy));
         BUFR_Err_Set( "Compression", buf );
         return -1;
      } else if ( FXY_F_Value(fxy_ent->fxy) == 1 && FXY_Y_Value(fxy_ent->fxy) != 0)
      {
        sprintf(buf, " %d %s = Replication should have been expanded already \n",
               total_count, FXY_String(fxy_ent->fxy));
         BUFR_Err_Set( "Compression", buf );
         return -1;
      } else if (  FXY_IsTableB( fxy_ent->fxy ) )
      {
        tableb_ct++;
      } else if( FXY_IsTableC( fxy_ent->fxy ) )
      {
        tablec_ct++;
      } else
      {
        sprintf(buf, " %d %s = Bad FXY value \n",
               total_count, FXY_String(fxy_ent->fxy));
        BUFR_Err_Set( "Compression", buf );
        return -1;
      }
      fxy_ent = fxy_ent->next;
    }


/*  We now know how many fxys and data elements there are.  Malloc the
    fxy array and the data array.  Then go through the fxys (again) to
    place them in the fxy array. */

    nsets = BM->subset_index;
    d_con =(BUFR_Val_t *) malloc(sizeof(BUFR_Val_t) * (nsets * (tableb_ct+1)));
    fxy_complete = (FXY_t *) malloc(sizeof(FXY_t) * (tableb_ct + tablec_ct));
    i = 0;
    total_count = ExpList->num_fxys;

    printf(" tablec_ct %d  tablec_ct %d total_count %d nsets %d\n",
           tableb_ct, tablec_ct, total_count, nsets);

    fxy_ent = FXY_List_First(ExpList);
    i = 0;
    for( ; fxy_ent != ExpList->tail; fxy_ent = fxy_ent->next)
    {
      fxy_complete[i] = fxy_ent->fxy;
/*
    printf(" i %d total count %d fxy %s %d\n", i, total_count, 
		FXY_String(fxy_complete[i]), fxy_complete[i]); 
*/
      i++;
    }

    inum = 0;
/* Go through list of fxys and copy to array so that current data sets can be 
 * deleated.  */

    for(j = 0; j < nsets; j++)
    {   
      for(i = 0; i <= (total_count-1); i++)
      {
        fxy = fxy_complete[i];
/* Ask about reference value */
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
                } else
                {
                    Y_val -= 128;
                    dwc = Y_val;
                    dwc_f = 1;
                }
                continue;

            case 2:     /* Change Scale */

                if( Y_val == 0 )
                {
                  scc_f = 0;
                }
                else
                {
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

                /* 100897 LAH: Added uint_t cast */

                if((temp_string=(char*)malloc( (uint_t) (Y_val+1)))==NULL)
                {
                    BUFR_Err_Set( "Compression", "Can't allocate string" );
                    fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                            "BUFR_Get_Value:", "Can't allocate string");
                    return BUFR_ERROR; 
                }

                d_con[j].Val_Type = DT_STRING;

                for( k=0, cp=temp_string; k < Y_val; k++, cp++ )
                {
                    if( BitStream_Get( S4BS, &enc_val, BITS_IN_BYTE ) )
                    {
                        BUFR_Err_Log( "Compression" );
                        return BUFR_ERROR; 
                    } else if( EncVal_Get( &val, enc_val, 0, 0, &m_flag) )
                    {
                        BUFR_Err_Log( "Compression" );
                       return BUFR_ERROR; 
                    } else
                    {
                        /* 100897 LAH: Added char cast */
                        *cp = (char) val;
                        EncVal_Destroy( enc_val );
                    }
                }
                *cp = (char) NULL;
            	d_con[inum+(j*tableb_ct)].missing_flag = 1;
                if((d_con[inum+(j*tableb_ct)].Val.string=
                     (char*)malloc( (uint_t) (Y_val+1)))==NULL)
                {
                  BUFR_Err_Set( "Compression", "Can't allocate string" );
                  fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
                         "BUFR_Get_Value:", "Can't allocate string");
                  return BUFR_ERROR; 
                 }
                 d_con[inum+(j*tableb_ct)].Val_Type   = DT_STRING;
                 strcpy(d_con[inum+(j*tableb_ct)].Val.string, temp_string);
                free(temp_string);
                continue;

            case 6:     /* Signify data width for next local descriptor */

                /*
                * Make sure that the next local descriptor is a Table B
                * descriptor (in the form 0-XX-YYY).  If it isn't,
                * complain and ignore this 2-06-YYY descriptor.
                */
                if( !FXY_IsTableB( fxy_complete[(i+1)] ) )
                {
                    fprintf( stderr, "compress(), WARNING: " );
                    fprintf( stderr,
                        "2-06-%03d followed by %s instead of\n",
                        Y_val, FXY_String( fxy_complete[i+1]));
                    fprintf( stderr, "a Table B descriptor.  Ignoring " );
                    fprintf( stderr, "the %s descriptor.\n",
                        FXY_String( fxy_complete[i] ) );
                    fprintf( BUFR_Cntl.bufr_log,
                        "compress(), WARNING: " );
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
/*  Do we read the next fxy and process it here?  Knowing only the data
    width?  */
                }
                continue;

            default:
                sprintf( buf, "Invalid Table C descriptor (%s)",
                    FXY_String( BUFR_Msg.exp_ptr->fxy ) );
                BUFR_Err_Set( "Compression", buf );
                return BUFR_ERROR; 

          }   /* switch( X_val ) */
        } /* end of Table C stuff */

        if(FXY_Get_Values(fxy, &scale, &rv0, &dw))
        {
          BUFR_Err_Log( "How did you get here?  Bad FXY in Compression" );
        }
        if(dwc_f == 1) dw += (int) dwc;
        if(dw6_f == 1) dw = (int) dw6;
        if(scc_f == 1) scale += (int) scc; 
        cc_flag = 0;

        if(FXY_UnitsType(fxy)  == CCITT_IA5 ) cc_flag = 1;
        /* if dealing with a string malloc space for it and set to zero */

        if(cc_flag == 1) 
        {
/*
          printf("char area 0 %d %d %d %d %d\n", j, dw, inum, 
		            (j*tableb_ct), tableb_ct);
*/
          dw = (dw / BITS_IN_BYTE);
          if((d_con[inum+(j*tableb_ct)].Val.string = malloc( (uint_t) (dw+1))) == NULL)
          {
            BUFR_Err_Set( "Compression", "Can't allocate string" );
            return BUFR_ERROR; 
          } else
            /* 100997 LAH: Added uint_t cast */
            memset( d_con[inum+(j*tableb_ct)].Val.string, 0, (uint_t) (dw+1) );

          /* Get string from bit stream. */
          for( k=0, cp=d_con[inum+(j*tableb_ct)].Val.string; k < dw; k++, cp++ )
          {
            if( BitStream_Get( S4BS, &enc_val, BITS_IN_BYTE ) )
            {
              BUFR_Err_Log( "Compression" );
              free( (void*) temp_string );
              return BUFR_ERROR; 
            } else if( EncVal_Get( &val, enc_val, scale, rv0, &m_flag) )
            {
              BUFR_Err_Log( "Compression" );
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
/* 	
          printf(" new string %s  %d  %d  %d  %d \n", d_con[inum+(j*tableb_ct)].Val.string,
                 j, tableb_ct, inum, (inum+(j*tableb_ct)));   */
        } else /* end of character stuff */
        {
          if( BitStream_Get( S4BS, &enc_val, dw ) )
          {
            BUFR_Err_Log( "Compression" );
            return BUFR_ERROR; 
          } else if( EncVal_Get( &val, enc_val, scale, rv0, &m_flag) )
          {
            BUFR_Err_Log( "Compression" );
          }
          d_con[inum+(j*tableb_ct)].FXY_Val = fxy;
          if(scale == 0)
          {
            d_con[inum+(j*tableb_ct)].Val_Type   = DT_INT;
            if(m_flag ==  0 )
            {
              d_con[inum+(j*tableb_ct)].missing_flag = 0;
              d_con[inum+(j*tableb_ct)].Val.int_number = (int)val;
            }else{
              d_con[inum+(j*tableb_ct)].missing_flag = 1;
              d_con[inum+(j*tableb_ct)].Val.int_number = (int)(val + .5);
            }
          } else
          {
            d_con[inum+(j*tableb_ct)].Val.number = val;
            d_con[inum+(j*tableb_ct)].Val_Type   = DT_DOUBLE;
            d_con[inum+(j*tableb_ct)].missing_flag = m_flag;
          }
        }
        inum++;
      } /* end of list of fxy's */
      inum = 0;
    } /* end of nsets */

    /* Gone through Section 4 data and put into an array.  Now destroy the
     *  Section 4 data are so it can be rewritten */

    BitStream_Destroy( &BM->Section4_Data );
    BitStream_Init( &BM->Section4_Data, 0 );
    S4BS = &BM->Section4_Data;
    S4BS->bp = S4BS->buffer; 
    dwc_f = 0;
    scc_f = 0;

    /*  Go through each data element array and find high and low value */
    /*  Or place character data into the section 4 bit stream, it needs
    no special help */

    inum = 0;
    for(i = 0; i <= (total_count-1); i++)
    {
      fxy = fxy_complete[i];
      if ( FXY_IsTableB(fxy))
      {
        if(FXY_Get_Values(fxy, &scale, &rv0, &dw))
        { 
          BUFR_Err_Log( "Bad FXY in Compress" );
        }
      }
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
              } else
              {
                  Y_val -= 128;
                  dwc = Y_val;
                  dwc_f = 1;
              }
              continue;

          case 2:     /* Change Scale */
              if( Y_val == 0 )
              {
                  scc_f = 0;
              } else
              {
                  Y_val -= 128;
                  scc = Y_val;
                  scc_f = 1;
              }
              continue;
        }   /* switch( X_val ) */
      }
/* 
      if( FXY_IsTableC( fxy ) && (scc_f == 1 || dwc_f == 1))  continue;
*/
      cc_flag = 0;
      tc_flag = 0;
      if( FXY_IsTableC( fxy ) && (X_val = FXY_X_Value( fxy )) == 5)
      {
        cc_flag = 1;
        dw = FXY_Y_Value( fxy );
        tc_flag = 1;
      }
      if(FXY_UnitsType(fxy)  == CCITT_IA5 )
         cc_flag = 1;
      if(cc_flag == 1)
      {
        if(tc_flag == 0)
            dw = (dw / BITS_IN_BYTE);

        if((temp_string = malloc( (uint_t) (dw+1))) == NULL)
        {
          BUFR_Err_Set( "Compression", "Can't allocate string" );
        } else
          memset( temp_string, 0, (uint_t) (dw+1) );

        /* For the character data place the minimum value 
        *  (unused) in section 4 */
        for(k = 0; k < dw; k++)
        {
          xnum = (double)temp_string[k];
          EncVal_Set( &enc_val, xnum, 0, 0, BITS_IN_BYTE );
             /* Encode data value. */

          if( BitStream_Put( S4BS, enc_val ) )
          {
            BUFR_Err_Log( "Compression" );
            return 1; 
          }
        }
        /* For the character data place the data width
        *   in octets in Section 4 */
        xnum = (double)dw;
        EncVal_Set( &enc_val, xnum, 0, 0, 6 );
        if( BitStream_Put( S4BS, enc_val ) )
        {
          BUFR_Err_Log( "Compression" );
          return 1; 
        }
      }
      low_num = 99999999999.;
      hi_num = -99999999999.;
      for(j = 0; j < nsets; j++)
      {   
         if(cc_flag == 1)
         {
           for(k = 0; k < dw; k++)
           {
             xnum = (double)d_con[inum+(j*tableb_ct)].Val.string[k];
             EncVal_Set( &enc_val, xnum, 0, 0, BITS_IN_BYTE );
                /* Encode data value. */

             if( BitStream_Put( S4BS, enc_val ) )
             {
               BUFR_Err_Log( "Compression" );
               return 1; 
             }
           }
         } /* End of character manipulation */
         xnum = (double)d_con[inum+(j*tableb_ct)].Val.number;
         if(d_con[inum+(j*tableb_ct)].Val_Type == DT_INT)
                xnum = (double)d_con[inum+(j*tableb_ct)].Val.int_number;

         if(xnum < low_num)
             low_num = xnum;

         if(xnum > hi_num)
             hi_num = xnum;

      } /* end of j loop */
      if(cc_flag == 1)
      {
        inum++;
        continue;
      }
      s0 = scale;
      if(scc_f == 1) s0 += (int) scc;
      if(dwc_f == 1) dw += (int) dwc;
      low_num = (int) (low_num * TenPow(s0)) - rv0;
      hi_num = (int) (hi_num * TenPow(s0)) - rv0 - low_num;
      dwn = BUFR_BitWidth((int)hi_num) + 1;
      xnum = (double) low_num;
      EncVal_Set( &enc_val, (double)xnum, 0, 0, dw );
      if( BitStream_Put( S4BS, enc_val ) )
      {
        BUFR_Err_Log( "Compression" );
             return 1; 
      }
      EncVal_Set( &enc_val, (double)dwn, 0, 0, 6 );
      if( BitStream_Put( S4BS, enc_val ) )
      {
        BUFR_Err_Log( "Compression" );
             return 1; 
      }

      for(j = 0; j < nsets; j++)
      {
        xnum = (double)d_con[inum+(j*tableb_ct)].Val.number;
        if(d_con[inum+(j*tableb_ct)].Val_Type == DT_INT)
              xnum = (double)d_con[inum+(j*tableb_ct)].Val.int_number;

        xnum = (double)((int) (xnum * TenPow(s0)) - rv0 - (int)low_num);
        EncVal_Set( &enc_val, (double)xnum, 0, 0, dwn );
        if( BitStream_Put( S4BS, enc_val ) )
        {
          BUFR_Err_Log( "Compression" );
               return 1; 
        }
      }
      inum++;
    }
    return(0);
}

