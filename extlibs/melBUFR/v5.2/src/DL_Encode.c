/*
 * DataList_Encode - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         DataList_Encode
 
  DESCRIPTION:         Put data list entries on Section 3 and 4 bit streams.
			Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int DataList_Encode()
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  There is no data	Write error message
                        Return with error
  The section 3 bit	Write error message
  stream is NULL        Return with error
  The section 4 bit	Write error message
  stream is NULL        Return with error
  Bad return from	Write error message
  BitStream_Put         Return with error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Encodes the error message
  BUFR_ProcMethod	Finds the BUFR procedure method
  EncVal_Set		Encodes a given value  
  BitStream_Put         Writes a value to the bit stream
  EncVal_Destroy	Destroys the encoded value
  EncVal_IsBad		Checks the validity of the encoded value
  DataList_NumFXYs	Returns the number of FXYs in a data list
  BUFR_Err_Log		Encodes an error message
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg      BUFR_Msg_t      External BUFR message structure
  BM		BUFR_Msg_t	Local copy of BUFR_Msg
  DL            DataEntry_t     Pointer to a data entry structure
  Dent          DataEntry_t     Pointer to a data entry structure
  S3BS		BitStream_t*	Pointer to section 3 bit stream
  S4BS		BitStream_t*	Pointer to section 4 bit stream
  enc_val	EncVal_t	Valid encoded value array
  ev		EncVal_t	Valid encoded value
  i		int		Loop counter
  Max_S3_FXYs	int		Maximum number of FXYs to be written
  S3_FXYs_written	int  	Number of FXYs written
  num_values	int		Number of encoded values
 
  METHOD:
	Initialize pointers
	If there is no data list then
	  Write error message
	  return with an error
	Endif
	If there is no Section 3 bit stream then
	  Write error message
	  return with an error
	Endif
	If there is no Section 4 bit stream then
	  Write error message
	  return with an error
	Endif

	If the Procedure Method is Template then
	  Set Max_S3_FXYs to the subset_size
	  Loop on Max_S3_FXYs
	    Perform EncVal_Set to encode a specific FXY
	    If there is an error while Performing BitStream_Put  then
		Perform BUFR_Err_Log to put an error message into memory
		Perform EncVal_Destroy to destroy memory for the encoded value
		Return with an error
	    Endif
	    Perform EncVal_Destroy to destroy memory for the encoded value
	    Increment the number of Section 3 FXYs written
	  End loop
	  Loop on the data list
	    Set the enc_val pointer to the data list value pointer
	    Loop on number of data list values
	      If there is an error while Performing EncVal_IsBad  then
		Continue to the bottom of the loop
	      Endif
	      If there is an error while Performing BitStream_Put  then
		Perform BUFR_Err_Log to put an error message into memory
		Return with an error
	      Endif
	    Endloop
	  Endloop
	  Return with no error
	Else
	  Set Max_S3_FXYs to the expanded number of FXYs
	EndIf

	Loop on the data list
	  If there are still Section 3 to write  then
	  Loop on  the number of fxys
            Perform EncVal_Set to encode a specific FXY
            If there is an error while Performing BitStream_Put  then
              Perform BUFR_Err_Log to put an error message into memory
              Perform EncVal_Destroy to destroy memory for the encoded value
              Return with an error
            Endif
            Perform EncVal_Destroy to destroy memory for the encoded value
            Increment the number of Section 3 FXYs written
	  End Loop

	  Set the enc_val pointer to the data list value pointer
	  Loop on number of data list values
	    If there is an error while Performing BitStream_Put  then
	       Perform BUFR_Err_Log to put an error message into memory
	       Return with an error
	     Endif
	  Endloop
	Endloop

	    
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
  Modified by Louis Hembree   10/08/97  Added int and uint_t casts 
  Modified by Louis Hembree   10/17/97  Removed redundent consistancy 
                                        and error checks
  Modified by Valerie Pastor  10/22/97  Updated prologue and deleted
					  unneeded printf statements
    INITIAL INSTALLATION:
 
 CHANGE LOG
 
  022498 LAH:  Added prints to bufr_log file.
  
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int DataList_Encode( void )

#else

int DataList_Encode()

#endif
{

    BUFR_Msg_t* BM;

    DataList_t*  DL;        /* Data list */
    DataEntry_t* Dent;

    BitStream_t* S3BS;      /* Section 3 bit stream (FXY values)     */
    BitStream_t* S4BS;      /* Section 4 bit stream (encoded values) */

    EncVal_t* enc_val;
    EncVal_t  ev;
    int    i;
    int    num_values;

    int Max_S3_FXYs, S3_FXYs_written;

    BM = &BUFR_Msg;

    DL = BM->data_list;

    S3BS = &BM->Section3_Data;
    S4BS = &BM->Section4_Data;

    if( DL == NULL )
    {
        BUFR_Err_Set( "DataList_Encode", "NULL DataList_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Encode", 
            "NULL DataList_t pointer" );
        return 1;
    }

    if( S3BS == NULL )
    {
        BUFR_Err_Set("DataList_Encode", "NULL Section 3 BitStream_t pointer");
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Encode", 
            "NULL Section 3 BitStream_t pointer" );
        return 1;
    }

    if( S4BS == NULL )
    {
        BUFR_Err_Set("DataList_Encode", "NULL Section 4 BitStream_t pointer");
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Encode", 
            "NULL Section 4 BitStream_t pointer" );
        return 1;
    }

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Encoding Data List\n" );
#endif

    if( BUFR_ProcMethod() == METHOD_TEMPLATE )
    {
        /* Write FXYs from the unexpanded list, not the data list. */

        Max_S3_FXYs     = BM->subset_size;
        S3_FXYs_written = 0;

        for( i=0; i < Max_S3_FXYs; i++ )
        {
            EncVal_Set( &ev, (double)BM->subset_fxys[i], 0,0,NUM_FXY_BITS );

            if( BitStream_Put( S3BS, ev ) )
            {
                BUFR_Err_Log( "DataList_Encode" );
                EncVal_Destroy( ev );
                return 1;
            }
            EncVal_Destroy( ev );   /* Hembree: 1/97 */
            S3_FXYs_written++;
        }

        /*
         * Any delayed replicators (1-XX-000), will not have a corresponding
         * encoded value.  Write every valid encoded value in the datalist
         * to Section 4.
         *
         * Table C descriptor 2-06-YYY will not have a corresponding encoded
         * value either.
         */

        for( Dent=DL->head->next; Dent != DL->tail; Dent=Dent->next )
        {
            enc_val = Dent->value;
            for( i=0; i < Dent->num_values; i++, enc_val++ )
            {
                if( EncVal_IsBad( *enc_val ) )
                    continue;

                /* Encode data value. */
                if( BitStream_Put( S4BS, *enc_val ) )
                {
                    BUFR_Err_Log( "DataList_Encode" );
                    return 1;
                }
            }
        }

        return 0;
    } else
    {
        Max_S3_FXYs = DataList_NumFXYs( BM->data_list );
        S3_FXYs_written = 0;
    }

    for( Dent=DL->head->next; Dent != DL->tail; Dent=Dent->next )
    {
      if( S3_FXYs_written < Max_S3_FXYs )
      {
        /* Write each FXY descriptor to Section 3. */
        for( i=0; i < Dent->num_fxys; i++ )
        {
          if( Dent->fxy_list[i] == (FXY_t)FXY_IGNORE )
                continue;

          EncVal_Set( &ev, (double)Dent->fxy_list[i], 0,0,NUM_FXY_BITS );

          if( BitStream_Put( S3BS, ev ) )
          {
            BUFR_Err_Log( "DataList_Encode" );
            EncVal_Destroy( ev );
            return 1;
          }
          EncVal_Destroy(ev);
          S3_FXYs_written++;
        }
      }

      num_values = Dent->num_values;
      enc_val = Dent->value;
	 
      /* Loop over values stored in data_list and enter into Section 4 */
      for (i=0; i< num_values; i++)
      {
        if ( BitStream_Put(S4BS,  *enc_val) )
        {
          BUFR_Err_Log(" DataList_Encode");
          return 1;
        }
        enc_val++;
      }
    }
    return 0;
}
