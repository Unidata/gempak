/*
 * BUFR_Add_AF - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Add_AF - Add an associated field and significance value.
 * Return 1 on error, else 0.
 * 
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         BUFR_Add_AF
 
  DESCRIPTION:         Add an associated field and significance value.
			Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int BUFR_Add_AF( int BitWidth, int Significance )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  BitWidth		int	   Input	Width of the Associated Field
  SIgnificance		int	   Input	Significance value
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Bitwidth is too 	Write error message to buffer
  small or too large    Return with an error
  Datawidth is		Write error message to buffer
  undefined             Return with an error
  AF_List_Put error	Write error message to buffer
                        Return with an error 
  EncVal_Set error	Write error message to buffer
                        Return with an error 
  DataList_Put error	Write error message to buffer
                        Return with an error 
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  free             Frees memory
  FXY_Get_DataWidth	Gets the Data width of a given FXY
  FXY_Get_Scale		Gets the scale of a give FXY
  FXY_Get_RefVal	Gets the reference value of a give FXY
  AF_List_Put		Adds the associated field to the linked list of
			associated fields
  EncVal_Set		Adds given value to the data list
  DataList_Put		Adds the given value to the data list
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg	BUFR_Msg_t	External message structure
  errbuf	char		error message character string
  BM		BUFR_Msg_t	Local copy of BUFR_Msg
  AF_fxys	FXY_t		Array of associated fields FXYs
  enc_val	EncVal_t	Encoded value structure
  s		int		Scale of the associated field
  rv		int		Reference value of the associated field
  dw		int		Data width of the associated field
 
  METHOD:
	If the bit width is too small or too large then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	If there is an error getting the data width while performing
		FXY_Get_DataWidth then
          Perform BUFR_Err_Set to write error message
          Return with error
	Else
	  Perform FXY_Get_Scale
	  Perform FXY_Get_RefVal
	Endif
	If there is an error adding the AF to the linked list of associated 
		fields while performing AF_List_Put then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	Perform EncVal_Set to BUFR encode the given value
	If there is an error then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	Perform DataList_Put to place the FXYs and the encoded value in the
		data list
	If there is an error then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
	Perform free to deallocate memory
	Return with no error

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    022498 LAH:  Added prints to bufr_log file. 
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int BUFR_Add_AF( int BitWidth, int Significance )

#else

int BUFR_Add_AF( BitWidth, Significance )
int         BitWidth;       /* of the associated field */
int         Significance;

#endif
{
    
    char errbuf[80];

    BUFR_Msg_t* BM;

    FXY_t    AF_fxys[2];    /* 02-04-YYY and AF_SIG_FXY (0-31-021) */
    EncVal_t enc_val;
    int      s, rv, dw;

    BM = &BUFR_Msg;

    if( BitWidth < 1 || BitWidth > MAX_Y_VAL )
    {
        sprintf( errbuf, "AF bitwidth (%d) outside range of 1 to %d",
            BitWidth, MAX_Y_VAL );
        BUFR_Err_Set( "BUFR_Add_AF", errbuf );
        fprintf(BUFR_Cntl.bufr_log, "BUFR_Add_AF: %s \n",  errbuf);
        return 1;
    }

    if( (dw = FXY_Get_DataWidth( AF_SIG_FXY )) == 0 )
    {
        /* AF significance descriptor is undefined! */

        BUFR_Err_Log( "BUFR_Add_AF" );
	fprintf(BUFR_Cntl.bufr_log, 
	     "BUFR_Add_AF: AF significance descriptor is undefined!\n");
        return 1;
    }
    else
    {
        s  = FXY_Get_Scale( AF_SIG_FXY );
        rv = FXY_Get_RefVal( AF_SIG_FXY );
    }

    /* Add AF to linked list of associated fields. */

    if( AF_List_Put( BM->af_list, BitWidth, Significance ) )
    {
        BUFR_Err_Log( "BUFR_Add_AF" );
        return 1;
    }

    /* Add AF descriptors and AF significance to data list. */

    AF_fxys[0] = FXY_Pack( 2, 4, BitWidth );
    AF_fxys[1] = AF_SIG_FXY;

    if( EncVal_Set( &enc_val, (double)Significance, s, rv, dw ) )
    {
        BUFR_Err_Log( "BUFR_Add_AF" );
        return 1;
    }

    if( DataList_Put( BM->data_list, AF_fxys, 2, &enc_val, 1 ) )
    {
        BUFR_Err_Log( "BUFR_Add_AF" );
        return 1;
    }

    /* Deallocate memory for hex string in enc_val */

    free( (void*) enc_val.value );

    return 0;
}
