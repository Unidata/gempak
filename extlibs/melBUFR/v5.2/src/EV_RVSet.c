/*
 * EncVal_RVSet - VERSION: %I%  %E% %T%
 */
/*
 * EncVal_Set - BUFR-encode the given value.  Return 1 on error, else 0.
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         EncVal_RVSet
 
  DESCRIPTION:         BUFR-encode the given value.  Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int EncVal_RVSet(EncVal_t* EV, double Val, int DataWidth)
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  EV		     EncVal_t	    In		Encoded Value
  Val		     Double	    In		Hexidecimal value
  DataWidth	     Int	    In		Width of data in bits
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Bad Encoded Value     Write error message to buffer
                        Return with an error
  Data Width < 1        Write error message to buffer
                        Return with an error
  Val = Missing Value   Write error message to buffer
                        Return with an error
  Bad HexStr_RVSet      Write error message to buffer
                        Return with an error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Places error message into an error buffer
  HexStr_RVSet		Converts a given Reverence Value into a hexadecimal str
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg      BUFR_Msg_t      External BUFR message structure
  BM            BUFR_Msg_t      BUFR message structure
  enc_val       double          decimal value
  hs		HexStr_t	String of hexidecimal digits
 
  METHOD:
        If the encoded value is bad then
          Perform BUFR_Err_Set to place an error message in a buffer
          Return with an error condition
        Endif
        If the Data width is less than one then
          Perform BUFR_Err_Set to place an error message in a buffer
          Return with an error condition
        Endif
        If the value equals the Missing value indicator then
          Perform BUFR_Err_Set to place an error message in a buffer
          Return with an error condition
        Else
          The returned variable equals the value
        Endif
        If there is an error return from Performing HexStr_RVSet  then
          Perform BUFR_Err_Set to place an error message in a buffer
          Return with an error condition
        Endif
	Set the encoded value structure to the hex string and the data width

 
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

int EncVal_RVSet(EncVal_t* EV, double Val, int DataWidth)

#else

int EncVal_RVSet( EV, Val, DataWidth )
EncVal_t* EV;
double    Val;
int       DataWidth;

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    double   enc_val;
    HexStr_t hs;
    BUFR_Msg_t* BM;
 
    BM = &BUFR_Msg;

    if( EV == NULL )
    {
        BUFR_Err_Set( "EncVal_RVSet", "NULL EncVal_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "EncVal_RVSet", 
	     "NULL EncVal_t pointer" );
        return 1;
    }

    if( DataWidth < 1 )
    {
        BUFR_Err_Set( "EncVal_RVSet", "Data width < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "EncVal_RVSet", 
	     "data width < 1" );
        return 1;
    }

    if( Val == BM->Missing_Value )
    {
        BUFR_Err_Set( "EncVal_RVSet", "Val = MISSING_VALUE" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "EncVal_RVSet", 
	     "Val = MISSING_VALUE" );
        return 1;
    } else
        enc_val = Val;

    if( (hs = HexStr_RVSet( enc_val, DataWidth )) == NULL )
    {
        BUFR_Err_Log( "EncVal_RVSet" );
        return 1;
    }

    EV->value = hs;
    EV->nbits = DataWidth;

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 3 )
    {
        fprintf(BUFR_Cntl.bufr_log, "EncVal_RVSet: " );
        fprintf(BUFR_Cntl.bufr_log, "%f [%dDW] encodes as ", Val, DataWidth );
        EncVal_Print( *EV, BUFR_Cntl.bufr_log );
    }
#endif

    return 0;
}
