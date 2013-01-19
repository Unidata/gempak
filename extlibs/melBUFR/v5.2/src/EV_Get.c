/*
 * EncVal_Get - VERSION: %I%  %E% %T%
 */
/*
 * EncVal_Get - Decode the given BUFR-encoded value.
 * Return 1 on error, else 0.
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         EncVal_Get
 
  DESCRIPTION:         Decode the given BUFR-encoded value.
			Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int EncVal_Get( double* D, EncVal_t EV, int Scale, int RefVal )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  D		      double	      Out	Decoded value
  EV		      EncVal_t	      In	Encoded value
  Scale		      int	      In	Scale of value
  RefVal	      int	      In	Reference value
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Bad Encoded Value	Write error message to buffer
			Return with an error
  Bad HexStr_Get   	Write error message to buffer
			Return with an error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  EncVal_IsBad		Checks to see if the Encoded value is bad in some way
  HexStr_Get		Converts given hex string to a decimal value
  TenPow		Returns 10 raised to the given integer value
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  val		double		decimal value
 
  METHOD:
	If the encoded value is bad the
	  Perform BUFR_Err_Set to place an error message in a buffer
	  Return with an error condition
	Endif
	If there is an error return from Performing HexStr_Get  then
	  Perform BUFR_Err_Set to place an error message in a buffer
	  Return with an error condition
	Endif
	If the value equals the Missing value indicator then
	  The returned variable equals the value
	Else
	  The returned variable equals the value after being scaled 
		and referenced
	Endif
	  
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    100997 LAH: Added double cast
    022498 LAH:  Added prints to bufr_log file.
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int EncVal_Get( double* D, EncVal_t EV, int Scale, int RefVal, int* m_flag )

#else

int EncVal_Get( D, EV, Scale, RefVal, m_flag )
double*  D;
EncVal_t EV;
int      Scale, RefVal;
int* m_flag;
#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;

    double val;
 
    if( EncVal_IsBad( EV ) )
    {
        BUFR_Err_Set( "EncVal_Get", "Bad encoded value" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "EncVal_Get", 
	     "Bad encoded value" );
        return 1;
    }

    if( HexStr_Get( EV.value, EV.nbits, &val, m_flag ) )
    {
        BUFR_Err_Log( "EncVal_Get" );
        return 1;
    }

/*  LAH 112601 - modified to use missing value flag m_flag */
    if( m_flag == 0 )
        *D = val;
    else
        /* 100997 LAH: Added double cast */
        *D = (val + (double) RefVal) / TenPow( Scale );

    return 0;
}
