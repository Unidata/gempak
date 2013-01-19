/*
 * FXY_Pack_Dec - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Pack_Dec - Return an FXY_t value from a decimal FXY value.
 * For example, given a decimal value of 301001 (read 3-01-001),
 * extract the F, X, and Y values return a properly packed value.
 * Return BAD_FXY_VAL on error
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_Pack_Dec
 
  DESCRIPTION:         Return an FXY_t value from a decimal FXY value.
			For example, given a decimal value of 301001 
			(read 3-01-001), extract the F, X, and Y values 
			return a properly packed value.  Return BAD_FXY_VAL 
			on error
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	FXY_t FXY_Pack_Dec( int DecimalFXY )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  DecimalFXY	     int	   in		FXY in decimal form
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  DecimalFXY < 0	Write error message to buffer
			Return with error code
  F > MAX_F_VAL 	Write error message to buffer
			Return with error code
  X > MAX_X_VAL 	Write error message to buffer
			Return with error code
  Y > MAX_Y_VAL 	Write error message to buffer
			Return with error code
  result equals 	Write error message to buffer
  BAD_FXY_VAL		Return with error code
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Write error message to a buffer
  FXY_Pack		Packs FXY
  BUFR_Err_Log		Write error message to log file
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  d		double		local copy of DecimalFXY
  F		int		F component of the DecimalFXY
  X		int		X component of the DecimalFXY
  Y		int		Y component of the DecimalFXY
  val		FXY_t		Packed FXY
 
  METHOD:
	If the input Decimal FXY is less than zero then
	  Perform BUFR_Err_Set to write error message to a buffer
	  Return with error
	Endif
	If after extracting the F component it is found to be greater than
		MAX_F_VAL  then
	  Perform BUFR_Err_Set to write error message to a buffer
	  Return with error
	Else
	  Subtract the F component value from the decimal value
	Endif
	If after extracting the X component it is found to be greater than
		MAX_X_VAL  then
	  Perform BUFR_Err_Set to write error message to a buffer
	  Return with error
	Else
	  Subtract the X component value from the decimal value
	Endif
	If after extracting the Y component it is found to be greater than
		MAX_Y_VAL  then
	  Perform BUFR_Err_Set to write error message to a buffer
	  Return with error
	Endif
	If the value received from Performing FXY_Pack = BAD_FXY_VAL then
	  Perform BUFR_Err_Log to write error message to a log file
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

FXY_t FXY_Pack_Dec( int DecimalFXY )

#else

FXY_t FXY_Pack_Dec( DecimalFXY )
int DecimalFXY;

#endif
{
    double d;
    int    F, X, Y;
    FXY_t  val;

    if( DecimalFXY < 0 )
    {
        BUFR_Err_Set( "FXY_Pack_Dec", "Invalid Decimal FXY value" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Pack_Dec", 
	     "Invalid Decimal FXY value" );
        return (FXY_t) BAD_FXY_VAL;
    }

#define F_DIVISOR 100000.0
#define X_DIVISOR   1000.0

    d = (double) DecimalFXY;

    if( (F = (int) (d / F_DIVISOR)) > MAX_F_VAL )
    {
        BUFR_Err_Set( "FXY_Pack_Dec", "Invalid F value" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Pack_Dec", 
            "Invalid F value" );
        return (FXY_t) BAD_FXY_VAL;
    }
    else
        /* 100997 LAH: Added double cast of F */
        d -= (double) ( (double) F * F_DIVISOR);

    if( (X = (int) (d / X_DIVISOR)) > MAX_X_VAL )
    {
        BUFR_Err_Set( "FXY_Pack_Dec", "Invalid X value" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Pack_Dec", 
            "Invalid X value" );
         return (FXY_t) BAD_FXY_VAL;
    }
    else
        /* 100997 LAH: Added double cast of X */
        d -= (double) ( (double) X * X_DIVISOR);

    if( (Y = (int) d) > MAX_Y_VAL )
    {
        BUFR_Err_Set( "FXY_Pack_Dec", "Invalid Y value" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Pack_Dec", 
            "Invalid Y value" );
        return (FXY_t) BAD_FXY_VAL;
    }

    if( (val = FXY_Pack( F, X, Y )) == (FXY_t)BAD_FXY_VAL )
        BUFR_Err_Log( "FXY_Pack_Dec" );

    return val;
}
