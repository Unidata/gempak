	FUNCTION PR_PSPD  ( drct, sped )
C************************************************************************
C* PR_PSPD								*
C*									*
C* This function computes PSPD from DRCT and SPED.  PSPD is in the	*
C* form DDFFF, where DD is the wind direction in tens of degrees,	*
C* and FFF is either the wind speed or wind speed plus 500, 		*
C* depending on the unit digit of direction rounded to the nearest	*
C* 5 degrees.  The following equation is used:				*
C*									*
C*          PSPD  =  JDRCT * 500 + JSPED				*
C*                JDRCT = NINT ( DRCT / 5 )				*
C*                JSPED = NINT ( SPED )					*
C*									*
C* REAL PR_PSPD  ( DRCT, SPED )						*
C*									*
C* Input parameters:							*
C*	DRCT		REAL		Wind direction in degrees	*
C*	SPED		REAL		Wind speed 			*
C*									*
C* Output parameters:							*
C*	PR_PSPD		REAL		Packed speed and direction	*
C**									*
C* Log:									*
C* D. Frey/CSC		 9/82	Original source code			*
C* I. Graffman/RDS	11/84	Corrected speed variable names		*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 7/88	Documentation,	correct rounding	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ( .not. ERMISS ( drct ) ) .and. 
     +	      ( .not. ERMISS ( sped ) ) )  THEN
	   jdrct   = NINT ( drct / 5. )
	   jsped   = NINT ( sped )
	   PR_PSPD = jdrct * 500 + jsped
	ELSE
	   PR_PSPD = RMISSD
	END IF
C*
	RETURN
	END
