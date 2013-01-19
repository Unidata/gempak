	FUNCTION PR_STDZ  ( pres, hght )
C************************************************************************
C* PR_STDZ								*
C*									*
C* This function computes a standard height used on upper-air charts.	*
C* For data below 500 mb, the standard height is the last three digits	*
C* of the height.  For data at and above 500 mb, the height is the	*
C* last three digits of the height in decameters.			*
C*									*
C* REAL PR_STDZ  ( PRES, HGHT )						*
C*									*
C* Input parameters:							*
C*	PRES		REAL		Pressure in millibars		*
C*	HGHT		REAL		Height in meters		*
C*									*
C* Output parameters:							*
C*	PR_STDZ		REAL		Abbreviated height		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/90						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( pres ) .or. ERMISS ( hght ) )  THEN
	    PR_STDZ = RMISSD
	    RETURN
	END IF
C
C*	Check for pressure below 500 mb.
C
	IF  ( pres .gt. 500 )  THEN
	    ihhh = NINT ( hght )
	    PR_STDZ = MOD ( ihhh, 1000 )
	  ELSE
	    ihhh = NINT ( hght / 10. )
	    PR_STDZ = MOD ( ihhh, 1000 )
	END IF
C*
	RETURN
	END
