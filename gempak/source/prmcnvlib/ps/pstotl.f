	FUNCTION PS_TOTL  ( t850, td850, t500 )
C************************************************************************
C* PS_TOTL								*
C*									*
C* This function computes the total totals index:			*
C*									*
C*      TOTL = ( T850 - T500 ) + ( TD850 - T500 )			*
C*									*
C* REAL PS_TOTL  ( T850, TD850, T500 )                                	*
C*									*
C* Input parameters:							*
C*	T850		REAL		850 mb temperature in Celsius	*
C*	TD850		REAL		850 mb dewpoint in Celsius	*
C*	T500		REAL		500 mb temperature in Celsius	*
C*									*
C* Output parameters:							*
C*	PS_TOTL		REAL		Total totals index		*
C**									*
C* Log:									*
C* P. Kocin/GSFC	1980						*
C* M. Goodman/RDS	11/84	Cleaned code				*
C* M. desJardins/GSFC	 3/88	Cleaned code				*
C************************************************************************
        INCLUDE      'GEMPRM.PRM'
        INCLUDE      'ERMISS.FNC'
C------------------------------------------------------------------------------
	IF ( ERMISS (t850) .or. ERMISS (td850) .or. ERMISS (t500) ) THEN
	    PS_TOTL = RMISSD
	  ELSE
C
C*	    Compute vertical totals.
C
	    vtot = PS_VTOT ( t850,  t500 )
C
C*	    Compute cross totals.
C
	    ctot = PS_CTOT ( td850, t500 )
C
C*	    The sum is the total totals.
C
	    PS_TOTL = ctot + vtot
	END IF
C*
	RETURN
	END
