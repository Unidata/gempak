	FUNCTION PR_TPFR  ( tmin, tmax, pp24 )
C************************************************************************
C* PR_TPFR								*
C*									*
C* This function computes TPFR.						*
C*									*
C* REAL PR_TPFR  ( TMIN, TMAX, PP24 )                                   *
C*									*
C* Input parameters:							*
C*	TMIN		REAL		Minimum temperature		*
C*	TMAX		REAL		Maximum temperature		*
C*	PP24 		REAL		Probability of precipitation	*
C*									*
C* Output parameters:							*
C*	PR_TPFR		REAL		Coded value for Min/Max/POP	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/99						*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
C*
	EQUIVALENCE	( ival, rval )
C*
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	ival = 0
C
C*	Construct the coded integer. The value is 9 digits as in
C*	PPPXXXNNN. Where PPP is the POP, XXX is the Max temp and
C*	NNN is the Min temp. All values are adjusted by 200 to 
C*	account for negative and missing values.
C
	IF  ( .not. ERMISS ( tmin ) )  THEN
	    ival = ival + ( NINT(tmin) + 200 )
	END IF
C
	IF  ( .not. ERMISS ( tmax ) )  THEN
	    ival = ival + ( NINT(tmax) + 200 ) * 1000
	END IF
C
	IF  ( .not. ERMISS ( pp24 ) )  THEN
	    ival = ival + ( NINT(pp24) + 200 ) * 1000000
	END IF
C
C*	If all three values are missing, then the answer is missing.
C*	Otherwise, set the output to the REAL number equivalent of 
C*	the coded integer.
C
	IF  ( ival .eq. 0 )  THEN
	    PR_TPFR = RMISSD
	  ELSE
	    PR_TPFR = rval
	END IF
C*
	RETURN
	END
