	CHARACTER*(*) FUNCTION PT_DPRC  ( p24i )
C************************************************************************
C* PT_DPRC								*
C*									*
C* This function takes the daily precipitation amount in hundredths of  *
C* inches and converts it to a character string with an embedded        *
C* decimal point.  If the amount is zero, which denotes a trace, the    *
C* character 'T' is substituted for the amount.                         *
C*									*
C* PT_DPRC  ( P24I )							*
C*									*
C* Input parameters:							*
C*	P24I		REAL		24-hour precip amount 		*
C*									*
C* Output parameters:							*
C*	PT_DPRC		CHAR*		Precip amount with decimal, or T*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/99		                                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	cvalue*8
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PT_DPRC = ' '
	ndec    = 2
C
C*	Check for missing data.
C
	IF  ( ERMISS ( p24i ) ) THEN
            RETURN
	  ELSE IF ( p24i .gt. 0. ) THEN
C
C*	    Convert to character string.
C
	    CALL ST_RLCH ( p24i, ndec, cvalue, ier ) 
	    IF ( cvalue ( 1:1 ) .eq. '0' ) cvalue = cvalue ( 2: )
	    PT_DPRC =  cvalue
	  ELSE IF ( p24i .eq. 0. ) THEN
C
C*	    Character 'T' is used for trace precip.
C
	    PT_DPRC = 'T'
	END IF
C*
	RETURN
	END
