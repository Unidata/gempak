	CHARACTER*(*) FUNCTION PT_DIGR  ( accret )
C************************************************************************
C* PT_DIGR								*
C*									*
C* This function takes the ice accretion rate on a vessel in salt water *
C* in inches per three hours and converts it to a character string with *
C* an embedded decimal point with one digit to the right of the point.  *
C*									*
C* CHAR* PT_DIGR  ( ACCRET )							*
C*									*
C* Input parameters:							*
C*	ACCRET		REAL		Ice accretion rate (inches/3hrs)*
C*									*
C* Output parameters:							*
C*	PT_DIGR		CHAR*		Ice accretion rate with decimal *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/00		                                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	cvalue*8
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PT_DIGR = ' '
	ndec    = 1
C
C*	Check for missing data.
C
	IF  ( ERMISS ( accret ) ) THEN
            RETURN
	  ELSE IF ( accret .ge. 0.1 ) THEN
C
C*	    Convert to character string.
C
	    CALL ST_RLCH ( accret, ndec, cvalue, ier ) 
	    IF ( cvalue ( 1:1 ) .eq. '0' ) cvalue = cvalue ( 2: )
	    PT_DIGR =  cvalue
	END IF
C*
	RETURN
	END
