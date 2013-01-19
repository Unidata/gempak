	CHARACTER*(*) FUNCTION PT_TPFC  ( tpfr )
C************************************************************************
C* PT_TPFC								*
C*									*
C* This function decodes the real encoded value for min/max/POP into	*
C* the min/max/POP string						* 
C*									*
C* PT_TPFC  ( TPFR )							*
C*									*
C* Input parameters:							*
C*	TPFR		REAL		Coded value for the Min/Max/POP	*
C*									*
C* Output parameters:							*
C*	PT_TPFC		CHAR*		Min/Max/POP output string	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/99						*
C* S. Jacobs/NCEP	 8/02	Changed default from blank to "M/M/M"	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	cval*12, carr(3)*12
C*
	EQUIVALENCE	( ival, rval )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PT_TPFC = 'M/M/M'
C
C*	Check for missing data.
C
	IF  ( .not. ERMISS ( tpfr ) ) THEN
C
C*	    If the data value is not missing, parse the value into
C*	    the POP, Max and Min as in PPPXXXNNN.
C
	    rval = tpfr
C
	    imin = MOD ( ival, 1000 ) - 200
	    IF  ( imin .eq. -200 )  THEN
		carr(1) = 'M'
	      ELSE
		CALL ST_INCH ( imin, carr(1), ier )
	    END IF
C
	    imax = MOD ( (ival/1000), 1000 ) - 200
	    IF  ( imax .eq. -200 )  THEN
		carr(2) = 'M'
	      ELSE
		CALL ST_INCH ( imax, carr(2), ier )
	    END IF
C
	    ipop = ( ival / 1000000 ) - 200
	    IF  ( ipop .eq. -200 )  THEN
		carr(3) = 'M'
	      ELSE
		CALL ST_INCH ( ipop, carr(3), ier )
	    END IF
C
C*	    Create an output string of the form MIN/MAX/POP.
C
	    CALL ST_LSTC ( carr, 3, '/', cval, ier )
C
	    PT_TPFC = cval
	END IF
C*
	RETURN
	END
