	CHARACTER*(*) FUNCTION PT_SWEL  ( rval )
C************************************************************************
C* PT_SWEL								*
C*									*
C* This function takes a real number and converts the integral part	*
C* into a 6-character string.  Leading blanks are changed to 0.  It	*
C* can be used to output combined swell wave direction, period and      *
C* height.                                                              *
C*									*
C* PT_SWEL  ( RVAL )							*
C*									*
C* Input parameters:							*
C*	RVAL		REAL		Value				*
C*									*
C* Output parameters:							*
C*	PT_SWEL		CHAR*		Six-character wave field 	*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/99		From PT_SALT			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	cvalue*8
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PT_SWEL = ' '
C
C*	Check for missing data.
C
	IF  ( ERMISS ( rval ) )  RETURN
C
C*	Convert to character string, get length.
C
	number = NINT ( rval )
	CALL ST_INLN  ( number, cvalue, lenc, ier )
	IF  ( ier .ne. 0 )  RETURN
C
C*	Pad with zeroes, if necessary
C
	IF  ( lenc .eq. 6 )  THEN
	    PT_SWEL = cvalue
	  ELSE IF  ( lenc .eq. 5 )  THEN
	    PT_SWEL = '0' // cvalue
	  ELSE IF  ( lenc .eq. 4 )  THEN
	    PT_SWEL = '00' // cvalue
	  ELSE IF  ( lenc .eq. 3 )  THEN
	    PT_SWEL = '000' // cvalue
	  ELSE IF  ( lenc .eq. 2 )  THEN
	    PT_SWEL = '0000' // cvalue
	  ELSE IF  ( lenc .eq. 1 )  THEN
	    PT_SWEL = '00000' // cvalue
	END IF
C*
	RETURN
	END
