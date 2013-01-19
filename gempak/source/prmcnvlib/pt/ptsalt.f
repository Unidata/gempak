	CHARACTER*(*) FUNCTION PT_SALT  ( rval )
C************************************************************************
C* PT_SALT								*
C*									*
C* This function takes a real number and converts the integral part	*
C* into a 3-character string.  Leading blanks are changed to 0.  It	*
C* can be used to output abbreviated pressure and altimeter values.	*
C*									*
C* PT_SALT  ( RVAL )							*
C*									*
C* Input parameters:							*
C*	RVAL		REAL		Value				*
C*									*
C* Output parameters:							*
C*	PT_SALT		CHAR*		Three-character pressure code 	*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 8/90						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	cvalue*8
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PT_SALT = ' '
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
	IF  ( lenc .eq. 3 )  THEN
	    PT_SALT = cvalue
	  ELSE IF  ( lenc .eq. 2 )  THEN
	    PT_SALT = '0' // cvalue
	  ELSE IF  ( lenc .eq. 1 )  THEN
	    PT_SALT = '00' // cvalue
	END IF
C*
	RETURN
	END
