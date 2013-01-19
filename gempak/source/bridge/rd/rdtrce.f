	SUBROUTINE RD_TRCE ( string, len, rval, iret )
C************************************************************************
C* RD_TRCE								*
C*									*
C* This subroutine checks a string for trace ("T" or " T") and		*
C* substitutes a value of .005 for it in rval.				*
C*									*
C* RD_TRCE ( STRING, IST, IEND, RVAL, IRET )				*
C*									*
C* Input parameters:							*
C*      STRING		CHAR*		String				*
C*	LEN		INTEGER		Length of string		*
C*									*
C* Output parameters:							*
C*      RVAL		REAL		Value of string			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = Invalid string length	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	11/02						*
C************************************************************************
	CHARACTER*(*)	string
C------------------------------------------------------------------------
	iret = 0
	IF ( len .eq. 2 ) THEN
	    ndx = INDEX ( string ( 1:len ), ' T' )
	    IF ( ndx .ne. 0 ) THEN
		rval = .005
	    END IF
	  ELSE IF ( len .eq. 1 ) THEN
	    IF ( string ( 1:1 ) .eq. 'T' ) THEN
		rval = .005
	    END IF
	  ELSE
	    iret = -1
	END IF
C*
	RETURN
	END
