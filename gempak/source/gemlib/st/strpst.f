	SUBROUTINE ST_RPST  ( string, substr, repstr,
     +			      ipos, outstr, iret )
C************************************************************************
C* ST_RPST								*
C*									*
C* This subroutine finds a substring within a string and returns 	*
C* the position of that substring and the output string with the 	*
C* substring replaced with repstr. If the substr is not found, the 	*
C* position, IPOS, is set to zero. If substr or repstr is blank, the	*
C* input string is returned in outstr, and a warning is sent. If the 	*
C* input string is blank, an error is returned. If the resulting string	*
C* length, after substitution, exceeds the length of outstr, the excess	*
C* is truncated.  ST_LSTR is used to get the lengths of the substring   *
C* and the replacement string.                                          *
C*									*
C* ST_RPST  ( STRING, SUBSTR, REPSTR, IPOS, OUTSTR, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C* 	SUBSTR		CHAR*		Substring 			*
C* 	REPSTR		CHAR*		Replace substring 		*
C*									*
C* Output parameters:							*
C*	IPOS		INTEGER		Position of substring		*
C*	OUTSTR		CHAR*		Output string 			*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +3 = substr cannot be blank	*
C*					 +4 = repstr cannot be blank	*
C*					 -1 = invalid input string	*
C*									*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 1/97	From ST_RMST				*
C* S. Jacobs/NCEP	 4/97	Changed setting of outstr; use temp vars*
C* D. Kidwell/NCEP	 3/00	Changed to call ST_RPSL                 *
C************************************************************************
	CHARACTER*(*)	string, substr, repstr, outstr
C------------------------------------------------------------------------
	iret = 0
C
C*	Get lengths of substring and replacement string.
C
	CALL ST_LSTR  ( repstr, lenrep, ier )
	IF  ( lenrep .eq. 0 )  iret = 4
C
	CALL ST_LSTR  ( substr, lensub, ier )
	IF  ( lensub .eq. 0 )  iret = 3
C
	IF ( iret .eq. 0 ) THEN
C
C*	    Do the string replacement.
C
	    CALL ST_RPSL ( string, substr, lensub, repstr, lenrep,
     +		           ipos, outstr, iret )
	  ELSE
C
C*	    Substring or replacement string cannot be blank.
C
	    ipos   = 0
	    outstr = string
	END IF
C*
	RETURN
	END 
