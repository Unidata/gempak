	SUBROUTINE ST_RPSL  ( string, substr, lensub, repstr, lenrep,
     +			      ipos, outstr, iret )
C************************************************************************
C* ST_RPSL								*
C*									*
C* This subroutine finds a substring within a string and returns 	*
C* the position of that substring and the output string with the 	*
C* substring replaced with repstr. If the substr is not found, the 	*
C* position, IPOS, is set to zero. If lensub or lenrep is zero, the	*
C* input string is returned in outstr, and a warning is sent. If the 	*
C* input string is blank, an error is returned. If the resulting string	*
C* length, after substitution, exceeds the length of outstr, the excess	*
C* is truncated.  Since the lengths of the substring and the replacement*
C* string must be provided, trailing blanks may be included in these    *
C* strings.                                                             *
C*									*
C* ST_RPSL  ( STRING, SUBSTR, LENSUB, REPSTR, LENREP, IPOS, OUTSTR,     *
C*            IRET )	                                        	*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C* 	SUBSTR		CHAR*		Substring 			*
C*	LENSUB		INTEGER		Length of substring             *
C* 	REPSTR		CHAR*		Replace substring 		*
C*	LENREP		INTEGER		Length of replacement string    *
C*									*
C* Output parameters:							*
C*	IPOS		INTEGER		Position of substring		*
C*	OUTSTR		CHAR*		Output string 			*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +5 = lensub cannot be zero 	*
C*					 +6 = lenrep cannot be zero	*
C*					 -1 = invalid input string	*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/00	From ST_RPST				*
C* F. J. Yen/NCEP	 4/01	Allowed for string length > 256.	*
C************************************************************************
	CHARACTER*(*)	string, substr, repstr, outstr
C*
	CHARACTER	sss*1250, ttt*1250
C------------------------------------------------------------------------
	iret = 0
	sss  = string
	ttt  = ' '
	ipos = 0
C
C*	Get lengths of strings.
C
C*	If the length of the input string is zero, return with an error.
C
	CALL ST_LSTR  ( sss, lenstr, ier )
	IF  ( lenstr .eq. 0 )  THEN
	    iret   = -1
	    outstr = ' '
	    RETURN
	END IF
C
C*	If the length of the substring or replacement string is zero,
C*	return with a warning and set the output string to the value
C*	of the input string.
C
	IF  ( lensub .le. 0 )  THEN
	    iret   = 5
	    outstr = sss
	    RETURN
	END IF
C
	IF  ( lenrep .le. 0 )  THEN
	    iret   = 6
	    outstr = sss
	    RETURN
	END IF
C
C*	Search for substring in string.
C
	ipos = INDEX ( sss, substr ( : lensub ) )
C
C*	Create new string if substring exists.
C
	IF  ( ipos .ne. 0 )  THEN
C
C*	    Find the end of the substring.
C
	    iend = ipos + lensub - 1
	    IF  ( ipos .gt. 1 )  THEN
C
C*	    	Take care of the case where the substring does not
C*		start at the first character.
C
		IF  ( iend .lt. lenstr )  THEN
C
C*		    First work with the case where the end of the
C*		    substring is before the end of the input string.
C
		    ttt = sss ( : ipos-1 ) // repstr ( : lenrep ) //
     +			  sss ( iend+1 : lenstr )
		  ELSE
C
C*		    Otherwise, the substring ends at the end of 
C*		    the input string, and there are no characters
C*		    to be added after the replacement string.
C
		    ttt = sss ( : ipos-1 ) // repstr ( : lenrep )
		END IF
	      ELSE
C
C*		Take care of the case where the substring starts
C*		at the first character.
C
		IF  ( iend .lt. lenstr )  THEN
C
C*		    First work with the case where the end of the
C*		    substring is before the end of the input string.
C
		    ttt = repstr ( : lenrep ) // sss ( iend+1 : lenstr )
		  ELSE
C
C*		    Otherwise, the entire input string is being
C*		    replaced.
C
		    ttt = repstr
		END IF
	    END IF
	  ELSE
C
C*	    If the substring does not exist, set the temporary output
C*	    string to the the temporary input string.
C
	    ttt = sss
	END IF
C
C*	Set the output string.
C
	outstr = ttt
C*
	RETURN
	END 
