	SUBROUTINE ST_CLS2  ( string, sep, cdef, nexp, carr, num, iret )
C************************************************************************
C* ST_CLS2								*
C*									*
C* This subroutine breaks a string containing a list of strings into	*
C* an array of strings.  The separator for the strings is input as SEP.	*
C* If the separator is a blank, multiple blanks will be changed to	*
C* single blanks before the string is processed.  If null strings	*
C* are encountered or fewer than NEXP strings are found in the		*
C* string, the appropriate CARR locations are set to CDEF.		*
C*									*
C* This routine is different from ST_CLST, since it does not remove all	*
C* blanks, if the separator is a character other than a blank.		*
C*									*
C* ST_CLS2  ( STRING, SEP, CDEF, NEXP, CARR, NUM, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	SEP		CHAR*1		Separator			*
C*	CDEF		CHAR*		Default string 			*
C*	NEXP		INTEGER 	Number of expected values 	*
C*									*
C* Output parameters:							*
C*	CARR  (NUM)	CHAR*		Array of strings 		*
C*	NUM		INTEGER 	Number of strings returned	*
C*	IRET		INTEGER 	Return code			*
C*				   	  1 = more than NEXP values	*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/96	Copied from ST_CLST			*
C* S. Jacobs/NCEP	 3/96	Removed call to ST_RMBL			*
C************************************************************************
	CHARACTER*(*) 	string, sep, carr (*), cdef
C*
	CHARACTER	strbuf*160, cchar*1
C------------------------------------------------------------------------
	iret = 0
	num  = 0
C
C*	Remove blanks from the input string if the separator is not
C*	a blank.
C
	IF  ( sep .ne. ' ' )  THEN
	    CALL ST_LSTR  ( string, isize, iret )
	    strbuf = string
	  ELSE
	    CALL ST_RXBL  ( string, strbuf, isize, iret )
	END IF
C
C*	Initialize output array.
C
	DO  i = 1, nexp
	    carr (i) = cdef
	END DO
C
C*	Check for zero length input string.
C
	IF  ( isize .eq. 0 )  THEN
	    num = 0
C
C*	    Check for separator and find list elements.
C
	  ELSE
	    cchar  = sep
	    iend   = 0
	    ibegin = 1
	    DO WHILE  ( ibegin .le. isize )
	        loc = INDEX  ( strbuf ( ibegin: ), cchar )
	        IF  ( loc .eq. 0 )  THEN
		    iend = isize + 1
	          ELSE
		    iend = ibegin + loc - 1
	        END IF
C
C*	        Add into output list.  Check that num <= nexp.
C
	        IF  ( num .ge. nexp )  THEN
		    iret = 1
		  ELSE
		    num = num + 1
	    	    IF  ( ibegin .ne. iend )  THEN
			carr ( num ) = strbuf ( ibegin : iend-1 )
	    	    END IF
		END IF
	    ibegin = iend + 1
	    END DO
	END IF
C*
	RETURN
	END
