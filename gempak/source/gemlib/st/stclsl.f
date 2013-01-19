	SUBROUTINE ST_CLSL  ( string, sep, cdef, nexp, carr, num, iret )
C************************************************************************
C* ST_CLSL								*
C*									*
C* This subroutine breaks a string containing a list of strings into	*
C* an array of strings.  The separator for the strings is input as SEP.	*
C* If the separator is not a blank, any blanks in the string will be	*
C* preserved.  If null strings are encountered or fewer than NEXP	*
C* strings are found in the string, the appropriate CARR locations are	*
C* set to CDEF.	 This routine is intended to be used for long strings	*
C* that cannot be parsed by ST_CLST.                                    *
C*									*
C* ST_CLSL  ( STRING, SEP, CDEF, NEXP, CARR, NUM, IRET )		*
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
C* S. Jacobs/NCEP	 8/98	Copied from ST_CLST; removed strbuf	*
C* D. Kidwell/NCEP	 9/02	Corrected prologue                      *
C************************************************************************
	CHARACTER*(*) 	string, sep, carr (*), cdef
C*
	CHARACTER	cchar*1
C------------------------------------------------------------------------
	iret = 0
	num  = 0
C
C*	Initialize output array.
C
	DO  i = 1, nexp
	    carr (i) = cdef
	END DO
C
C*	Check for zero length input string.
C
	CALL ST_LSTR ( string, isize, ier )
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
	        loc = INDEX  ( string ( ibegin: ), cchar )
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
			carr ( num ) = string ( ibegin : iend-1 )
	    	    END IF
		END IF
	    ibegin = iend + 1
	    END DO
	END IF
C*
	RETURN
	END
