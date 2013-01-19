	SUBROUTINE GDINST ( string, cdef, mxstr, lrmbl, str, nstr, iret )
C************************************************************************
C* GDINST								*
C*									*
C* This subroutine takes a GDPLOT parameter input string (STRING) and 	*
C* separates it into an array of substrings (STR) using an exclamation 	*
C* point as the field separator.  Prior to returning, blanks/nulls in 	*
C* the STR array are either 1) replaced with CDEF if CDEF is not the 	*
C* '<' character, or 2) replaced with the previous value in the STR 	*
C* array if CDEF is '<'.  NSTR is the number of fields encountered.	*
C*									*
C* GDINST  ( STRING, CDEF, MXSTR, LRMBL, STR, NSTR, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	CDEF		CHAR*		Default string 			*
C*	MXSTR		INTEGER		Max number of substring		*
C*	LRMBL		LOGICAL		Logical remove blanks		*
C*									*
C* Output parameters:							*
C*	STR(*)		CHAR*		The returned string array	*
C*	NSTR		INTEGER		Number of substrings found	*
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	10/96						*
C************************************************************************
	CHARACTER*(*) 	string, str(*), cdef
	CHARACTER	sep*1, lesthn*1
	LOGICAL		lrmbl
C*
C------------------------------------------------------------------------
	iret = 0
	nstr  = 0
	nbscal = 0
	sep  = '!'
	lesthn = '<'
C
	DO  n = 1, mxstr
		str(n) = lesthn
	END DO
C
C*	Get string length
C
	CALL ST_LSTR ( string, isize, iret )
C
C*	Check for zero length input string.
C
	IF  ( isize .eq. 0 )  THEN
	    nstr = 1
	    str(nstr) = ' '
C
C*	    Check for separator and find list elements.
C
	  ELSE
	    iend   = 0
	    ibegin = 1
	    DO WHILE  ( ibegin .le. isize )
	        loc = INDEX  ( string ( ibegin: ), sep )
	        IF  ( loc .eq. 0 )  THEN
		    iend = isize
	          ELSE
		    iend = ibegin + loc - 2
	        END IF
		IF ( ibegin .le. iend ) THEN
		    nstr = nstr + 1
		    IF  ( nstr .le. mxstr ) 
     +			str(nstr) = string ( ibegin : iend )
		  ELSE IF ( ibegin .eq. iend + 1 ) THEN
		    nstr = nstr + 1
		    IF ( nstr .le. mxstr ) str(nstr) = cdef
		ENDIF
		IF ( str(nstr) .eq. ' ' )  str(nstr) = cdef
		ibegin = iend + 2
	    END DO
C
	    IF ( string ( isize : isize ) .eq. '!' ) THEN
		nstr = nstr + 1
		IF ( nstr .le. mxstr )  str(nstr) = cdef
	    END IF
	    IF ( str(1) .eq. lesthn )  str(1) = ' '
C
	END IF
C
C*	Remove blanks, check for carryover.
C
	IF ( lrmbl )  CALL ST_RMBL ( str(1), str(1), l, iret )
	DO  n = 2, mxstr
	    IF ( str(n) .eq. lesthn )  str(n) = str(n-1)
	    IF ( lrmbl )  CALL ST_RMBL ( str(n), str(n), l, iret )
	END DO
C
	RETURN
	END
