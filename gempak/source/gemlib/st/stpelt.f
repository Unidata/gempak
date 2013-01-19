	SUBROUTINE ST_PELT  ( string, ielt, cdef, nscal, celt, iret )
C************************************************************************
C* ST_PELT								*
C*									*
C* This subroutine returns the specified plot specification element of	*
C* a string containing a list of strings delimited by exclamation	*
C* points.  The exclamation points separate each of the plot		*
C* specifications for each of the overlays of the plot.	 If IELT is 	*
C* less than or equal to zero, the number of elements, less the number 	*
C* of trailing blank elements will be returned in NSCAL.		*
C*									*
C* ST_PELT  ( STRING, IELT, CDEF, NSCAL, CELT, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	IELT		INTEGER		The element to be returned	*
C*	CDEF		CHAR*		Default string 			*
C*									*
C* Output parameters:							*
C*	NSCAL		INTEGER		Total number of scalar elements	*
C*	CELT		CHAR*		The returned element		*
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/84	Original source for STLIST		*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	 2/85	Modified for ST_CLST			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* G. Krueger/EAI	 5/93	Modified from ST_CLST to ST_PELT	*
C* G. Krueger/EAI	10/93	Added ignore trailing blank commands	*
C* S. Jacobs/NCEP	 3/96	Fixed typo in documentation		*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	CHARACTER*(*) 	string, celt, cdef
C*
	CHARACTER	cchar*1
C------------------------------------------------------------------------
	iret = 0
	nscal  = 0
	nbscal = 0
	celt = ' '
C
C*	Get string length
C
	CALL ST_LSTR ( string, isize, iret )
C
C*	Check for zero length input string.
C
	IF  ( isize .eq. 0 )  THEN
	    nscal = 0
C
C*	    Check for separator and find list elements.
C
	  ELSE
	    cchar  = '!'
	    iend   = 0
	    ibegin = 1
	    DO WHILE  ( ibegin .le. isize )
	        loc = INDEX  ( string ( ibegin: ), cchar )
	        IF  ( loc .eq. 0 )  THEN
		    iend = isize
	        ELSE
		    iend = ibegin + loc - 2
	        END IF
		IF ( ibegin .le. iend ) THEN
		    nscal = nscal + 1
		    IF ( string ( ibegin : iend ) .ne. ' ' ) THEN
			nbscal = nscal
		    ENDIF
		    IF  ( nscal .le. ielt )  THEN
			celt = string ( ibegin : iend )
		    END IF
		ELSE IF ( ibegin .eq. iend + 1 ) THEN
		    nscal = nscal + 1
		    IF ( nscal .le. ielt ) celt = ' '
		ENDIF
		ibegin = iend + 2
	    END DO
	    IF ( string ( isize : isize ) .eq. '!' ) THEN
		nscal = nscal + 1
		IF ( nscal .le. ielt ) celt = ' '
	    ENDIF
	END IF
	IF ( nscal .eq. 0 ) THEN
	    celt = cdef
	ENDIF
	IF ( ielt .le. 0 ) THEN
	    nscal = nbscal
	ENDIF
C*
	RETURN
	END
