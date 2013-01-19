	SUBROUTINE ST_RNUL  ( string, outstr, lens, iret )
C************************************************************************
C* ST_RNUL								*
C*									*
C* This subroutine finds the NULL character in a string, if it exists, 	*
C* and then replaces it and the rest of the output array with blanks. 	*
C* The input and output strings may be the same variable.  The output	*
C* length of the string doesn't include trailing blanks, tabs, or nulls.*
C*									*
C* ST_RNUL  ( STRING, OUTSTR, LENS, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String without NULLs 		*
C*	LENS		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 3/98	From ST_NULL				*
C* T. Piper/SAIC	05/03	Modified to fill string with blanks	*
C************************************************************************
	INCLUDE	'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, outstr
	LOGICAL		found
C-----------------------------------------------------------------------
	lens = 0
	iret = 0
	lenout = LEN(outstr)
	IF  ( lenout .le. 0 )  RETURN
C
C*  Get the actual length of the input and output strings.
C
	lenin = LEN ( string )
	IF ( lenout .lt. lenin )  THEN
	    iret = 2
	END IF
	outstr = string
C
C*  Check each character for NULL.
C
	found = .false.
	ii = 1
	DO WHILE  ( ( .not. found ) .and. ( ii .le. lenout ) )
	    IF  ( outstr(ii:ii) .eq. CHNULL )  THEN
	    	found = .true.
		ipos  = ii
	    END IF
	    ii = ii + 1
	END DO
C
	IF  ( found )  THEN
	    outstr(ipos:lenout) = ' '
	END IF
	CALL ST_LSTR ( outstr, lens, ier )
C*
	RETURN
	END
