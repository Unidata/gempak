	SUBROUTINE ST_UNP1  ( string, lenin, outstr, lenout, iret )
C************************************************************************
C* ST_UNP1								*
C*									*
C* This subroutine eliminates substrings of unprintable characters.	*
C* It differs from ST_UNPR in that it will not eliminate the line feed  *
C* (LF) control character, and it will not compress adjacent blanks.    *
C* Substrings of control characters, i.e., characters less than a blank,*
C* are replaced by a single blank.  Characters greater than '}'         *
C* (CHAR (126)) are replaced by '~' (CHAR (127)).  This subroutine can  *
C* be used to replace control characters such as CR and RS with a       *
C* single blank.  Invalid characters in the ASCII character set are     *
C* replaced by '~' so that the lengths of fields in the record will     *
C* remain unchanged.  The input and output strings may be the same      *
C* variable.                                                            *
C*									*
C* ST_UNP1  ( STRING, LENIN, OUTSTR, LENOUT, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Input string			*
C*	LENIN		INTEGER		Length of input string		*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		Output string			*
C*	LENOUT		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP      11/98   From ST_UNPR                            *
C* D. Kidwell/NCEP       6/04   Added lenomx for bug if ip > LEN(outstr)*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	string, outstr
C*
	LOGICAL		bl
	CHARACTER	c*1
C------------------------------------------------------------------------
	iret   = 0
	bl     = .true.
	ip     = 0
	lenomx = LEN ( outstr )
C
C*	Check each character to see if it is a control character,
C*	(other than LF) or out of range.
C
	DO  i = 1, lenin
	    IF ( ip .lt. lenomx ) THEN
	        c = string (i:i)
	        IF  ( ( ( c .ge. ' ' ) .and. ( c .le. '}' ) ) .or.
     +                ( c .eq. CHLF ) )  THEN
C
C*		    Add character to the string.
C
		    ip = ip + 1
		    outstr ( ip: ip ) = c
		    bl = .false.
	          ELSE IF  ( c .gt. '}' )  THEN
C
C*		    Replace out of range characters with a tilda.
C
		    ip = ip + 1
		    outstr ( ip : ip ) = CHTLDA 
		    bl = .false.
C
C*		    Replace control character with a blank only if the 
C*		    character immediately preceding it was not itself
C*		    replaced by a blank.
C
	          ELSE IF ( .not. bl ) THEN
		    bl = .true.
		    ip = ip + 1
		    outstr ( ip : ip ) = ' '
	        END IF
	    END IF
	END DO
C
C*	Blank out remainder of string.
C
	IF ( ip .lt. lenomx ) outstr ( ip+1 : lenomx ) = ' '
C
C*	Set output length.  If last character was a blank, eliminate it.
C
	IF  ( ( ip .gt. 0 ) .and. ( outstr (ip:ip) .eq. ' ' ) ) THEN
	    lenout = ip - 1
	  ELSE
	    lenout = ip
	END IF
C*
	RETURN
	END
