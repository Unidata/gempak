	SUBROUTINE ST_UNPR  ( string, lenin, outstr, lenout, iret )
C************************************************************************
C* ST_UNPR								*
C*									*
C* This subroutine eliminates substrings of unprintable characters.	*
C* Substrings of control characters, i.e., characters less than a 	*
C* blank, are replaced by a single blank.  Characters greater than	*
C* '}' (CHAR (126)) are replaced by '~' (CHAR (127)).  This subroutine	*
C* can be used to replace control characters such as CR and LF with	*
C* a single blank.  Invalid characters in the ASCII character set	*
C* are replaced by '~' so that the lengths of fields in the record	*
C* will remain unchanged.  The input and output strings may be the	*
C* same variable.							*
C*									*
C* ST_UNPR  ( STRING, LENIN, OUTSTR, LENOUT, IRET )			*
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
C* M. desJardins/GSFC	12/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Whistler/SSAI	 5/91	Put tilda definition in GEMPRM.PRM	*
C* D. Kidwell/NCEP	 6/04	Added lenomx for bug if ip > LEN(outstr)*
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
C*	a blank or out of range.
C
	DO  i = 1, lenin
	    IF ( ip .lt. lenomx ) THEN
	        c = string (i:i)
	        IF  ( ( c .ge. '!' ) .and. ( c .le. '}' ) )  THEN
C
C*		    Add non blanks to the string.
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
C*		    Add a blank to the list only if the last character
C*		    is not already a blank.
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
