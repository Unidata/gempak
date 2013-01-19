	SUBROUTINE ST_LCUC  ( string, outstr, iret )
C************************************************************************
C* ST_LCUC   								*
C*									*
C* This subroutine converts lower-case characters in a string to 	*
C* upper case.  The input and output string may be the same variable.	*
C*									*
C* ST_LCUC  ( STRING, OUTSTR, IRET )					*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String in upper case		*
C*	IRET 		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C* K. Brill/NMC		05/93	Replace CALL ST_LSTR with LEN		*
C* S. Jacobs/NMC	 7/94	Added initialization of iret		*
C************************************************************************
	CHARACTER*(*)	string, outstr
C*---------------------------------------------------------------------
	iret = 0
C
C*	Move input string into output string and get length.
C
	outstr = string
	isize = LEN ( outstr )
C
C*	Loop through all characters.
C
	DO  i = 1, isize
C
C*	    Check to see if this is a lower case letter.
C
	    IF  ( ( outstr (i:i) .ge. 'a' ) .and. 
     +		  ( outstr (i:i) .le. 'z' ) )  THEN
C
C*		Get value of character, subtract 32 to convert to
C*		upper case and reinsert in string.
C
		j = ICHAR  ( outstr (i:i) )
		j = j - 32
		outstr (i:i) = CHAR (j)
	    ENDIF
	END DO
C*
      RETURN
      END
