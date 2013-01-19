	SUBROUTINE RU_GFLD  ( string, lens, ipoint, field, lenf, iret )
C************************************************************************
C* RU_GFLD								*
C*									*
C* This subroutine returns the first field from a string starting	*
C* at IPOINT.  The fields in the string must be separated by blanks.	*
C* Blanks at the beginning of the string are skipped.  The pointer 	*
C* is updated to point to the first non-blank character after the	*
C* separating blank.  If the next field is 10 characters in length,	*
C* the first 5 characters are returned, leaving the next 5 for the 	*
C* next pass.								*
C*									*
C* RU_GFLD  ( STRING, LENS, IPOINT, FIELD, LENF, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*lens	String				*
C*	LENS		INTEGER		Length of string		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer in report		*
C*									*
C* Output parameters:							*
C*	FIELD		CHAR*		Next substring			*
C*	LENF		INTEGER		Length of substring		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no more substrings	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	CHARACTER*(*)	string, field
C*
	LOGICAL		start
C------------------------------------------------------------------------
	iret  = 0
	field = ' '
	lenf  = 0
C
C*	Check for the start of the string.
C
	start = .false.
	DO WHILE  ( .not. start )
	    IF  ( ipoint .gt. lens )  THEN
		iret = -1
		RETURN
	      ELSE IF  ( string ( ipoint: ipoint ) .ne. ' ' )  THEN
		start = .true.
	      ELSE
		ipoint = ipoint + 1
	    END IF
	END DO
C
C*	Search for a blank to terminate this field.  If no blank is
C*	found, end of field is at end of string.
C
	iend = INDEX  ( string ( ipoint: ), ' ' )
	IF  ( iend .eq. 0 ) THEN
	    iend = lens
	  ELSE
	    iend = iend + ipoint - 2
	END IF
C
C*	Return substring and length.
C
	field  = string ( ipoint : iend )
	lenf   = iend - ipoint + 1
	ipoint = iend + 2
C
C*	Check for special case where field is 10 characters.
C
	IF  ( lenf .eq. 10 )  THEN
	    lenf   = 5
	    field  = field ( 1: 5 )
	    ipoint = ipoint - 6
	END IF
C
C*	Move pointer past trailing blanks.
C
	DO WHILE  ( ( string (ipoint:ipoint) .eq. ' ' ) .and.
     +		    ( ipoint .le. lens ) )
	    ipoint = ipoint + 1
	END DO
C*
	RETURN
	END
