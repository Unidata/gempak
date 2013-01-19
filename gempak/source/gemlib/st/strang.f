	SUBROUTINE ST_RANG  ( string, first, last, inc, itype, iret )
C************************************************************************
C* ST_RANG								*
C*									*
C* This subroutine changes a string range into the beginning, end, 	*
C* and increment values.  The values must be separated by '-'.		*
C*									*
C* ST_RANG  ( STRING, FIRST, LAST, INC, ITYPE, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:							*
C*	FIRST 		CHAR*		First value in range		*
C*	LAST		CHAR*		Last value in range		*
C*	INC		CHAR*   	Range increment			*
C*	ITYPE		INTEGER 	Range type			*
C*				   	  0 = no range input 		*
C*				   	  1 = range without increment	*
C*				   	  2 = range with increment	*
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/84	Original source				*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	11/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP         2/96   Increased size of strbuf                *
C************************************************************************
	CHARACTER*(*) 	string, first, last, inc
C*
	CHARACTER*160 	strbuf
C------------------------------------------------------------------------
	iret  = 0
	first = ' '
	last  = ' '
	inc   = ' '
	itype = 0
C
C*	Remove blanks from the input string.
C
	CALL ST_RMBL  ( string, strbuf, length, iret )
	IF  ( length .le. 0 )  RETURN
C
C*	Check for dash (-); eliminate dashes at start of elements since 
C*	these are minus signs.
C
	idash = INDEX  ( string, '-' )
	IF  ( idash .eq. 1 )  THEN
	    idash = INDEX  ( string ( 2: ), '-' )
	    IF  ( idash .ne. 0 )  idash = idash + 1
	END IF
C
C*	Return if there is no dash.
C
	IF  ( idash .eq. 0 )  RETURN
C
C*	Get first element of range.
C
	first = string ( 1: idash - 1 )
C
C*	Check for second dash.
C
	ipos  = INDEX  ( string ( idash + 1: ), '-' )
	IF  ( ipos .eq. 1 )  THEN
	    ipos = INDEX  ( string ( idash + 2: ), '-' )
	    IF  ( ipos .ne. 0 )  ipos = ipos + 1
	END IF
C
C*	Get second value in range.  Also, get increment if there is a
C*	second dash.
C
	IF  ( ipos .eq. 0 )  THEN
	    last = string ( idash + 1: )
	    IF  ( last .eq. ' ' )  last = first
	    itype = 1
	  ELSE
	    last  = string ( idash + 1: idash + ipos - 1 )
	    inc   = string ( idash + ipos + 1: )
	    itype = 2
	    IF  ( inc .eq. ' ' )  itype = 1
	END IF
C*
	RETURN
	END
