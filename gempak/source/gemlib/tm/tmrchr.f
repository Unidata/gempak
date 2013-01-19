	SUBROUTINE TM_RCHR  ( string, iret )
C************************************************************************
C* TM_RCHR								*
C*									*
C* This subroutine reads a character string from the terminal and 	*
C* checks for <CR> or EXIT.						*
C*									*
C* TM_RCHR  ( STRING, IRET )						*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		User input			*
C*	IRET		INTEGER		Return code			*
C*					  2 = EXIT entered		*
C*					  1 = <CR> entered		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. Goodman/RDS	 4/84	Original source code			*
C* M. desJardins/GSFC	 5/84	Eliminated length as output parameter	*
C* M. desJardins/GSFC	 3/88	Cleaned up				*
C* S. Schotz/GSC	 8/90	Added parsing of <CTRL Z>		*
C************************************************************************
	CHARACTER*(*) 	string, strout*132
	LOGICAL 	abbr
C------------------------------------------------------------------------
	iret = 0
C
C*	Read input string and get length.
C
	READ  ( 5, 1000, END = 9999 )  string
1000	FORMAT ( A )
	CALL ST_LSTR  ( string, length, ierr )
C
C*	Check if only a <CR> was input.
C
	IF  ( length .eq. 0 )  THEN
	    iret = 1
C
C*	    Check to see if 'EXIT' was entered.
C
	  ELSE IF  ( length .le. 4 )  THEN
	     CALL ST_LCUC  ( string, strout, ierr )
	     CALL ST_ABBR  ( 'EXIT', strout, abbr, ierr )
	     IF  ( abbr )  iret = 2 
	END IF
	RETURN
C*
9999	CONTINUE
	iret = 2
C*
	RETURN
	END
