	SUBROUTINE ST_ALNM  ( chrstr, ityp, iret )
C************************************************************************
C* ST_ALNM                                    				*
C*									*
C* This subroutine determines whether a character is a letter, number	*
C* or non-alphanumeric character.					*
C* 									*
C* ST_ALNM  ( CHRSTR, ITYP, IRET )					*
C*									*
C* Input parameters: 							*
C*	CHRSTR		CHAR*1		Character to analyze		*
C*									*
C* Output parameters:							*
C*	ITYP		INTEGER		Character type			*
C*				   	  0 = non-alphanumeric		*
C*				   	  1 = number			*
C*				   	  2 = letter			*
C*	IRET		INTEGER 	Return code 			*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/84						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	CHARACTER*(*) 	chrstr
C*
C*-----------------------------------------------------------------------
	iret = 0
	ityp = 0
C
C*	Check for numbers; then letters.
C
	IF  ( (chrstr .ge. '0') .and. (chrstr .le. '9') )  ityp = 1
	IF  ( (chrstr .ge. 'a') .and. (chrstr .le. 'z') )  ityp = 2
	IF  ( (chrstr .ge. 'A') .and. (chrstr .le. 'Z') )  ityp = 2
C*
	RETURN
	END
