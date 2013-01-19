   	SUBROUTINE ST_INLN  ( intg, string, lens, iret )
C************************************************************************
C* ST_INLN								*
C*									*
C* This subroutine converts an integer to a character string.  Unlike	*
C* ST_INCH, the length of the string is returned.			*
C* 									*
C* ST_INLN  ( INTG, STRING, LENS, IRET )				*
C*									*
C* Input parameters:							*
C*	INTG		INTEGER		Integer				*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		String				*
C*	LENS		INTEGER		Length of string		*
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C*				  	 -2 = conversion error		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 2/84	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	From ST_INCH				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C*
	CHARACTER	str*10
C------------------------------------------------------------------------
	iret   = 0
	lens   = 0
	string = ' '
C
C*	Encode number into string.
C
	WRITE  ( str, 5000, IOSTAT = ier )  intg
5000	FORMAT ( I10 )
	IF  ( ier .gt. 0 )  THEN
	    iret = -2
	    WRITE  ( str, 5000 )  IMISSD
	END IF
C
C*	Remove leading spaces.
C
	CALL ST_LDSP  ( str, string, lens, ier )
C*
	RETURN
	END
