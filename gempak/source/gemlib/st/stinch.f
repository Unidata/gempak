   	SUBROUTINE ST_INCH  ( intg, string, iret )
C************************************************************************
C* ST_INCH								*
C*									*
C* This subroutine encodes an integer in a character string.		*
C* 									*
C* ST_INCH  ( INTG, STRING, IRET )					*
C*									*
C* Input parameters:							*
C*	INTG		INTEGER		Integer				*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		Encoded value			*
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C*				  	 -2 = error on conversion	*
C**									*
C* Log:									*
C* I. Graffman/CSC	 7/82	STR_INCHR				*
C* I. Graffman/RDS	 2/84	to use new GEMPAK routines		*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	string
	CHARACTER	str*10
C------------------------------------------------------------------------
	iret   = 0
	nc     = 0
	string = ' '
C
C*	Encode number into string.
C
	WRITE  ( str, 5000, IOSTAT = ier ) intg
5000	FORMAT ( I10 )
	IF  ( ier .gt. 0 )  THEN
	    iret = -2
	    WRITE  ( str, 5000 )  IMISSD
	END IF
C
C*	Remove leading spaces.
C
	CALL ST_LDSP  ( str, string, nc, ier )
C*
	RETURN
	END
