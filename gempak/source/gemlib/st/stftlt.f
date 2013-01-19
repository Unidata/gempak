	SUBROUTINE ST_FTLT  ( string, outstr, iret )
C************************************************************************
C* ST_FTLT                                     				*
C* 									*
C* This subroutine returns the first and last characters in a string.	*
C* 									*
C* ST_FTLT  ( STRING, OUTSTR, IRET )					*
C*									*
C* Input parameters:                                                   	*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:                                                  	*
C*	OUTSTR		CHAR*		Output string of 2 characters  	*
C*	IRET		INTEGER 	Return code  			*
C*				   	 0 = normal return 		*
C** 									*
C* K. Brill/NMC		08/91						*
C************************************************************************
	CHARACTER*(*)	string, outstr
C*
C------------------------------------------------------------------------
	iret   = 0
	outstr = ' '
C
C*	Find length of input character string.
C
	CALL ST_LSTR ( string, isiz, iret )
	IF  ( isiz .le. 0 )  RETURN
C*
	outstr (1:1) = string (1:1)
	outstr (2:2) = string (isiz:isiz)
	RETURN
	END
