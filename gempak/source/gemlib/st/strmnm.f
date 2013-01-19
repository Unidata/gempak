	SUBROUTINE ST_RMNM  ( string, outstr, nncr, lens, iret )
C************************************************************************
C* ST_RMNM                                     				*
C* 									*
C* This subroutine removes numeric characters from a string.		*
C* 									*
C* ST_RMNM  ( STRING, OUTSTR, NNCR, LENS, IRET )			*
C*									*
C* Input parameters:                                                   	*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:                                                  	*
C*	OUTSTR		CHAR*		Converted string          	*
C*	NNCR		INTEGER		Number of numeric CHARs removed	*
C*	LENS		INTEGER		Length of output string		*
C*	IRET		INTEGER 	Return code  			*
C*				   	 0 = normal return 		*
C** 									*
C* K. Brill/NMC		08/91						*
C* S. Jacobs/NCEP	11/96	Added temporary variables sss and ttt;	*
C*				Added ST_ALNM to find numerals		*
C************************************************************************
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	sss*160, ttt*160
C------------------------------------------------------------------------
	iret = 0
	sss  = string
	ttt  = ' '
	lens = 0
C
C*	Find length of input character string.
C
	CALL ST_LSTR ( sss, isiz, iret )
	IF  ( isiz .le. 0 )  RETURN
C
C*	Check each character in string.
C
	io   = 1
	nncr = 0
	DO  i = 1, isiz
C
C*	    Find numeric characters.
C
	    CALL ST_ALNM  ( sss (i:i), itype, iret )
	    IF  ( itype .ne. 1 )  THEN
		ttt (io:io) = sss (i:i)
		io = io + 1
	      ELSE
		nncr = nncr + 1
	    END IF 
	END DO
C
C*	Transfer to output string.
C
	lens = io - 1
	IF  ( lens .gt. 0 )  outstr = ttt (1:lens)
C*
	RETURN
	END
