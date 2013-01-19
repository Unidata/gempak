	SUBROUTINE WW_WNUM ( string, lens, wnum, iret )
C************************************************************************
C* WW_WNUM  								*
C*									*
C* This subroutine gets the watch box number.                           *
C*                                                                      *
C* WW_WNUM  ( STRING, LENS, WNUM, IRET )                                *
C*									*
C* Input parameters:							*
C*	STRING 		CHAR*		String containing watch number  *
C*	LENS		INTEGER		Length of string	        *
C*									*
C* Output parameters:							*
C*	WNUM 		CHAR*		Watch box number                *
C*	IRET		INTEGER		Return code			*
C*				          0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 5/99	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, wnum
C*
	CHARACTER	wchnum*6
C------------------------------------------------------------------------
	iret  = 0
	wnum = ' '
C
	CALL ST_LDSP ( string ( :lens ), wchnum, nc, ier )
	nc = MIN ( nc, 6 )
C
C*	Stop when the first non-numeric is reached.
C
	DO i = 1, nc
	    CALL ST_ALNM ( wchnum ( i:i ), ityp, ier )
	    IF ( ityp .ne. 1 ) RETURN
	    wnum ( i:i ) = wchnum ( i:i )
	END DO
C*
	RETURN
	END
