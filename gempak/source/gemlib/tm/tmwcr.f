	SUBROUTINE TM_WCR  ( iret )
C************************************************************************
C* TM_WCR								*
C*									*
C* This subroutine prompts the user with the message,			*
C*									*
C*               'Enter <CR> to continue'				*
C*									*
C* and waits for the user to enter a carriage return.			*
C*									*
C* TM_WCR  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/CSC	 4/82	Original source code			*
C* M. Goodman/RDS	 4/84	Added IRET return code			*
C* K. Brill/NMC		01/92	Call TM_WLIN				*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	CHARACTER 	char*80
C------------------------------------------------------------------------
	iret = 0
C*
	char = 'Press ENTER to continue: '
	CALL TM_WLIN ( char, .false., ier )
C
C*	Wait for some user input.
C
	READ   ( 5, 5010 )  char
5010	FORMAT ( A )
C*
	RETURN
	END
