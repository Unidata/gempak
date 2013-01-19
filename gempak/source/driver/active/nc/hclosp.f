	SUBROUTINE HCLOSP   ( ncurwn, iret )
C************************************************************************
C* HCLOSP - NC 								*
C* 									*
C* This subroutine closes the plot file.				*
C*									*
C* HCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI          4/94                                           *
C* S. Jacobs/NMC	 6/94	General clean up			*
C* S. Jacobs/NCEP	 4/96	Added NCURWN				*
C* S. Maxwell/GSC        6/97   Documentation changes  		        *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C
	CALL MCLOSE ( iret )
C*
	RETURN
	END
