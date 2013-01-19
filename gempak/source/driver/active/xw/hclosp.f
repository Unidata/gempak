	SUBROUTINE HCLOSP  ( ncurwn, iret )
C************************************************************************
C* HCLOSP - XW								*
C* 									*
C* This subroutine closes the plot file/window.				*
C*									*
C* HCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 4/96	Aded NCURWN				*
C* S. Maxwell/GSC	 6/97	Documentation changes     		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Close the window.
C
	CALL XCLOSP ( ixsize, iysize, ncurwn, iret )
	iright = ixsize
	ibot   = iysize
C*
	RETURN
	END
