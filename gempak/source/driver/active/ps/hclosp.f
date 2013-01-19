	SUBROUTINE HCLOSP  ( ncurwn, iret )
C************************************************************************
C* HCLOSP - PS								*
C* 									*
C* This subroutine closes the plot file.				*
C*									*
C* HCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* M. desJardins/GSFC	12/90						*
C* A. Chang/EAI          2/94   Modified to call C routine              *
C* S. Jacobs/NCEP	 4/96	Added NCURWN				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C*
	CALL PCLOSP ( iret )
C*
	RETURN
	END
