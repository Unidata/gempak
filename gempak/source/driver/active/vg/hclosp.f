	SUBROUTINE HCLOSP  ( ncurwn, iret )
C************************************************************************
C* HCLOSP - VG								*
C* 									*
C* This subroutine closes the VG plot file.				*
C*									*
C* HCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* J. Wu/GSC	02/01   	Created based on hclosp.f for PS device	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C*
	CALL VCLOSP ( iret )
C*
	RETURN
	END
