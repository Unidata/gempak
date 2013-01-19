	SUBROUTINE GLOOPC  ( icomm, iret )
C************************************************************************
C* GLOOPC								*
C* 									*
C* Send loop control commands to D.D.					*
C*									*
C* GLOOPC  ( ICOMM, IRET )						*
C*									*
C* Input parameters:							*
C*	ICOMM		INTEGER						*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI	 	12/93						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	CALL DLOOPC ( icomm, ier )
C*
	RETURN
	END
