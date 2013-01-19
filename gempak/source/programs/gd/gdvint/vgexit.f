	SUBROUTINE VG_EXIT  ( exit, iret )
C************************************************************************
C* VG_EXIT								*
C*									*
C* This subroutine executes a GEMPAK exit upon the user's request.	*
C*									*
C* VG_EXIT ( EXIT, IRET )						*
C*									*
C* Output parameters:							*
C*	EXIT		LOGICAL		Exit flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = GEMPAK detected error	*
C**									*
C* Log:									*
C* K. Brill/NMC      07/92						*
C************************************************************************
	INCLUDE		'vicmn.cmn'
	LOGICAL		exit
C-----------------------------------------------------------------------
	iret = 0
C*
	CALL IP_DYNM ( exit, ier )
	IF ( exit ) CALL IP_EXIT ( iret )
C*
	RETURN
	END
