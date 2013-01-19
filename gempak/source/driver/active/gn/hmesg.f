	SUBROUTINE HMESG  ( messag, iret )
C************************************************************************
C* HMESG - GN								*
C*									*
C* This subroutine writes a frame title to a sequential device.		*
C*									*
C* HMESG  ( MESSAG, IRET )						*
C*									*
C* Input parameters:							*
C*	MESSAG		CHAR*		Message				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	messag
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
