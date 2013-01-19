	SUBROUTINE HMESG  ( messag, iret )
C************************************************************************
C* HMESG - NC								*
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
C* A. Chang/EAI          4/94                                           *
C* S. Jacobs/NMC	 6/94		General clean up		*
C* S. Maxwell/GSC        6/97           Documentation changes           *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	messag
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL ST_LSTR ( messag, ilen, ier )
C
	CALL MMESG ( messag, ilen, iret )
C*
	RETURN
	END
