	SUBROUTINE DENANM  ( iret )
C************************************************************************
C* DENANM								*
C* 									*
C* This subroutine ends an animation sequence.                          *
C* 									*
C* DENANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI	 	12/93						*
C* S. Jacobs/NMC         2/94           Renamed DEPIXM to DENANM        *
C* S. Jacobs/NMC	 6/94		Added call to HENANM		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver to end the sequence.
C
	CALL HENANM ( iret )
C*
	RETURN
	END
