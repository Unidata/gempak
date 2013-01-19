	SUBROUTINE DSTANM  ( iret )
C************************************************************************
C* DSTANM								*
C* 									*
C* This subroutine starts a new animation sequence.			*
C* 									*
C* DSTANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI	 	12/93						*
C* S. Jacobs/NMC         3/94           Renamed DSPIXM to DSTANM        *
C* S. Jacobs/NMC	 6/94		Added call to HSTANM		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver to start the sequence.
C
	CALL HSTANM ( iret )
C*
	RETURN
	END
