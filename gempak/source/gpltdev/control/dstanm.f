	SUBROUTINE DSTANM  ( iret )
C************************************************************************
C* DSTANM								*
C* 									*
C* This subroutine defines the start of a new animation sequence.       *
C* 									*
C* DSTANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* A. Chang/EAI	 	12/93						*
C* S. Jacobs/NMC         3/94           Renamed DSPIXM to DSTANM        *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = CSTANM
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
