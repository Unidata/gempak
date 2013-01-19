	SUBROUTINE GENANM  ( iret )
C************************************************************************
C* GENANM								*
C* 									*
C* This subroutine ends an animation sequence.   			*
C* 									*
C* GENANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* A. Chang/EAI	 	12/93						*
C* L. Williams/EAi       3/94   Renamed file from GEPIXM to GENANM and  *
C*                              removed blank comments from header      *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FENANM
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
