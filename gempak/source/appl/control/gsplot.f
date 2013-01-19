	SUBROUTINE GSPLOT  ( iret )
C************************************************************************
C* GSPLOT								*
C* 									*
C* This subroutine defines the start of a new frame within an animation *
C* sequence.                                                            *
C* 									*
C* GSPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* A. Chang/EAI	 	12/93						*
C* L. Williams/EAi       3/94   Changed file description and removed    *
C*                              blank comments from header              *
C* A. Hardy/GSC		 6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FSPLOT
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
