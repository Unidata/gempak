	SUBROUTINE DLOOPC  ( icomm, iret ) 
C************************************************************************
C* DLOOPC								*
C* 									*
C* Send loop control commands to D.D.                                   *
C*									*
C* DLOOPC  ( ICOMM, IRET )						*
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
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
C*
	INTEGER isend (3)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = CLOOPC
	isend (3) = icomm
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret , 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
