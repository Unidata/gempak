	SUBROUTINE DEPLOT  ( iret )
C************************************************************************
C* DEPLOT								*
C* 									*
C* This subroutine is called when a plot is complete.  It can be 	*
C* used to take a terminal out of graphics mode.			*
C* 									*
C* DEPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* M. desJardins/GSFC	 5/88						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = CEPLOT
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
