	SUBROUTINE DEGRP  ( iret )
C************************************************************************
C* DEGRP								*
C* 									*
C* This subroutine ends a new drawing element group.		 	*
C* 									*
C* DEGRP  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* S. Maxwell/GSC	 7/97						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
C------------------------------------------------------------------------
	isend (1) = 2
	isend (2) = CEGRP
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
