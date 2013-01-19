	SUBROUTINE GEGRP  ( iret )
C************************************************************************
C* GEGRP								*
C* 									*
C* This subroutine ends a new drawing element group, defined for        *
C* product generation.							*
C* 									*
C* GEGRP  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* S. Maxwell/GSC	 7/97						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
	isend (1) = 2
	isend (2) = FEGRP
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
