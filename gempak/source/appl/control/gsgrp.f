	SUBROUTINE GSGRP  ( igroup, iret )
C************************************************************************
C* GSGRP								*
C* 									*
C* This subroutine starts a new drawing element group, defined for      *
C* product generation.							*
C* 									*
C* GSGRP  ( IGROUP, IRET )						*
C*									*
C* Input parameters:							*
C*	IGROUP		INTEGER		Group type			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 6/97						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (3)
C------------------------------------------------------------------------
	isend (1) = 3
	isend (2) = FSGRP
	isend (3) = igroup
C
	CALL GPUT  ( isend, 3, iret )
	IF ( iret .ne. NORMAL )  RETURN
C
        CALL GGET  ( iret , 1, ierr )
        IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
