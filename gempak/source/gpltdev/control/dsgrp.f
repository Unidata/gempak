	SUBROUTINE DSGRP  ( igroup, iret ) 
C************************************************************************
C* DSGRP								*
C* 									*
C* This subroutine starts a new drawing element group.			*
C*									*
C* DSGRP  ( IGROUP, IRET )						*
C*									*
C* Input parameters:							*
C*	IGROUP		INTEGER		Group type			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 7/97						*
C************************************************************************
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
C*
	INTEGER isend (3)
C------------------------------------------------------------------------
	isend (1) = 3
	isend (2) = CSGRP
	isend (3) = igroup
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret , 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
