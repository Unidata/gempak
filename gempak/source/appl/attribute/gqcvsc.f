	SUBROUTINE GQCVSC  ( dvcvsc, iret )
C************************************************************************
C* GQCVSC								*
C*									*
C* This subroutine returns the curve scaling factor of a device driver.	*
C*									*
C* GQCVSC  ( DVCVSC, IRET )						*
C*									*
C* Output parameters:							*
C*	DVCVSC		REAL		Curve scaling factor		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Lee/GSC		 7/98						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQCVSC
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret   = ierr
	    RETURN
	END IF
C
	CALL GGETR  ( dvcvsc, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
