	SUBROUTINE GQRDUC ( filter, iret )
C************************************************************************
C* GQRDUC								*
C*									*
C* This subroutine returns the filter factor for the point reduction	*
C* scheme.								*
C*									*
C* GQRDUC ( FILTER, IRET )						*
C*									*
C* Output parameters:							*
C*	FILTER		REAL		Filter factor for pnt reduction	*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 5/99						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQRDUC
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
	CALL GGETR  ( filter, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
