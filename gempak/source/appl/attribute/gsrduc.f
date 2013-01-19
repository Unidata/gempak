	SUBROUTINE GSRDUC ( filter, iret )
C************************************************************************
C* GSRDUC								*
C*									*
C* This subroutine sets the filter factor for the point reduction	*
C* scheme.								*
C*									*
C* GSRDUC ( FILTER, IRET )						*
C*									*
C* Input parameters:							*
C*	FILTER		REAL		Filter factor for pnt reduction	*
C*									*
C* Output parameters:							*
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
	isend (1) = 3
	isend (2) = FSRDUC
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( filter, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
