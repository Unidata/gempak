	SUBROUTINE GSSMTH ( ismtyp, dens, iret )
C************************************************************************
C* GSSMTH								*
C*									*
C* This subroutine sets the line smoothing attributes, including the    *
C* smoothing type and the density of intermediate points.		*
C*									*
C* GSSMTH ( ISMTYP, DENS, IRET )					*
C*									*
C* Input parameters:							*
C*	ISMTYP		INTEGER		Smoothing type			*
C*					  0 = none			*
C*					  1 = splines			*
C*					  2 = parametric		*
C*	DENS		REAL		Density of intermediate points	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 2/98						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (3)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = FSSMTH
	isend (3) = ismtyp
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( dens, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
