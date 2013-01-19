	SUBROUTINE GQSMTH ( ismtyp, dens, iret )
C************************************************************************
C* GQSMTH								*
C*									*
C* This subroutine returns the line smoothing attributes, including     *
C* the smoothing type and the density of intermediate points.           *
C*									*
C* GQSMTH ( ISMTYP, DENS, IRET )					*
C*									*
C* Output parameters:							*
C*	ISMTYP		INTEGER		Smoothing type			*
C*					  0 = none			*
C*					  1 = splines			*
C*					  2 = parametric		*
C*	DENS		REAL		Density of intermediate points	*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 2/98						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQSMTH
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( ircv, 2, ier )
	IF  ( ier .eq. NORMAL )  THEN
	    iret   = ircv (1)
	    ismtyp = ircv (2)
	    CALL GGETR  ( dens, 1, iret )
	END IF
C*
	RETURN
	END
