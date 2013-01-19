	SUBROUTINE GQTURB ( szturb, ituwid, iret )
C************************************************************************
C* GQTURB								*
C*									*
C* This subroutine returns the turbulence symbol attributes, including  *
C* the turbulence symbol size and line width.                           *
C*									*
C* GQTURB ( SZTURB, ITUWID, IRET )					*
C*									*
C* Output parameters:							*
C*	SZTURB		REAL		Turbulence symbol size		*
C*	ITUWID		INTEGER		Line width	                *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on GQWTHR				*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQTURB
C
	CALL GPUT ( isend, 2, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL ) THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR ( szturb, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GGET ( ituwid, 1, iret )
C*
	RETURN
	END
