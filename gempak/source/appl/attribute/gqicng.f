	SUBROUTINE GQICNG ( szicng, icewid, iret )
C************************************************************************
C* GQICNG								*
C*									*
C* This subroutine returns the icing symbol size and line width.	*
C*									*
C* GQICNG ( SZICNG, ICEWID, IRET )					*
C*									*
C* Output parameters:							*
C*	SZICNG		REAL		Icing symbol size		*
C*	ICEWID		INTEGER		Line width		        *
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
	isend (2) = FQICNG
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
	CALL GGETR ( szicng, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GGET ( icewid, 1, iret )
C*
	RETURN
	END
