	SUBROUTINE GQPWTH  ( szpwth, ipwwid, iret )
C************************************************************************
C* GQPWTH								*
C* 									*
C* This subroutine returns the past weather symbol attributes,          *
C* including the past weather symbol size and line width.		*
C* 									*
C* GQPWTH  ( SZPWTH, IPWWID, IRET )					*
C*									*
C* Output parameters:							*
C* 	SZPWTH		REAL		Past weather symbol size 	*
C*	IPWWID		INTEGER		Line width			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* T. Lee/GSC		 9/97	Fixed IRET inconsistency		*
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
	isend (2) = FQPWTH
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL ) THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR ( szpwth, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET ( ipwwid, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END

