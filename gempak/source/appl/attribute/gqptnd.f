	SUBROUTINE GQPTND  ( szptnd, iptwid, iret )
C************************************************************************
C* GQPTND								*
C* 									*
C* This subroutine returns the pressure tendency symbol attributes,	*
C* including the pressure tendency symbol size and line width. 		*
C* 									*
C* GQPTND  ( SZPTND, IPTWID, IRET )					*
C*									*
C* Output parameters:							*
C* 	SZPTND		REAL		Pressure tendency symbol size   *
C*	IPTWID		INTEGER		Line width			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* T. Lee/GSC		 9/97	Fixed IRET inconsistency		*
C* T. Piper/GSC		 5/98	Corrected typo in documentation         *
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
	isend (2) = FQPTND
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
	CALL GGETR ( szptnd, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET ( iptwid, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END

