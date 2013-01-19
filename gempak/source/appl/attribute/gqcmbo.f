	SUBROUTINE GQCMBO ( szcmwx, icmbwd, iret )
C************************************************************************
C* GQCMBO								*
C* 									*
C* This subroutine returns the combination weather code symbol          *
C* attributes, including the weather symbol size and line width.        *
C* 									*
C* GQCMBO ( SZCMWX, ICMBWD, IRET )					*
C*									*
C* Output parameters:							*
C* 	SZCMWX		REAL		Combination symbol size         *
C*	ICMBWD		INTEGER		Combination symbol line width   *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC         10/98   Copied from GQWTHR                      *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQCMBO
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
	CALL GGETR ( szcmwx, 1, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    CALL GGET ( icmbwd, 1, ierr )
	END IF
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
