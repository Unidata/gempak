	SUBROUTINE GQFILL  ( szfil, iftyp, iret )
C************************************************************************
C* GQFILL								*
C* 									*
C* This subroutine returns the fill pattern size and the fill pattern   *
C* type.                                                                *
C* 									*
C* GQFILL  ( SZFIL, IFTYP, IRET )					*
C*									*
C* Output parameters:							*
C* 	SZFIL		REAL		Fill pattern size 		*
C*	IFTYP		INTEGER		Fill pattern type		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 6/97						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQFILL
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
	CALL GGETR ( szfil, 1, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    CALL GGET ( iftyp, 1, ierr )
	END IF
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
