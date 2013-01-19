	SUBROUTINE GQCTYP  ( szctyp, ictwid, iret )
C************************************************************************
C* GQCTYP								*
C* 									*
C* This subroutine returns the cloud type symbol attributes, including  *
C* the cloud symbol size and line width. 				*
C* 									*
C* GQCTYP  ( SZCTYP, ICTWID, IRET )					*
C*									*
C* Output parameters:							*
C* 	SZCTYP		REAL		Cloud symbol size 		*
C*	ICTWID		INTEGER		Line width			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C* M. desJardins/NMC	10/91	Fixed return code			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
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
	isend (2) = FQCTYP
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR  ( szctyp, 1, ierr )
	IF  ( iret .eq. NORMAL )  THEN
	    CALL GGET  ( ictwid, 1, ierr )
	END IF
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
