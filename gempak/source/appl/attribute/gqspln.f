	SUBROUTINE GQSPLN  ( isltyp, islstr, isldir, slsiz, islwid,
     +			     iret )
C************************************************************************
C* GQSPLN								*
C*									*
C* This subroutine returns the current special line attributes,		*
C* including the special line type, stroke size, direction indicator,   *
C* pip size and line width.				                *
C*									*
C* GQSPLN  ( ISLTYP, ISLSTR, ISLDIR, SLSIZ, ISLWID, IRET )		*
C*									*
C* Output parameters:							*
C*	ISLTYP		INTEGER		Special line type               *
C*	ISLSTR		INTEGER		Stroke size - not used		*
C*	ISLDIR		INTEGER		Direction indicator		*
C*					  1 = up or out                 *
C*					  0 = no change                 *
C*					 -1 = down or in                *
C*	SLSIZ		REAL		Pip size		        *
C*	ISLWID		INTEGER		Line width 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/97		Copied from GQLINE		*
C* A. Hardy/GSC          6/98           Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2), ircv (3)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQSPLN

	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( ircv, 3, ierr )
	IF  ( iret .eq. NORMAL ) iret = ierr
	    isltyp = ircv (1)
	    islstr = ircv (2)
	    isldir = ircv (3)
C
	CALL GGETR  ( slsiz, 1, ierr )
	IF  ( iret .eq. NORMAL ) iret = ierr
C
	CALL GGET  ( islwid, 1, ierr )
	IF  ( iret .eq. NORMAL ) iret = ierr
C*
	RETURN
	END
