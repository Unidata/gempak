	SUBROUTINE GQSKY  ( szsky, isktyp, iskwid, iret)
C************************************************************************
C* GQSKY								*
C* 									*
C* This subroutine returns the sky coverage attributes, including the   *
C* sky coverage symbol size, symbol type and line width.  	        *
C* 									*
C* GQSKY  ( SZSKY, ISKTYP, ISKWID, IRET)				*
C*									*
C* Output parameters:							*
C* 	SZSKY		REAL		Sky coverage symbol size 	*
C*	ISKTYP		INTEGER		Symbol type			*
C*					  1 = not filled in		*
C*					  2 = filled in			*
C*	ISKWID		INTEGER		Line width			*
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
C
	INTEGER		isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQSKY
C
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
	CALL GGETR  ( szsky, 1, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    CALL GGET  ( ircv, 2, ierr )
	    IF  ( ierr .eq. NORMAL )  THEN 
		isktyp  = ircv (1)
		iskwid  = ircv (2)
	    END IF
	END IF
	IF  ( ierr .ne. 0 )  iret = ierr
C*
	RETURN
	END
