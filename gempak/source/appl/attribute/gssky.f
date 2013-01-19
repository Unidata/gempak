	SUBROUTINE GSSKY  ( szsky, isktyp, iskwid, iret)
C************************************************************************
C* GSSKY								*
C* 									*
C* This subroutine sets the sky coverage attributes, including the sky  *
C* coverage symbol size, symbol type and line width.  If these          *
C* parameters are not positive, no change is made.    			*
C* 									*
C* GSSKY  ( SZSKY, ISKTYP, ISKWID, IRET)				*
C*									*
C* Input parameters:							*
C* 	SZSKY		REAL		Sky coverage symbol size 	*
C*	ISKTYP		INTEGER		Symbol type			*
C*					  1 = not filled in		*
C*					  2 = filled in			*
C*	ISKWID		INTEGER		Line width			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
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
	isend (1) = 5
	isend (2) = FSSKY
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szsky, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = isktyp
	isend (2) = iskwid
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END

