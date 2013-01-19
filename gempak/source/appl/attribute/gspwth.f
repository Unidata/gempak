	SUBROUTINE GSPWTH  ( szpwth, ipwwid, iret )
C************************************************************************
C* GSPWTH								*
C* 									*
C* This subroutine sets the past weather symbol attributes, including   *
C* the past weather symbol size and line width.  If these parameters are*
C* not positive, no change is made.					*
C* 									*
C* GSPWTH  ( SZPWTH, IPWWID, IRET )					*
C*									*
C* Input parameters:							*
C* 	SZPWTH		REAL		Past weather symbol size 	*
C*	IPWWID		INTEGER		Line width			*
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
	isend (1) = 4
	isend (2) = FSPWTH
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szpwth, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( ipwwid, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END

