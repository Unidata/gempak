	SUBROUTINE GSCTYP  ( szctyp, ictwid, iret )
C************************************************************************
C* GSCTYP								*
C* 									*
C* This subroutine sets the cloud type symbol attributes, including     *
C* the cloud symbol size and line width.  If these parameters are not   *
C* positive, no change is made.   					*
C* 									*
C* GSCTYP  ( SZCTYP, ICTWID, IRET )					*
C*									*
C* Input parameters:							*
C* 	SZCTYP		REAL		Cloud symbol size 		*
C*	ICTWID		INTEGER		Line width			*
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
	isend (2) = FSCTYP
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szctyp, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( ictwid, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
