	SUBROUTINE GSHASH  ( szhsh, ihwid, ilwid, iret )
C************************************************************************
C* GSHASH								*
C* 									*
C* This subroutine sets the hash mark size, line width and line spacing.*
C* If these parameters are not positive, no change is made.		*
C*									*
C* GSHASH ( SZHSH, IHWID, ILWID, IRET )					*
C*									*
C* Input parameters:							*
C*	SZHSH		REAL		Hash mark size 			*
C*	IHWID		INTEGER		Line width 			*
C*	ILWID		INTEGER		Line spacing 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C* A. Hardy/GSC 	 6/98		Cleaned up prolog		*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 5
	isend (2) = FSHASH
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR ( szhsh, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = ihwid
	isend (2) = ilwid
	CALL GPUT ( isend, 2, iret)
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
