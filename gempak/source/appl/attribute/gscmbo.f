	SUBROUTINE GSCMBO ( szcmwx, icmbwd, iret )
C************************************************************************
C* GSCMBO								*
C* 									*
C* This subroutine sets the combination weather code symbol attributes, *
C* including the weather symbol size and line width.  If these          * 
C* parameters are not positive, no change is made.            		*
C* 									*
C* GSCMBO ( SZCMWX, ICMBWD, IRET )					*
C*									*
C* Input parameters:							*
C* 	SZCMWX		REAL		Combination symbol size         *
C*	ICMBWD		INTEGER		Combination symbol line width	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC          6/98   Copied form GSWTHR                      *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = FSCMBO
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szcmwx, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( icmbwd, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END

