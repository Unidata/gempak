	SUBROUTINE GSICNG ( szicng, icewid, iret )
C************************************************************************
C* GSICNG								*
C*									*
C* This subroutine sets the icing symbol size and line width.  If these *
C* parameters are not positive, no change is made.	                *
C*									*
C* GSICNG ( SZICNG, ICEWID, IRET )					*
C*									*
C* Input parameters:							*
C*	SZICNG		REAL		Icing symbol size		*
C*	ICEWID		INTEGER		Line width		        *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on GSWTHR				*
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
	isend (2) = FSICNG
C
	CALL GPUT ( isend, 2, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( szicng, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( icewid, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF  ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
