	SUBROUTINE GSSPCL ( szspcl, ispwid, iret )
C************************************************************************
C* GSSPCL								*
C*									*
C* This subroutine sets the special symbol attributes, including the    *
C* special symbol size and line width.  If these parameters are not     *
C* positive, no change is made.		              		        *
C*									*
C* GSSPCL ( SZSPCL, ISPWID, IRET )					*
C*									*
C* Input parameters:							*
C*	SZSPCL		REAL		Special symbol size		*
C*	ISPWID		INTEGER		Line width			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on GSWTHR				*
C* A. Hardy/GSC		 6/98	Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = FSSPCL
C
	CALL GPUT ( isend, 2, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( szspcl, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( ispwid, 1, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF  ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
