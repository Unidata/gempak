	SUBROUTINE DSKY ( iwndw, np, skcod, x, y, ixoff, iyoff, iret )
C************************************************************************
C* DSKY									*
C*									*
C* This subroutine draws sky coverage symbols on the current graphics	*
C* device.								*
C*									*
C* DSKY ( IWNDW, NP, SKCOD, X, Y, IXOFF, IYOFF, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of symbols		*
C*	SKCOD (NP)	REAL		Sky coverage symbol codes	*
C*	X     (NP)	REAL		X coordinates in device units	*
C*	Y     (NP)	REAL		Y coordinates in device units	*
C*	IXOFF (NP)	INTEGER		X offsets in half characters	*
C*	IYOFF (NP)	INTEGER		Y offsets in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC		GEMPLT Version 5.0			*
C* M. Linda/GSC		 3/96	Added check for DEVICE buffer overflow	*
C* M. Linda/GSC		12/96	Changed X, Y, and symbol code to reals	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		skcod (*), x (*), y (*)
	INTEGER		ixoff (*), iyoff (*)
C
	INTEGER		isend (4)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check for zero points.
C
	IF ( np .le. 0 ) RETURN
C
C*	Check if DEVICE buffer will overflow.
C
	isnd = 2 + ( 2 + ( 5 * np ) )
	ircv = 1 + ( 1 )
	IF ( ( isnd + ircv ) .gt. IGDSIZ ) THEN
	    iret = NXDBUF
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = isnd
	isend (2) = CSKY
	isend (3) = iwndw
	isend (4) = np
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( skcod, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( x, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( y, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( ixoff, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( iyoff, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
