	SUBROUTINE DTICMK ( iwndw, itick, size, np, x, y, iret )
C************************************************************************
C* DTICMK								*
C*									*
C* This subroutine draws tic marks on the current graphics device.	*
C*									*
C* DTICMK ( IWNDW, ITICK, SIZE, NP, X, Y, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	ITICK		INTEGER		Tick mark number		*
C*	SIZE		REAL		Tick size			*
C*	NP		INTEGER		Number of tick marks		*
C*	X (NP)		REAL		X coordinates in device units	*
C*	Y (NP)		REAL		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/89	Added size				*
C* M. Linda/GSC		 3/96	Added check for DEVICE buffer overflow	*
C* M. Linda/GSC		12/96	Changed X and Y to reals		*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		x (*), y (*)
C
	INTEGER		isend (5)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check for zero points.
C
	IF ( np .le. 0 ) RETURN
C
C*	Check if DEVICE buffer will overflow.
C
	isnd = 2 + ( 4 + ( 2 * np ) )
	ircv = 1 + ( 1 )
	IF ( ( isnd + ircv ) .gt. IGDSIZ ) THEN
	    iret = NXDBUF
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = isnd
	isend (2) = CTICM
	isend (3) = iwndw
	isend (4) = itick
	isend (5) = np
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( size, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( np, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( x, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( y, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
