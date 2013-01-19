	SUBROUTINE DLOGO ( iwndw, x, y, size, iclmod, ilogo, iret )
C************************************************************************
C* DLOGO								*
C*									*
C* This subroutine draws a specified emblem such as the NOAA seagull.	*
C*									*
C* DLOGO ( IWNDW, X, Y, SIZE, ICLMOD, ILOGO, IRET )				*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	SIZE		REAL		Emblem size			*
C*	ICLMOD		INTEGER		Emblem color mode		*
C*					    '1' - monochrome		*
C*					    '2' - color			*
C*	ILOGO		INTEGER		Emblem ID			*
C*					  '1' = NOAA			*
C*					  '2' = NWS			*
C*					  '3' = NOAA w/o text		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 7/97	Original				*
C* J. Wu/GSC		 3/01   Added emblem ID 			*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		isend ( 3 )
	REAL		rsend ( 3 )
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1 ) = 8
	isend ( 2 ) = CLOGO
	isend ( 3 ) = iwndw
C
	CALL GPUT ( isend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	rsend ( 1 ) = x
	rsend ( 2 ) = y
	rsend ( 3 ) = size
C
	CALL GPUTR ( rsend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( iclmod, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( ilogo, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
