	SUBROUTINE GSDEVA ( device, iunit, filnam, itype, xsize, ysize,
     +			    iret )
C************************************************************************
C* GSDEVA								*
C* 									*
C* This subroutine sets the plot device.  If another device is in use   *
C* when it is called, GSDEVA terminates plotting on that device, then   *
C* starts the device subprocess for the requested device.		*
C*									*
C* GSDEVA ( DEVICE, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, IRET )		*
C*									*
C* Input parameters:							*
C* 	DEVICE		CHAR*		Plot device name		*
C* 	IUNIT		INTEGER		Type of output device		*
C*					  For XW:			*
C*					    1 = GEMPAK window		*
C*					    2 = Motif window		*
C*	FILNAM		CHAR*		File name or window name	*
C*	ITYPE		INTEGER		Device color capability		*
C*	XSIZE		REAL		Width in inches or pixels	*
C*	YSIZE		REAL		Height in inches or pixels	*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 3/96	GSDEVA based on GSDEV			*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	device, filnam
C
	CHARACTER	dev*12, fil*72
	INTEGER		isend (25)
	REAL		rsend (2)
C------------------------------------------------------------------------
C
C*	Store text strings into integer arrays.
C
	dev = device (1:12)
	CALL ST_STOI ( dev, 12, nv, isend (3), ier )
C
	fil = filnam (1:72)
	CALL ST_STOI ( fil, 72, nv, isend (7), ier )
C
C*	Load input parameters into message queue buffer.
C
	isend ( 1) = 27
	isend ( 2) = FSDEVA
	isend ( 6) = iunit
	isend (25) = itype
C
C*	Send message queue buffer.
C
	CALL GPUT ( isend, 25, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	rsend (1) = xsize
	rsend (2) = ysize
C
	CALL GPUTR ( rsend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get return code from message queue.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
