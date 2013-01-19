	SUBROUTINE HINITA ( device, cdev, iunit, filnam, itype,
     +			    xsize, ysize, ncurwn, iret )
C************************************************************************
C* HINITA - VG								*
C*									*
C* This subroutine is called to initialize the device and attributes.	*
C*									*
C* HINITA ( DEVICE, CDEV, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN,	*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C*	DEVICE		CHAR*		Device name			*
C*	CDEV		CHAR*		Current device name		*
C*	IUNIT		INTEGER		Type of output device		*
C*					  For XW:			*
C*					    1 = GEMPAK window		*
C*					    2 = Motif window		*
C*	FILNAM		CHAR*		File name or window name	*
C*	ITYPE		INTEGER		Device color capability		*
C*	XSIZE		REAL		Width in inches or pixels	*
C*	YSIZE		REAL		Height in inches or pixels	*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/96	HINITA based on HINITD			*
C* S. Jacobs/NCEP	 4/96	Updated return code			*
C* S. Jacobs/NCEP	10/96	Added cdev to the calling sequence	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* T. Piper/SAIC	02/04	Calling sequence change for vinita	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	device, cdev, filnam
C*
	CHARACTER	fnm*132
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C
C*	Save device in common area.
C
	ddev   = device
	curdev = cdev
	niunit = iunit
C
C*	Initialize device characteristics common area for specific
C*	device.
C
	CALL HINIT  ( iret )
C
C*	Check to make sure bscalh has been set in device driver. If
C*	it has not, set it to a reasonable value.
C
	IF ((bscalh .le. 0.) .or. (bscalh .gt. 1000.)) bscalh = 3.0
C
C*	Initialize the VGF output file.
C
	CALL ST_NULL ( filnam, fnm, lenf, ier )
	CALL VINITA ( iunit, fnm, lenf, itype, iret )
C*
	RETURN
	END
