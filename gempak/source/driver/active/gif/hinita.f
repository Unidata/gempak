	SUBROUTINE HINITA  ( dev, cdev, iunit, fname, itype, 
     +			     xsize, ysize, ncurwn, iret )
C************************************************************************
C* HINITA - GIF 							*
C*									*
C* This subroutine is called to initialize the device and attributes.	*
C*									*
C* HINITA ( DEV, CDEV, IUNIT, FNAME, ITYPE, XSIZE, YSIZE, NCURWN, IRET )*
C*									*
C* Input parameters:							*
C*	DEV		CHAR*		Device name			*
C*	CDEV		CHAR*		Current device name             *
C*	IUNIT		INTEGER		Unit number (not used)		*
C*	FNAME		CHAR*		File name of output		*
C*	ITYPE		INTEGER		Pixel type			*
C*					  0 = Monochrome		*
C*					  1 = Grayscale			*
C*					  2 = Color			*
C*	XSIZE		REAL		Width in inches or pixels	*
C*	YSIZE		REAL		Height in inches or pixels	*
C*                                                                      *
C* Output parameters:							*
C*      NCURWN          INTEGER         Current window number		* 
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Austin		 5/96						*
C* J. Nielsen-G/TAMU	 5/98	Adapted from hinitd.f following xw	*
C* T. Lee/GSC		 7/00	Added cdev to the calling sequence	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'driver.cmn'
C
	CHARACTER*(*)	dev, cdev, fname
C------------------------------------------------------------------------
	iret = NORMAL
	ncurwn = 0
C
C*	Save device in common area.
C
	ddev   = dev
	curdev = cdev
	niunit = iunit
C
C*      Initialize device characteristics common area for specific
C*      device.
C
        CALL HINIT  ( iret )
C
C*	Set default name for gif output file. 
C
	CALL ST_RMBL ( fname, filnam, lf, iret )
	IF ( filnam .eq. ' ' ) filnam = 'gempak.gif'
	fname = filnam
	opnfil = .false.
	gfplot = .false.
C
	IF ( ( xsize .GT. 1 ) .AND. ( xsize .LT. 5001 )
     +	     .AND. ( ysize .GT. 1 ) .AND. ( ysize .LT. 5001 ) )  THEN
	  iright = NINT (xsize) - 1
	  ibot   = NINT (ysize) - 1
	ELSE
	  iright = 639
	  ibot   = 479
	END IF
C
C*	Set color bank.
C
	CALL HSCINT  ( iret )
C*
	RETURN
	END
