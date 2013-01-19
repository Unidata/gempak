	SUBROUTINE HINITA  ( dev, cdev, iunit, filnam, itype,
     +			     xsize, ysize, ncurwn, iret )
C************************************************************************
C* HINITA - FAX								*
C*									*
C* This subroutine is called to initialize a new device driver.		*
C*									*
C* HINITA  ( DEV, CDEV, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	DEV		CHAR*		Device name			*
C*	CDEV		CHAR*		Current device name		*
C*	IUNIT		INTEGER		Type of output device		*
C*					  (not used for PostScript)	*
C*	FILNAM		CHAR*		File name of output		*
C* 	ITYPE		INTEGER		Pixel type			*
C*					  0 = Monochrome		*
C*					  1 = Grayscale			*
C*					  2 = Color			*
C*	XSIZE		REAL		X Paper size			*
C*	YSIZE		REAL		Y paper size			*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*					  (not used for PostScript)	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAi	 7/96	Adopted to handle raster driver		*
C* S. Jacobs/NCEP	 9/96	Added cdev to the calling sequence	*
C* E. Wehner/EAi	 1/97	Changed scaling factors			*
C* E. Wehner/Eai	 3/97	Added span calculations from HINIT	*
C* C. Lin/EAI		 6/97	Initialize 'S' coordinates for FAX	*
C* S. Maxwell/GSC        6/97	Documentation changes			*
C* S. Jacobs/NCEP	 6/97	Removed scale factors settings		*
C* S. Jacobs/NCEP	 7/97	Changed colcmp to false			*
C* T. Piper/SAIC	02/04	Calling sequence change to rinita	*
C************************************************************************
	INCLUDE         'ERROR.PRM'
	INCLUDE         'DVWNDW.CMN'
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'DEVACT.CMN'
C*
	CHARACTER*(*)	dev, cdev, filnam
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C
C*	Save device common area.
C
	ddev   = dev
	curdev = cdev
	niunit = iunit
C
C*	Initialize device characteristics common area for specific
C*	device.
C
	CALL HINIT ( iret)
C
C*	Check to make sure bscalh has been set in device driver. If
C*	it has not, set it to a reasonable value.
C
	IF  ((bscalh .le. 0.) .or. (bscalh .gt. 1000.)) bscalh = 3.0
C
C*	Initialize the output file.
C
	CALL ST_NULL ( filnam, filnam, lenf, ier )
	CALL RINITA ( filnam, xsize, ysize,
     +		      ileft, ibot, iright, itop, nncolr, iret )
C
C*      isxoff = x offset of the 'S' coord relative to 'D' coord
C*      isyoff = y offset of the 'S' coord relative to 'D' coord
C*      iswdht = width of the 'S' coord system
C*      ishght = height of the  'S' coord system
C*      not used in PS driver
C
        isxoff  = 0
        isyoff  = 0
        iswdth  = ABS ( iright - ileft )
        ishght  = ABS ( itop - ibot )
C
C*	colcmp = color compute flag ( .true.  = color device )
C*				    ( .false. = monochromatic device )
C
	colcmp = .false.
C*
C*	Do not change anything below this line.
C------------------------------------------------------------------------
	ispanx = ISIGN ( 1, ( iright - ileft ) )
	ispany = ISIGN ( 1, ( itop - ibot ) )
C
	CALL RSPAN ( ispanx, ispany, ier)
C*
	RETURN
	END
