	SUBROUTINE HINITA  ( dev, cdev, iunit, filnam, itype,
     +			     xsize, ysize, ncurwn, iret )
C************************************************************************
C* HINITA - UTF								*
C*									*
C* This subroutine is called to initialize a new device driver.		*
C*									*
C* HINITA  ( DEV, CDEV, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	DEV		CHAR*		Device name			*
C*	CDEV            CHAR*           Current device name		*
C*	IUNIT		INTEGER		Type of output device		*
C*					  (not used for UTF)	        *
C*	FILNAM		CHAR*		File name of output		*
C* 	ITYPE		INTEGER		Output file format		*
C*					    2 = NAFOS format		*
C*					  <>2 = AFOS format		*
C*	XSIZE		REAL		X size in pixels		*
C*	YSIZE		REAL		Y size in pixels		*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*					  (not used for UTF)		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	11/96	Initial Coding				*
C* C. Lin/EAI            6/97   Initialize 'S' coordinates 		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 8/97	Changes ST_LSTR to ST_NULL		*
C* S. Jacobs/NCEP	10/97	Added base time to UINITA call		*
C************************************************************************
	INCLUDE         'ERROR.PRM'
	INCLUDE         'DVWNDW.CMN'
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'DEVACT.CMN'
C*
	CHARACTER*(*)	dev, cdev, filnam
C*
	CHARACTER	carr(2)*160, fname*160
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
C*	Initialize the UTF output file.
C
	CALL ST_CLST ( filnam, ';', ' ', 2, carr, num, ierr )
	CALL ST_NULL ( carr(1), fname, lenf, ier )
	CALL ST_NUMB ( carr(2), ibase, ier )
	CALL UINITA ( iunit, fname, lenf, itype, ibase, xsize, ysize,
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
C*	Do not change anything below this line.
C------------------------------------------------------------------------
	ispanx = ISIGN ( 1, ( iright - ileft ) )
	ispany = ISIGN ( 1, ( itop - ibot ) )
C*
	RETURN
	END
