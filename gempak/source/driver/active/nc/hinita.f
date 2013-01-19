	SUBROUTINE HINITA ( dev, cdev, iunit, filnam, itype,
     +			    xsize, ysize, ncurwn, iret )
C************************************************************************
C* HINITA - NC								*
C*									*
C* This subroutine is called to initialize the device and attributes.	*
C*									*
C* HINITA ( DEV, CDEV, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN,	*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C*	DEV		CHAR*		Device name			*
C*	CDEV            CHAR*           Current device name		*
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
C* S. Jacobs/NCEP	 5/96	Added iunit to MINITA call		*
C* S. Jacobs/NCEP	 9/96	Added cdev to the calling sequence	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* R. Tian/SAIC          4/02   Moved init of 'S' coord from HINIT      *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	dev, cdev, filnam
C------------------------------------------------------------------------
	iret = NORMAL
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
	CALL HINIT  ( iret )
C
C*	Check to make sure bscalh has been set in device driver. If 
C*	it has not, set it to a reasonable value.
C
	IF  ((bscalh .le. 0.) .or. (bscalh .gt. 1000.)) bscalh = 3.0
C
C*	Call to initialize metafile.
C
	CALL ST_LSTR ( filnam, lenf, ier )
	CALL MINITA ( iunit, filnam, lenf, itype, xsize, ysize, 
     +		      ileft, ibot, iright, itop, iret )
C
C*      isxoff = x offset of the 'S' coord relative to 'D' coord
C*      isyoff = y offset of the 'S' coord relative to 'D' coord
C*      iswdht = width of the 'S' coord system
C*      ishght = height of the 'S' coord system
C
        isxoff  = 0
        isyoff  = 0
        iswdth  = iright - ileft
        ishght  = itop - ibot
C*
	RETURN
	END
