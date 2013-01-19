	SUBROUTINE HINITA ( device, cdev, iunit, filnam, itype,
     +			    xsize, ysize, ncurwn, iret )
C************************************************************************
C* HINITA - XW								*
C*									*
C* This subroutine is called to initialize the device and attributes.	*
C*									*
C* HINITA ( DEVICE, CDEV, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN,   *
C*								IRET )	*
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
C* S. Jacobs/NCEP	 9/96	Added cdev to the calling sequence	*
C* C. Lin/EAI	 	 6/97	Added 'S' coordinates			*
C*				Changed calling sequence to XINITA      *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Wang/GSC          11/97   add call to xinitclr() 			*
C* S. Jacobs/NCEP	 7/98	Removed init of txszx and txszy		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	device, cdev, filnam
C------------------------------------------------------------------------
	iret = NORMAL
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
C*	Call to initialize window.
C
	CALL ST_LSTR ( device, lend, ier )
	CALL ST_LSTR ( filnam, lenf, ier )
	CALL XINITA  ( device, lend, iunit, filnam, lenf, itype,
     +		       xsize, ysize, ixsize, iysize, isxsiz, isysiz, 
     +		       ncurwn, iret )
C
	IF ( ( iret .eq. NORMAL ) .or. ( iret .eq. NEWWIN ) ) THEN
C
	    CALL XINITCLR ( )
C
C*	    Update device attributes in /DEVCHR/.
C
	    iright = ixsize
	    ibot   = iysize
	    iswdth = isxsiz
	    ishght = isysiz
	    isxoff = 0
	    isyoff = 0
C
C*	    Clear the screen so that it will be the background color.
C
	    CALL HCLEAR ( ier )
	END IF
C*
	RETURN
	END
