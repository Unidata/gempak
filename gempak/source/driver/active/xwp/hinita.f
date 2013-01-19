	SUBROUTINE HINITA ( device, cdev, iunit, filnam, itype,
     +			    xsize, ysize, ncurwn, iret )
C************************************************************************
C* HINITA - XWP								*
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
C* S. Jacobs/NCEP	10/96	Added cdev and checks for sub-devices	*
C* C. Lin/EAI		 6/97	Changed calling sequence to XINITA	*
C*				Initialize 'S' coordinates for PS	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Wang/GSC          11/97   change color initializaiton sequence 	*
C*				between sub-devices			*
C* S. Jacobs/NCEP	 7/98	Removed txszx and txszy			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
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
C*	Check the requested device.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
C
C*	    Initialize device characteristics common area for specific
C*	    device.
C
	    CALL HINIT_XW  ( iret )
C
C*	    Check to make sure bscalh has been set in device driver. If
C*	    it has not, set it to a reasonable value.
C
	    IF ((bscalh .le. 0.) .or. (bscalh .gt. 1000.)) bscalh = 3.0
C
C*	    Call to initialize window.
C
	    CALL ST_LSTR ( curdev, lend, ier )
	    CALL ST_LSTR ( filnam, lenf, ier )
	    CALL XINITA  ( curdev, lend, iunit, filnam, lenf,
     +			   itype, xsize, ysize, ixsize, iysize,
     +			   isxsiz, isysiz, ncurwn, iret )
C
	    IF  ( ( iret .eq. NORMAL ) .or.
     +		  ( iret .eq. NEWWIN ) )  THEN
C
C*	    	update device attributes in /DEVCHR/.
C
	        IF  ( ddev .eq. 'XWP' .and. curdev .ne. 'XWP' )  THEN
	            CALL XINITCLR ( )
		ELSE
		    CALL XUPDCLR ( )
	   	END IF
C
		iright = ixsize
		ibot   = iysize
                iswdth = isxsiz
                ishght = isysiz
                isxoff = 0
                isyoff = 0
C
C*	    	Clear the screen so that it will be the
C*		background color.
C
		IF  ( curdev .ne. 'XWP' )  CALL HCLEAR ( ier )
	    END IF
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
C
C*	    Initialize device characteristics common area for specific
C*	    device.
C
	    CALL HINIT_PS  ( iret )
C
C*	    Check to make sure bscalh has been set in device driver. If
C*	    it has not, set it to a reasonable value.
C
	    IF ((bscalh .le. 0.) .or. (bscalh .gt. 1000.)) bscalh = 3.0
C
C*	    Initialize the PostScript output file.
C
	    CALL ST_LSTR ( filnam, lenf, ier )
	    CALL PINITA  ( iunit, filnam, lenf, itype, xsize, ysize,
     +			   ileft, ibot, iright, itop, nncolr, iret )
	    ncurwn = -1
C
C*	    colcmp = color compute flag (.true.  = color device)
C*					(.false. = monochromatic device)
C
	    IF  ( nncolr .eq. 1 )  THEN
		colcmp = .false.
C
	    ELSE
	        colcmp = .true.
C
C*	        update color bank
C
	        CALL PUPDCLR( itype )
	    END IF
C
C*	    ISPANX/Y must be computed here, because the bounds are
C*	    set in PINITA, not in HINIT.
C
	    ispanx = ISIGN ( 1, ( iright - ileft ) )
	    ispany = ISIGN ( 1, ( itop - ibot ) )
C
C*          isxoff = x offset of the 'S' coord relative to 'D' coord
C*          isyoff = y offset of the 'S' coord relative to 'D' coord
C*          iswdht = width of the 'S' coord system
C*          ishght = height of the  'S' coord system
C*          not used in PS driver 
C
            isxoff  = 0
            isyoff  = 0
            iswdth  = ABS ( iright - ileft ) 
            ishght  = ABS ( itop - ibot )

	END IF
C*
	RETURN
	END
