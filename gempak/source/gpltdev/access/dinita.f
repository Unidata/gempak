	SUBROUTINE DINITA ( device, curdev, iunit, filnam, itype,
     +			    xsize, ysize, ncurwn, iret )
C************************************************************************
C* DINITA								*
C*									*
C* This subroutine starts a new device driver.  If the subprocess is	*
C* started, the device characteristics are returned.			*
C*									*
C* DINITA ( DEVICE, CURDEV, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE,		*
C*	    NCURWN, IRET )						*
C*									*
C* Input parameters:							*
C* 	DEVICE		CHAR*		Device name			*
C* 	CURDEV		CHAR*		Device name			*
C* 	IUNIT		INTEGER		Type of output device		*
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
C*	IRET		INTEGER 	Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 3/96	DINITA based on DINITD			*
C* S. Jacobs/NCEP	 9/96	Added curdev and check for XWP		*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'ADBUFF.CMN'
C*
	CHARACTER*(*)	device, curdev, filnam
C
	CHARACTER	cdev*12, rdev*12, cfil*72
	INTEGER		isend(28)
	REAL		rsend(2)
	DATA		isend/28*0/, rsend/2*0.0/
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Start the subprocess.
C
	IF  ( ( curdev .eq. 'XWP' ) .and.
     +	      ( ( device .eq. 'XWP' ) .or.
     +	        ( device .eq. 'XW'  ) .or.
     +		( device .eq. 'PS'  ) ) )  THEN
C
C*	    Do not start a new process.
C
	  ELSE
	    mproc = 1
	    CALL GSPROC ( mproc, device, mbchan, istat, iret )
	END IF
C
C*	Return if error, else initialize /ADBUFF/.
C
	IF ( iret .ne. NORMAL ) THEN
	    iret = NODEVC
	    RETURN
	ELSE
	    ntypsr = -1
	    irtype = 2
	    iwtype = 1
	END IF
C
C*	Convert text strings into integer arrays.
C
	cdev = device (1:12)
	CALL ST_STOI ( cdev, 12, nv, isend (3), ier )
C
	rdev = curdev (1:12)
	CALL ST_STOI ( rdev, 12, nv, isend (6), ier )
C
	cfil = filnam (1:72)
	CALL ST_STOI ( cfil, 72, nv, isend (10), ier )
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1) = 30
	isend ( 2) = CINITA
	isend ( 9) = iunit
	isend (28) = itype
C
	CALL GPUT ( isend, 28, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	rsend (1) = xsize
	rsend (2) = ysize
C
	CALL GPUTR ( rsend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	    RETURN
	END IF
C
	CALL GGET ( ncurwn, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
