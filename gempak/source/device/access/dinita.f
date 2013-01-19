	SUBROUTINE DINITA ( device, curdev, iunit, filnam, itype,
     +			    xsize, ysize, ncurwn, iret )
C************************************************************************
C* DINITA								*
C*									*
C* This subroutine is called when a new device is started.		*
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
C* S. Jacobs/NCEP	 9/96	Added curdev to calling sequence	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	device, curdev, filnam
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Initialize device and its common areas.
C
	IF  ( ( curdev .eq. 'XWP' ) .and.
     +	      ( ( device .eq. 'XWP' ) .or.
     +          ( device .eq. 'XW'  ) .or.
     +          ( device .eq. 'PS'  ) ) ) THEN
C
C*	    Do not reinitialize the common variables.
C
	  ELSE
	    CALL DINIT
	END IF
C
C*	Initialize the sub-device.
C
	CALL HINITA ( device, curdev, iunit, filnam, itype,
     +		      xsize, ysize, ncurwn, iret )
C*
	RETURN
	END
