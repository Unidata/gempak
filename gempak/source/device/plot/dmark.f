	SUBROUTINE DMARK ( iwndw, np, x, y, iret )
C************************************************************************
C* DMARK								*
C*									*
C* This subroutine draws markers on the current graphics device.	*
C*									*
C* DMARK ( IWNDW, NP, X, Y, IRET )					*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of markers		*
C*	X (NP)		REAL		X coordinates in device units	*
C*	Y (NP)		REAL		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* S. Schotz/GSC	 1/90	Update for addition of marker width	*
C* M. Linda/GSC		12/96	Changed X and Y to reals		*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* S. Jacobs/NCEP	 3/99	Added check for UTF and RBK drivers	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	REAL		x (*), y (*)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HMARK ( np, x, y, iret )
	    RETURN
	END IF
C
C*	Save line type and line width.
C
	IF  ( ( mltyp .ne. 1 ) .or. ( mlwid .ne. mmkwid ) ) THEN
	    jltyp = mltyp
	    jlwid = mlwid
	    CALL DSLINE ( 1, 0, mmkwid, 0, i1, i2, i3, i4, ier )
	  ELSE
	    jltyp = 0
	    jlwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Compute marker size.
C
	size = tmksz * bscalm
C
C*	If the device is UTF or RBK, pass the marker codes and locations.
C
	IF  ( ( ddev .eq. 'UTF' ) .or. ( ddev .eq. 'RBK' ) ) THEN
	    IF  ( size .gt. 0. ) THEN
		DO  i = 1, np
		    amrk = FLOAT ( mmark )
		    CALL HSYMB ( 9, 1, amrk, x, y, 0, 0, iret )
		END DO
	    END IF
	  ELSE
C
C*	    Loop through markers if size is greater than 0.
C
	    IF  ( size .gt. 0. ) THEN
		IF  ( mmkhw .ne. 2 ) THEN
		    DO  i = 1, np
			ix = NINT ( x (i) )
			iy = NINT ( y (i) )
			CALL IMARK ( ix, iy, mmark, size, ier )
		    END DO
		  ELSE
		    CALL HMARK ( np, x, y, ier )
		END IF
	    END IF
	END IF
C
C*	Restore line type and line width.
C
	IF  ( ( jltyp .ne. 0 ) .or. ( jlwid .ne. 0 ) )
     +	    CALL DSLINE ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C*
	RETURN
	END
