	SUBROUTINE DDARR ( iwndw, np, x, y, dir, iret )
C************************************************************************
C* DDARR								*
C*									*
C* This subroutine draws wind arrows at points defined in any		*
C* coordinate system. The length of the arrow is proportional to	*
C* speed and its orientation is relative to local north.  If the	*
C* arrows are not plotted on a map projection, local north is		*
C* assumed to be vertical.  By convention, the direction is the		*
C* direction from which the wind is blowing.  The arrows will be	*
C* drawn using attributes defined by GSDARR.				*
C*									*
C* DDARR ( IWNDW, NP, X, Y, DIR, IRET )					*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of arrows		*
C*	X   (NP)	REAL		X coordinates in device units	*
C*	Y   (NP)	REAL		Y coordinates in device units	*
C*	DIR (NP)	REAL		Wind directions			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C* I. Durham/GSC	04/98	Changed call to VGF driver		*
C* I. Durham/GSC	04/98	Corrected call to IARRW			*
C* S. Jacobs/NCEP	 4/98	Renamed line type & width vars for check*
C* T. Lee/GSC		 9/98	Allowed I3TYP (1) to be 3 or 4		*
C* S. Jacobs/NCEP	 2/99	Added form-fitting background blank out	*
C* S. Jacobs/NCEP	 4/99	Fixed calc of bg arrow head size	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	PARAMETER	(SPEED=10.0)
C*
	REAL		x (*), y (*), dir (*)
C
	INTEGER		i3typ (3)
	REAL		spd ( LLMXPT )
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    DO i = 1, np
	       spd ( i ) = SPEED
	    END DO
	    CALL HWIND ( 3, np, x, y, spd, dir, iret )
	    RETURN
	END IF
C
C*	Break down the arrow type into three numbers corresponding to
C*	three decimal digits numbered right to left.
C
	i3typ (1) = MOD ( mdartp, 10 )
	IF ( i3typ (1) .le. 0 .or. i3typ (1) .gt. 4 ) i3typ (1) = 1
	i3typ (2) = MOD ( ( mdartp / 10 ), 10 )
	IF ( i3typ (2) .le. 0 .or. i3typ (2) .gt. 3 ) i3typ (2) = 1
	i3typ (3) = MOD ( ( mdartp / 100), 10 )
	IF ( i3typ (3) .le. 0 .or. i3typ (3) .gt. 2 ) i3typ (3) = 1
C
C*	Save line type and line width.
C
	jltyp = mltyp
	jlwid = mlwid
C
C*	Check width for form-fitting blank out flag.
C
	IF  ( mdarwd .gt. 99 )  THEN
	    kfgwid = MOD ( mdarwd, 100 )
	    IF  ( kfgwid .le. 0 )  kfgwid = 1
	    kbgwid = ( mdarwd / 100 ) + kfgwid
	    IF  ( i3typ(1) .eq. 3 )  i3typ(1) = 1
	    IF  ( i3typ(1) .eq. 4 )  i3typ(1) = 2
	  ELSE
	    kfgwid = mdarwd
	    kbgwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Compute wind arrow size.
C
	wasize = twdasz * bscala
C
C*	Compute wind arrow head size.
C
	hdsiz = tdahsz * bscalh
C
C*	Loop through arrows if size exceeds zero.
C
	IF  ( wasize .gt. 0. ) THEN
	    DO  i = 1, np
C
C*		Check for missing winds.
C
		IF  ( .not. ERMISS ( dir (i) ) ) THEN
C
C*		    Rotate wind arrow to get direction relative to
C*		    right pointing vector.
C
		    d  = 270. - dir (i)
		    ix = NINT ( x (i) )
		    iy = NINT ( y (i) )
C
C*		    If the user requests the form-fitting blank out,
C*		    draw a wider arrow in the background color.
C
		    IF  ( kbgwid .ne. 0 )  THEN
			mmcolr = mcolr
			CALL DSCOLR ( 101, imclr, ier )
			CALL DSLINE ( 1, 0, kbgwid, 0,
     +				      i1, i2, i3, i4, ier )
			IF  ( i3typ(1) .eq. 2 )  THEN
			    ahdsz = hdsiz + ( 1.0 * bscalh )
			  ELSE
			    ahdsz = hdsiz
			END IF
			CALL IARRW ( ix, iy, SPEED, d, i3typ, wasize,
     +				     ahdsz, ier )
			CALL DSCOLR ( mmcolr, imclr, ier )
		    END IF
C
C*		    Then draw the requested arrow.
C
		    CALL DSLINE ( 1, 0, kfgwid, 0,
     +				  i1, i2, i3, i4, ier )
		    CALL IARRW ( ix, iy, SPEED, d, i3typ, wasize,
     +				 hdsiz, ier )
		END IF
	    END DO
	END IF
C
C*	Restore line type and line width.
C
	IF  ( (jltyp .ne. 0) .or. (jlwid .ne. 0) )
     +	    CALL DSLINE ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C*
	RETURN
	END
