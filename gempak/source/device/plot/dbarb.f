	SUBROUTINE DBARB ( iwndw, np, x, y, spd, dir, iret )
C************************************************************************
C* DBARB								*
C*									*
C* This subroutine draws wind barbs on the current graphics device.	*
C*									*
C* DBARB ( IWNDW, NP, X, Y, SPD, DIR, IRET )				*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of barbs			*
C*	X   (NP)	REAL		X coordinates in device units	*
C*	Y   (NP)	REAL		Y coordinates in device units	*
C*	SPD (NP)	REAL		Wind speeds			*
C*	DIR (NP)	REAL		Wind directions			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* S. Schotz/GSC	 1/90	Added barb width			*
C* S. Schotz/GSC	 5/90	Added option for not drawing barb for	*
C*				calm wind				*
C* K. Brill/NMC		11/91	Move all barb type checking to IBARB	*
C* K. Brill/NMC		11/91	Assign and pass I3TYP to IBARB		*
C* S. Jacobs/EAI	 6/92	Added check for missing winds		*
C* S. Jacobs/NMC	 8/94	Added check for S. Hemisphere data	*
C* D.W.Plummer/NCEP	 9/96	Added flip option check in type		*
C* M. Linda/GSC		12/96	Changed X and Y to reals		*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* T. Lee/GSC		 8/98	Allowed I3TYP (1) to be 3 or 4		*
C* S. Jacobs/NCEP	 2/99	Added form-fitting background blank out	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	REAL		x (*), y (*), spd (*), dir (*)
C
	INTEGER		i3typ (3)
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*      If the driver is VG, send the points directly to the device
C*      for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HWIND ( 1, np, x, y, spd, dir, iret )
	    RETURN
	END IF
C
C*	Break down the barb type into four numbers corresponding to
C*	four decimal digits numbered right to left.  Save only the
C*	first (rightmost) three to pass on to IBARB routine.
C*	The fourth (leftmost) digit is a flip option whereby the wind
C*	barb is reversed (aka the southern hemisphere) from what
C*	it normally should be.
C
	i3typ (1) = MOD ( mbrtyp, 10 )
	IF ( i3typ (1) .le. 0 .or. i3typ (1) .gt. 4 ) i3typ (1) = 1
	i3typ (2) = MOD ( ( mbrtyp / 10  ), 10 )
	IF ( i3typ (2) .le. 0 .or. i3typ (2) .gt. 3 ) i3typ (2) = 1
	i3typ (3) = MOD ( ( mbrtyp / 100 ), 10 )
	IF ( i3typ (3) .le. 0 .or. i3typ (3) .gt. 2 ) i3typ (3) = 1
C
C*	Save flip option as a sign multiplier to be used later.
C
	iflip     = MOD ( ( mbrtyp / 1000), 10 )
	IF ( iflip .ne. 0 ) THEN
		iflip = -1
	ELSE
		iflip = 1
	END IF
C
C*	Save line type and line width.
C
	jltyp = mltyp
	jlwid = mlwid
C
C*	Check width for form-fitting blank out flag.
C
	IF  ( mbrwid .gt. 99 )  THEN
	    kfgwid = MOD ( mbrwid, 100 )
	    IF  ( kfgwid .le. 0 )  kfgwid = 1
	    kbgwid = ( mbrwid / 100 ) + kfgwid
	    IF  ( i3typ(1) .eq. 3 )  i3typ(1) = 1
	    IF  ( i3typ(1) .eq. 4 )  i3typ(1) = 2
	  ELSE
	    kfgwid = mbrwid
	    kbgwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Compute wind barb size.
C
	wbsize = twbsz * bscalb
C
C*	Loop through barbs if size is greater than 0.
C
	IF  ( wbsize .gt. 0. ) THEN
	    DO  i = 1, np
C
C*		Check for missing winds.
C
		IF  ( .not. ERMISS ( spd (i) ) .and.
     +		      .not. ERMISS ( dir (i) ) ) THEN
C
C*		    Rotate wind barb to get direction relative to
C*		    right pointing vector.
C
		    d   = 90. - dir (i)
C
C*		    Check for southern hemisphere data.
C
		    ihs = iflip * ISIGN ( 1, NINT ( spd (i) ) )
		    s   = ABS ( spd (i) )
		    ix  = NINT ( x (i) )
		    iy  = NINT ( y (i) )
C
C*		    If the user requests the form-fitting blank out,
C*		    draw a wider barb in the background color.
C
		    IF  ( kbgwid .ne. 0 )  THEN
			mmcolr = mcolr
			CALL DSCOLR ( 101, imclr, ier )
			CALL DSLINE ( 1, 0, kbgwid, 0,
     +				      i1, i2, i3, i4, ier )
			CALL IBARB ( ix, iy, s, d, ihs, i3typ,
     +				     wbsize, ier )
			CALL DSCOLR ( mmcolr, imclr, ier )
		    END IF
C
C*		    Then draw the requested barb.
C
		    CALL DSLINE ( 1, 0, kfgwid, 0,
     +				  i1, i2, i3, i4, ier )
		    CALL IBARB ( ix, iy, s, d, ihs, i3typ, wbsize, ier )
		END IF
	    END DO
	END IF
C
C*	Restore line type and line width.
C
	IF  ( ( jltyp .ne. 0 ) .or. ( jlwid .ne. 0 ) )
     +	    CALL DSLINE  ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C*
	RETURN
	END
