	SUBROUTINE DTURB ( iwndw, np, tucod, x, y, ixoff, iyoff, iret )
C************************************************************************
C* DTURB								*
C*									*
C* This subroutine draws turbulence symbols on the current graphics	*
C* device.								*
C*									*
C* DTURB ( IWNDW, NP, TUCOD, X, Y, IXOFF, IYOFF, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of symbols		*
C*	TUCOD (NP)	REAL		Turbulence symbol codes		*
C*	X     (NP)	REAL		X coordinates in device units	*
C*	Y     (NP)	REAL		Y coordinates in device units	*
C*	IXOFF (NP)	INTEGER		X offsets in half characters	*
C*	IYOFF (NP)	INTEGER		Y offsets in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on DWTHR				*
C* M. Linda/GSC		12/96	Changed X, Y, and symbol code to reals	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* S. Jacobs/NCEP	 3/97	Added values to the symbol codes;	*
C*				Added ability to have 2 syms at a time	*
C* S. Jacobs/NCEP	 3/97	Fixed offset of text when entire symbol	*
C*				is offset				*
C* S. Jacobs/NCEP	 4/97	Removed values from symbol codes	*
C* S. Jacobs/NCEP	 8/97	Added check for UTF driver		*
C* W. Li/EAI		12/97	Fixed UTF driver turb. display problem	*
C* A. Hardy/GSC          9/98   Added check for RBK driver              *
C* J. Wu/SAIC            8/03   Added proper space based on font size	*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	REAL		tucod (*), x (*), y (*)
	INTEGER		ixoff (*), iyoff (*)
C*
	REAL		scod (2), sx (2), sy (2)
	INTEGER		ixof (2), iyof (2)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSYMB ( 8, np, tucod, x, y, ixoff, iyoff, iret )
	    RETURN
	END IF
C
C*	If the driver is UTF or RBK, separate the two possible turbulence
C*	symbols in the code and call HSYMB.
C
C*	Save line type and line width.
C
	IF  ( ( mltyp .ne. 1 ) .or. ( mlwid .ne. mtuwid ) ) THEN
	    jltyp = mltyp
	    jlwid = mlwid
	    CALL DSLINE ( 1, 0, mtuwid, 0, i1, i2, i3, i4, ier )
	  ELSE
	    jltyp = 0
	    jlwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Compute symbol size.
C
	wsize = ttursz * bscalw
C
C*	For turbs consisting of two symbols, make the space between
C*      two symbols proportional to the font size.
C
	ratio = 1.5
	IF ( ttursz .lt. 0.8 ) THEN
	    ratio = 2.2
	  ELSE IF ( ttursz .gt. 0.8 .and. ttursz .lt. 1.0 ) THEN
	    ratio = 1.8
	  ELSE IF ( ttursz .gt. 1.0 .and. ttursz .lt. 1.5 ) THEN
	    ratio = 1.2
	  ELSE IF ( ttursz .gt. 1.5 .and. ttursz .lt. 2.0 ) THEN
	    ratio = 0.9		
	  ELSE IF ( ttursz .gt. 2.0 ) THEN
	    ratio = .65		
	END IF
C
	IF  ( ( ddev .eq. 'UTF' ) .or. ( ddev .eq. 'RBK' ) ) THEN
	    DO  i = 1, np
		IF  ( tucod(i) .lt. 10 )  THEN
		    CALL HSYMB ( 8, 1, tucod(i), x(i), y(i),
     +				 ixoff(i), iyoff(i), ier )
		  ELSE
		    scod(1) = NINT ( tucod (i) ) / 10 - 1
		    ixof(1) = ixoff(i) - NINT ( ttursz * ratio )
		    iyof(1) = iyoff(i)
		    sx(1)   = x(i)
		    sy(1)   = y(i)
		    scod(2) = MOD ( NINT ( tucod (i) ), 10 )
		    ixof(2) = ixoff(i) + NINT ( ttursz * ratio )
		    iyof(2) = iyoff(i)
		    sx(2)   = x(i)
		    sy(2)   = y(i)
		    CALL HSYMB ( 8, 2, scod, sx, sy, ixof, iyof, ier )
		END IF
	    END DO
	    RETURN
	END IF
C
C*	Loop through symbols.
C
	DO  i = 1, np
C
C*	    The symbol is coded as ABCCCDDD, where A is the left symbol
C*	    code plus one, B is the right symbol code, CCC is the
C*	    top flight level value and DDD is the bottom flight level
C*	    to be plotted. If A is zero, then symbol B is plotted in
C*	    the center. The values in CCC and DDD are plotted below
C*	    the symbol(s) with a "/" is inserted between the values.
C*	    If DDD is missing, CCC is plotted with a "/" appended to 
C*	    the value.
C
	    itusy  = MOD ( NINT ( tucod (i) ), 10 )
	    itusy2 =       NINT ( tucod (i) ) / 10
C
C*	    Plot the symbol.
C
	    ix = NINT ( x (i) )
	    iy = NINT ( y (i) )
	    IF  ( itusy2 .eq. 0 )  THEN
		CALL ITURB ( itusy, ix, iy, ixoff(i), iyoff(i),
     +			     wsize, ier )
	      ELSE
		ixo = ixoff (i) - NINT ( ttursz * ratio )
		CALL ITURB ( itusy2-1, ix, iy, ixo, iyoff(i),
     +			     wsize, ier )
		ixo = ixoff (i) + NINT ( ttursz * ratio )
		CALL ITURB ( itusy, ix, iy, ixo, iyoff(i),
     +			     wsize, ier )
	    END IF
	END DO
C
C*	Restore line type and line width.
C
	IF  ( ( jltyp .ne. 0 ) .or. ( jlwid .ne. 0 ) )
     +	    CALL DSLINE ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C*
	RETURN
	END
