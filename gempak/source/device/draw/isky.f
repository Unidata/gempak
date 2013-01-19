	SUBROUTINE ISKY ( icode, ix, iy, ixoff, iyoff, size, iret )
C************************************************************************
C* ISKY									*
C*									*
C* This subroutine computes the circles and line segements needed to	*
C* produce cloud cover symbols, adapted from IMARK and IWTHR. The	*
C* sky cover symbols will always be plotted at the center postion on	*
C* a surface map. The fill pattern is a cross-hatch in the areas	*
C* specified.								*
C*									*
C* ISKY ( ICODE, IX, IY, IXOFF, IYOFF, SIZE, IRET )			*
C*									*
C* Input parameters:							*
C*	ICODE		INTEGER		Cloud cover code		*
C*	IX		INTEGER		X coordinate in device units	*
C*	IY		INTEGER		Y coordinate in device units	*
C*	IXOFF		INTEGER		X offset in half characters	*
C*	IYOFF		INTEGER		Y offset in half characters	*
C*	SIZE		REAL		Cloud symbol size multiplier	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 9/91	GEMPLT Version 5.0			*
C* K. Brill/NMC		10/91	Added offsets; shifted origin of quads	*
C* K. Brill/NMC		02/92	Changed calls to IQUAD			*
C* K. Brill/NMC		03/92	Refined symbols for size < 1		*
C* K. Brill/NMC		09/92	Adjust fill for only quarter circle	*
C* M. Linda/GSC		12/96	Added inbnds test			*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* D.W.Plummer/NCEP      2/99   Added blank background capability       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
C*
	PARAMETER	( NCIRC = 60 )
C
	INTEGER		ixp (2), iyp (2)
C
C*	Statement function to check a point to be within clipping bounds.
C
	LOGICAL		inbnds
C
	inbnds ( jx, jy ) = ( ( ispanx * (jx  - icleft) .ge. 0 ) .and.
     +			      ( ispanx * (icrght - jx ) .ge. 0 ) .and.
     +			      ( ispany * (jy  - icbot ) .ge. 0 ) .and.
     +			      ( ispany * (ictop  - jy ) .ge. 0 ))
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Compute offsets based on text character spacing.
C
	IF  ( MOD ( ixoff, 2 ) .ne. 0 ) THEN
	    ixo = ( ixoff - 1 ) / 2 * 7 + 4
	  ELSE
	    ixo = ixoff / 2 * 7
	END IF
C
	IF  ( MOD ( iyoff, 2 ) .ne. 0 ) THEN
	    iyo = ( iyoff - 1 ) / 2 * 9 + 5
	  ELSE
	    iyo = iyoff / 2 * 9
	END IF
C
C*	Calculate the symbol center location by adding the offsets.
C
	ixc = ix + ispanx * NINT ( ixo * bscalc * txsize )
	iyc = iy + ispany * NINT ( iyo * bscalc * txsize )
C
C*	Check center against clipping area; also checks if point missing.
C
	IF ( .not. inbnds ( ixc, iyc ) ) RETURN
C
C*	Set up some parameters.
C
	radcrc = size * RADSKY
	iradcr = NINT  ( radcrc + .49 )
	radcrc = FLOAT ( iradcr )
C
C*	Find the maximum and minimum x and y values on the
C*	circumference of the circle.
C
	maxx   = ixc + ispanx * NINT ( radcrc )
	maxy   = iyc + ispany * NINT ( radcrc )
	minx   = ixc - ispanx * NINT ( radcrc )
	miny   = iyc - ispany * NINT ( radcrc )
	midmxy = ( maxy + iyc ) / 2
	midmny = ( miny + iyc ) / 2
	ixmnsz = NINT ( size )
	IF ( ixmnsz .eq. 0 ) ixmnsz = 1
	ixp1   = ixc + ispanx
	iyp1   = iyc + ispany
C
        lwidsv = mskwid
        iclrsv = mcolr
C
C*      Do background first, if necessary.
C
        IF  ( mskwid .gt. 99 )  THEN
            kfgwid = MOD ( mskwid, 100 )
            IF  ( kfgwid .le. 0 )  kfgwid = 1
            kbgwid = ( mskwid / 100 ) + kfgwid
            CALL DSCOLR ( 101, imclr, ier )
	    CALL DSLINE ( 0, 0, kbgwid, 0, i1, i2, i3, i4, ier)
	    CALL ICIRC ( ixc, iyc, radcrc, NCIRC, ier )
	    CALL IQUAD ( ixc, iyc, radcrc,   0, 100, 0, 0, ier )
	    CALL IQUAD ( ixc, iyc, radcrc,  90, 190, 0, 0, ier )
	    CALL IQUAD ( ixc, iyc, radcrc, 180, 280, 0, 0, ier )
	    CALL IQUAD ( ixc, iyc, radcrc, 270, 370, 0, 0, ier )
            CALL DSCOLR ( iclrsv, imclr, ier )
	    CALL DSLINE ( 0, 0, kfgwid, 0, i1, i2, i3, i4, ier)
        END IF
C
C*	Draw the station circle. Do not fill in if CLEAR (icode=0)
C
	CALL ICIRC ( ixc, iyc, radcrc, NCIRC, ier )
C
C*	Verify that a valid cloud number has been specified.
C
	IF  ( icode .ge. 0 .and. icode .le. 9 ) THEN
C
C*	    Draw a single line in the middle of the circle.
C
	    IF  ( icode .eq. 1 ) THEN
		ixp (1) = ixc
		iyp (1) = midmxy
		ixp (2) = ixc
		iyp (2) = midmny
		CALL ILINE ( 2, ixp, iyp, ier )
	    END IF
C
C*	    Fill in the upper right quadrant.
C
	    IF  ( icode .ge. 2 .and. icode .le. 6 ) THEN
		CALL IQUAD ( ixc, iyc, radcrc, 0, 90, 1, 0, ier )
	    END IF
C
C*	    Draw an extra line to separate the bottom right quadrant.
C
	    IF  ( icode .eq. 3 ) THEN
		ixp (1) = ixp1
		iyp (1) = iyp1
		ixp (2) = ixp1
		iyp (2) = miny
		CALL ILINE ( 2, ixp, iyp, ier )
	    END IF
C
C*	    Fill in the bottom right quadrant.
C
	    IF  ( icode .ge. 4 .and. icode .le. 6 ) THEN
		CALL IQUAD ( ixc, iyc, radcrc, 270, 390, 1, 0, ier )
		ixp (1) = ixp1
		iyp (1) = iyp1
		ixp (2) = maxx
		iyp (2) = iyp1
		CALL ILINE ( 2, ixp, iyp, ier )
	    END IF
C
C*	    Draw an extra line to separate the bottom left quadrant.
C
	    IF  ( icode .eq. 5 ) THEN
		ixp (1) = ixc + ispanx
		iyp (1) = iyc
		ixp (2) = minx
		iyp (2) = iyc
		CALL ILINE ( 2, ixp, iyp, ier )
	    END IF
C
C*	    Fill in the bottom left quadrant.
C
	    IF  ( icode .eq. 6 ) THEN
		CALL IQUAD ( ixc, iyc, radcrc, 180, 300, 0, 2, ier )
		ixp (1) = ixc
		iyp (1) = iyc - 2 * ispany
		ixp (2) = ixc
		iyp (2) = miny
		CALL ILINE ( 2, ixp, iyp, ier )
	    END IF
C
C*	    Fill in the sides of the circle, leaving the center open.
C
	    IF  ( icode .eq. 7 ) THEN
		CALL IQUAD ( ixc, iyc, radcrc,  -5,  77, ixmnsz, 0, ier )
		CALL IQUAD ( ixc, iyc, radcrc, 283, 360, ixmnsz, 0, ier )
		CALL IQUAD ( ixc, iyc, radcrc, 175, 257, ixmnsz, 0, ier )
		CALL IQUAD ( ixc, iyc, radcrc, 103, 180, ixmnsz, 0, ier )
	    END IF
C
C*	    Fill in the entire circle
C
	    IF  ( icode .eq. 8 ) THEN
		CALL IQUAD ( ixc, iyc, radcrc,   0, 100, 0, 0, ier )
		CALL IQUAD ( ixc, iyc, radcrc,  90, 190, 0, 0, ier )
		CALL IQUAD ( ixc, iyc, radcrc, 180, 280, 0, 0, ier )
		CALL IQUAD ( ixc, iyc, radcrc, 270, 370, 0, 0, ier )
	    END IF
C
C*	    Draw an X in the center of the circle.
C
	    IF  ( icode .eq. 9 ) THEN
		ixp (1) = ispanx * NINT (radcrc * COS (315*DTR) ) + ixc
		iyp (1) = ispany * NINT (radcrc * SIN (315*DTR) ) + iyc
		ixp (2) = ispanx * NINT (radcrc * COS (135*DTR) ) + ixc
		iyp (2) = ispany * NINT (radcrc * SIN (135*DTR) ) + iyc
		CALL ILINE ( 2, ixp, iyp, ier)
		ixp (1) = ispanx * NINT (radcrc * COS ( 45*DTR) ) + ixc
		iyp (1) = ispany * NINT (radcrc * SIN ( 45*DTR) ) + iyc
		ixp (2) = ispanx * NINT (radcrc * COS (225*DTR) ) + ixc
		iyp (2) = ispany * NINT (radcrc * SIN (225*DTR) ) + iyc
		CALL ILINE ( 2, ixp, iyp, ier)
	    END IF
	  ELSE
C
C*	    If missing draw an M.
C
	    ixp (1) = ixc - ispanx * ixmnsz
	    iyp (1) = ispany * NINT (radcrc * SIN (225*DTR) ) + iyc
	    ixp (2) = ixc - ispanx * ixmnsz
	    iyp (2) = ispany * NINT (radcrc * SIN (135*DTR) ) + iyc
	    CALL ILINE ( 2, ixp, iyp, iret )
	    ixp (1) = ixc
	    iyp (1) = iyc
	    CALL ILINE ( 2, ixp, iyp, iret )
	    ixp (1) = ixc + ispanx * ixmnsz
	    iyp (1) = ispany * NINT (radcrc * SIN (315*DTR) ) + iyc
	    ixp (2) = ixc + ispanx * ixmnsz
	    iyp (2) = ispany * NINT (radcrc * SIN ( 45*DTR) ) + iyc
	    CALL ILINE ( 2, ixp, iyp, iret )
	    ixp (1) = ixc
	    iyp (1) = iyc
	    CALL ILINE ( 2, ixp, iyp, iret )
C
	END IF
C
	mskwid = lwidsv
C*
	RETURN
	END
