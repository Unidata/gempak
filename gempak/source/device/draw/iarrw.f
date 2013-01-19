	SUBROUTINE IARRW  ( ixx, iyy, spd, dir, i3typ, size, hdsiz,
     +			    iret )
C************************************************************************
C* IARRW								*
C*									*
C* This subroutine computes the line segments necessary to draw an	*
C* arrow.  The directions are relative to a right pointing vector.	*
C* Note that this direction is not the same as the conventional wind	*
C* direction.  Wind directions must be rotated before using this	*
C* subroutine.								*
C*									*
C* IARRW  ( IXX, IYY, SPD, DIR, I3TYP, SIZE, HDSIZ, IRET )		*
C*									*
C* Input parameters:							*
C*	IXX		INTEGER		X coordinate in device units	*
C*	IYY		INTEGER		Y coordinate in device units	*
C*	SPD		REAL		Magnitude of vector		*
C*	DIR		REAL		Direction of vector		*
C*	I3TYP (3)	INTEGER		Type parameters for arrows	*
C*					I3TYP (1) => fill flag		*
C*					I3TYP (2) => position flag	*
C*					I3TYP (3) => calm wind flag	*
C*	SIZE		REAL		Length in device coordinates	*
C*	HDSIZ		REAL		Arrow head length in device	*
C*					units				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 2/82						*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 5/90	Added arrow head size to parameter list *
C* K. Brill/NMC		11/91	Changes for new arrow types		*
C* M. Linda/GSC		12/96	Added inbnds test, removed aspect	*
C* S. Jacobs/NCEP	 4/98	Replaced ITRIF with IFILL		*
C* T. Lee/GSC		 8/98	Added fill box				*
C* A. Hardy/GSC          9/98   Added RBK device driver                 *
C* S. Jacobs/NCEP	 2/99	Fixed head location			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		i3typ (3), jxo (5), jyo (5)
	INTEGER		ixp (2), iyp (2), ihdx (3), ihdy (3)
	REAL		xoff (5), yoff (5)
C
C*	HANGLE is the arrow head angle in radians.
C
	PARAMETER	( HANGLE = DTR * 150. )
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
C*	Compute radius of sky cover circles.
C
	scyrad = tskysz * bscalc * RADSKY
C
C*	Compute length of arrow.
C
	arrwln = size * spd
C
C*	Check wind speed.  If less than .5, do nothing or draw arrow
C*	head only.
C
	IF ( spd .lt. 0.5 .and. i3typ (3) .eq. 2 ) THEN
	    RETURN
C
	ELSE IF ( spd .lt. 0.5 .and . i3typ (3) .eq. 1 ) THEN
	    arrwln = 0.0
	END IF
C
C*	Check for centered or sky cover shifted arrow.
C
	IF ( i3typ (2) .eq. 3 ) THEN
	    apstn = scyrad
	ELSE IF ( i3typ (2) .eq. 2 ) THEN
	    apstn = (-0.5) * arrwln
	ELSE
	    apstn = 0.0
	END IF
C
C*	Compute sine and cosine of direction.
C
	rdir    = dir * DTR
	xcos    = COS ( rdir )
	xsin    = SIN ( rdir )
C
C*	First point is base of arrow.
C
	ix = ixx + ispanx * NINT ( apstn * xcos )
	iy = iyy + ispany * NINT ( apstn * xsin )
C
C*	Check base against clipping area; also checks if point missing.
C
	IF ( .not. inbnds ( ix, iy ) ) RETURN
C
	ixp (1) = ix
	iyp (1) = iy
C
C*	Compute other end of shaft.
C*	Note that the direction of increasing x and y coordinates is
C*	accounted for in /DVWNDW/ as ispanx and ispany.
C
	xn      = arrwln * xcos
	yn      = arrwln * xsin
	ixp (2) = ix + ispanx * NINT ( xn )
	iyp (2) = iy + ispany * NINT ( yn )
C
C*	Compute extra space around the arrow.  Consider the case when
C*	arrow head is longer than shaft length.
C
	extra = 1.5 * size
	hcos  = COS ( HANGLE )
	hsin  = SIN ( HANGLE )
	hlen  = hdsiz * ABS ( hcos )
	hdx   = 0.
	hdx2  = 0.
	IF ( HANGLE .ge. HALFPI )  THEN
	    IF ( hlen .gt. arrwln ) hdx = hlen - arrwln
	  ELSE
	    hdx2 = hlen
	END IF
	hdy = hdsiz * ABS ( hsin )
C
C*	Draw a fill box with the background color.
C
	IF  ( i3typ(1) .eq. 1 .or. i3typ(1) .eq. 3 )  THEN
	    arrwid = arrwln
	  ELSE
	    arrwid = arrwln + ( 0.75 * hlen )
	END IF
C
	np = 5
	IF  ( i3typ (1) .eq. 3 .or. i3typ (1) .eq. 4 )  THEN
	    xoff(1) = - ( extra + hdx )
	    yoff(1) = - ( extra + hdy )
	    xoff(2) = xoff (1)
	    yoff(2) = - yoff(1)
C
	    xoff(3) = extra + arrwid + hdx2
	    yoff(3) = yoff (2)
	    xoff(4) = xoff(3)
	    yoff(4) = yoff(1)
	    xoff(5) = xoff(1)
	    yoff(5) = yoff(1)
C
	    DO i = 1, np
		xprimd = xoff (i) * xcos - yoff (i) * xsin
		yprimd = xoff (i) * xsin + yoff (i) * xcos
		jxo (i) = ixp (1) + NINT ( ispanx * xprimd )
		jyo (i) = iyp (1) + NINT ( ispany * yprimd )
	    END DO
C
	    mmcolr = mcolr
	    mmfltyp = mfltyp
	    CALL DSCOLR ( 101, imclr, ier )
	    CALL DSFILL ( 0., 1, fsiz, jtyp, ier )
	    CALL IFILL  ( np, jxo, jyo, ier )
	    CALL DSCOLR ( mmcolr, imclr, ier )
	    CALL DSFILL ( 0., mmfltyp, fsiz, jtyp, ier )
	END IF
C
C*	Draw the arrow shaft.
C
	CALL ILINE  ( 2, ixp, iyp, ier )
C
C*	Compute the center point of the arrow head. Make sure that
C*	the end of the shaft does not extend beyond the arrow head
C*	by adding the line width to the length.
C
	xn       = arrwid * xcos
	yn       = arrwid * xsin
	ihdx (2) = ix + ispanx * NINT ( xn )
	ihdy (2) = iy + ispany * NINT ( yn )
C
C*	Compute one side of arrow head.
C
	rang1   = rdir - HANGLE
	ihdx (1) = NINT ( hdsiz * cos (rang1) + xn ) * ispanx + ix
	ihdy (1) = NINT ( hdsiz * sin (rang1) + yn ) * ispany + iy
C
C*	Compute other side of arrow head.
C
	rang2   = rdir + HANGLE
	ihdx (3) = NINT ( hdsiz * cos (rang2) + xn ) * ispanx + ix
	ihdy (3) = NINT ( hdsiz * sin (rang2) + yn ) * ispany + iy
C
C*	Fill the head if required, always use a solid fill.
C*	Do not fill the arrow head if the device is UTF or RBK.
C
	IF  ( ( i3typ (1) .eq. 2 .or. i3typ (1) .eq. 4 ) .and. 
     +	      ( ( ddev .ne. 'UTF' ) .and. ( ddev .ne. 'RBK' ) ) ) THEN
	    msvft = mfltyp
	    svfsz = tfilsz
	    CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	    CALL IFILL  ( 3, ihdx, ihdy, ier )
	    CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
	  ELSE
C
C*	    Draw the outside of the arrow head using lines.
C
	    CALL ILINE  ( 3, ihdx, ihdy, ier )
	END IF
C*
	RETURN
	END
