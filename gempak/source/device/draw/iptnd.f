      SUBROUTINE IPTND ( icode, ix, iy, ixoff, iyoff, size, iret )
C************************************************************************
C* IPTND								*
C*									*
C* This subroutine draws the pressure tendency symbol corresponding	*
C* to the code number.							*
C*									*
C* IPTND ( ICODE, IX, IY, IXOFF, IYOFF, SIZE, IRET )			*
C*									*
C* Input parameters:							*
C*	ICODE		INTEGER		Pressure tendency code		*
C*	IX		INTEGER		X coordinate in device units	*
C*	IY		INTEGER		Y coordinate in device units	*
C*	IXOFF		INTEGER		X coordinate offset		*
C*	IYOFF		INTEGER		Y coordinate offset		*
C*	SIZE		REAL		Pressure tendency symbol size	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/NMC		11/91						*
C* K. Brill/NMC		02/92	RETURN for invalid symbol code		*
C* M. Linda/GSC		 9/96	Changed offsets logic for consistency	*
C* M. Linda/GSC		12/96	Added inbnds test, removed aspect	*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		ixp (3), iyp (3)
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
C*	Compute length of short segment.
C
	sssize = .4 * size
C
C*	Set long segment orientation angle and
C*	short segment angle measured from +x axis.
C
	hafsiz = .5 * size
	IF ( icode .eq. 0 ) THEN
	    rdir   = 50.
	    hangle = -50.
	ELSE IF ( icode .eq. 1 ) THEN
	    rdir   = 50.
	    hangle = 0.
	ELSE IF ( icode .eq. 2 ) THEN
	    rdir   = 50.
	    hangle = 999.
	ELSE IF ( icode .eq. 3 ) THEN
	    rdir   = 50.
	    hangle = 130.
	ELSE IF ( icode .eq. 4 ) THEN
	    rdir   = 0.
	    hangle = 999.
	ELSE IF ( icode .eq. 5 ) THEN
	    rdir   = 130.
	    hangle = 50.
	ELSE IF ( icode .eq. 6 ) THEN
	    rdir   = 130.
	    hangle = 360.
	ELSE IF ( icode .eq. 7 ) THEN
	    rdir   = 130.
	    hangle = 999.
	ELSE IF ( icode .eq. 8 ) THEN
	    rdir   = 130.
	    hangle = -130.
	ELSE
	    RETURN
	END IF
	rdir = rdir * DTR
C
C*	Compute sine and cosine of segment orientation.
C
	xcos = COS ( rdir )
	xsin = SIN ( rdir )
C
C*	Set position of base of long segment.
C
	iblsx = ixc - ispanx * NINT ( hafsiz * xcos )
	iblsy = iyc - ispany * NINT ( hafsiz * xsin )
C
C*	Set position of end of long segment.
C
	ielsx = ixc + ispanx * NINT ( hafsiz * xcos )
	ielsy = iyc + ispany * NINT ( hafsiz * xsin )
C
C*	Check for need to draw the short segment.
C
	IF  ( hangle .gt. 360. ) THEN
	    ixp ( 1 ) = iblsx
	    iyp ( 1 ) = iblsy
	    ixp ( 2 ) = ielsx
	    iyp ( 2 ) = ielsy
	    CALL ILINE ( 2, ixp, iyp, ier )
	    RETURN
	END IF
C
C*	A short segment exists at either top or bottom of long
C*	segment.  First compute relative position of its end.
C
	hangle = hangle * DTR
	xrel = sssize * cos ( hangle )
	yrel = sssize * sin ( hangle )
C
	IF  ( hangle .gt. 0.0 ) THEN
C
C*	    Short segment is at the bottom.
C
	    ixp ( 1 ) = iblsx + ispanx * NINT ( xrel )
	    iyp ( 1 ) = iblsy + ispany * NINT ( yrel )
	    ixp ( 2 ) = iblsx
	    iyp ( 2 ) = iblsy
	    ixp ( 3 ) = ielsx
	    iyp ( 3 ) = ielsy
	ELSE
C
C*	    Short segment is at the top.
C
	    ixp ( 1 ) = iblsx
	    iyp ( 1 ) = iblsy
	    ixp ( 2 ) = ielsx
	    iyp ( 2 ) = ielsy
	    ixp ( 3 ) = ielsx + ispanx * NINT ( xrel )
	    iyp ( 3 ) = ielsy + ispany * NINT ( yrel )
	END IF
C
C*	Connect these three line points.
C
	CALL ILINE ( 3, ixp, iyp, ier )
C*
	RETURN
	END
