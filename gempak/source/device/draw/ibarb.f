      SUBROUTINE IBARB  ( ixx, iyy, spd, dir, ihs, i3typ, size, iret )
C************************************************************************
C* IBARB								*
C*									*
C* This subroutine draws a wind barb at the specified point.  The	*
C* direction is relative to local north.				*
C*									*
C* IBARB  ( IXX, IYY, SPD, DIR, IHS, I3TYP, SIZE, IRET )		*
C*									*
C* Input parameters:							*
C*	IXX			INTEGER		X coordinate		*
C*	IYY			INTEGER		Y coordinate		*
C*	SPD			REAL		Wind speed		*
C*	DIR			REAL		Wind direction		*
C*	IHS			INTEGER		Hemisphere flag		*
C*					 	 +1 = north		*
C*						 -1 = south		*
C*	I3TYP (3)		INTEGER		Barb type parameters	*
C*                                               I3TYP (1) => fill flag	*
C*                                               I3TYP (2) => position	*
C*                                               I3TYP (3) => calm wind	*
C*	SIZE			REAL		Size in device coord	*
C*									*
C* Output parameters:							*
C*	IRET			INTEGER		Return code		*
C**									*
C* Log:									*
C* G. Chatters/RDS	 2/82						*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* S. Schotz/GSC	 5/90   Reduce size of calm circle		*
C* K. Brill/NMC		10/91	Changes for new barb types		*
C* K. Brill/NMC		11/91	Receive I3TYP through subroutine call	*
C* K. Brill/NMC		11/91	Changed SCYRAD factor from 1.2 to 1.8	*
C* M. Linda/GSC		12/96	Added inbnds test, removed aspect	*
C* T. Lee/GSC		 8/98	Replace ITRIF with IFILL; Add fill box	*
C* S. Jacobs/NCEP	 2/99	Force the flag fill to use solid fill	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		i3typ (3)
	INTEGER		ipx (3), ipy (3), jxo (5), jyo (5)
	REAL		xoff (5), yoff (5)
C
	PARAMETER	( BRBANG = 70., NCIRC = 40 )
	PARAMETER	( MAXBAS = 6 )
	PARAMETER	( SPFRAC = .16666667, BFRAC = .33333333 )
	PARAMETER	( HBFRAC = .16666667 )
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
C*	Compute radius of calm wind and sky cover circles.
C
	clmrad = size * RADCLM
	scyrad = tskysz * bscalc * RADSKY
C
C*	Make the calm wind circle larger than the sky cover circle
C*	if that is required.
C
	IF ( i3typ (2) .eq. 3 ) clmrad = scyrad * 1.8
C
C*	Check wind speed.  If less than .5, do nothing or draw a circle.
C
	extra = .15 * size
	np = 5
	IF  ( spd .lt. 0.5 .and. i3typ (3) .eq. 2 )  THEN
	    RETURN
C
	  ELSE IF ( spd .lt. 0.5 .and. i3typ (3) .eq. 1 ) THEN
C
C*	    Check center against clipping area; also checks if missing.
C
	    IF ( .not. inbnds ( ixx, iyy ) ) RETURN
C
C*	    Draw a fill box around calm wind.
C
	    IF ( i3typ (1) .eq. 3 .or. i3typ (1) .eq. 4 )  THEN
		arm = clmrad + extra / 2.
		jxo (1) = ixx - arm
		jyo (1) = iyy - arm
		jxo (2) = jxo (1)
		jyo (2) = iyy + arm
		jxo (3) = ixx + arm
		jyo (3) = jyo (2) 
		jxo (4) = jxo (3)
		jyo (4) = jyo (1)
		jxo (5) = jxo (1)
		jyo (5) = jyo (1)
		mmcolr = mcolr
		mmfltyp = mfltyp
		CALL DSCOLR ( 101, imclr, ier )
		CALL DSFILL ( 0., 1, fsiz, jtyp, ier )
		CALL IFILL  ( np, jxo, jyo, ier )
		CALL DSCOLR ( mmcolr, imclr, ier )
		CALL DSFILL ( 0., mmfltyp, fsiz, jtyp, ier ) 
	    END IF
C
	    
            CALL ICIRC  ( ixx, iyy, clmrad, ncirc, ier )
C
	  ELSE
C
C*	    Compute the number of flags, whole barbs and half barbs
C*	    needed to represent the wind speed.
C
	    nspd  = spd + 0.5
	    nspd  = MOD ( nspd, 10 )
	    spd1  = spd + 2.5
	    nflag = spd1 / 50.
	    bbarb = (spd1 - nflag*50.) / 10.
	    nbarb = bbarb
	    kbarb = bbarb + 0.5
	    nhbarb = 0
	    IF  ( nbarb .ne. kbarb )  nhbarb = 1
	    numbas = nflag*2 + nbarb + nhbarb
C
C*	    Compute size of barbs and spacing.
C
	    sp    = size * spfrac
	    blen  = size * bfrac
	    hblen = size * hbfrac
C
C*	    Test if barb needs to be lengthened and by how much.
C
	    tlen = size
	    IF (numbas .gt. maxbas) tlen = size + (numbas-maxbas) * sp
C
C*	    NOTE: SIZE is used to compute length of barbs and flags.
C*		  TLEN is used to compute plot positions.
C
C*	    Shift barb starting point if it must start on the
C*	    sky cover circle or is shifted half a length.
C
	    IF ( i3typ (2) .eq. 3 ) THEN
		bpstn = scyrad
	    ELSE IF ( i3typ (2) .eq. 2 ) THEN
		bpstn = (-0.5) * tlen
	    ELSE
		bpstn = 0.0
	    END IF
	    rorien = dir * DTR
	    cosror = COS (rorien)
	    sinror = SIN (rorien)
	    ix = NINT (bpstn * cosror) * ispanx + ixx
	    iy = NINT (bpstn * sinror) * ispany + iyy
C
C*	    Check base against clipping area; also checks if missing.
C
	    IF ( .not. inbnds ( ix, iy ) ) RETURN
C
C*	    Move pen to base of wind barb.
C
	    ipx (1) = ix
	    ipy (1) = iy
C
C*	    Compute end of wind barb line.
C
	    ipx (2) = NINT (tlen * cosror) * ispanx + ix
	    ipy (2) = NINT (tlen * sinror) * ispany + iy

C
C*	    Draw a fill box with the background color.
C
	    IF  ( i3typ (1) .eq. 3 .or. i3typ (1) .eq. 4 )  THEN
		blenr   = blen * SIN ( brbang * DTR )
		xoff(1) = - extra
		yoff(1) = - ( extra + blenr * ABS (ihs + 1.) / 2. ) 
		xoff(2) = xoff (1)
		yoff(2) = extra + blenr * ABS (ihs - 1.) / 2.
C
		xoff(3) = extra + tlen
		yoff(3) = yoff (2)
		xoff(4) = xoff (3)
		yoff(4) = yoff (1)
		xoff(5) = xoff (1)
		yoff(5) = yoff (1)
C
		DO i = 1, np
		    xprimd = xoff (i) * cosror - yoff (i) * sinror
		    yprimd = xoff (i) * sinror + yoff (i) * cosror
		    jxo (i) = ipx (1) + NINT ( ispanx * xprimd )
		    jyo (i) = ipy (1) + NINT ( ispany * yprimd )
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
C*	    Draw barb shaft.
C
	    CALL ILINE  ( 2, ipx, ipy, ier )
C
C*	    Compute angle for positioning of flags and barbs.
C
	    rbang = ( dir - ihs * brbang ) * DTR
C
C*	    Compute positions of flags.
C
	    nbase  = 0
	    cosrba = COS ( rbang )
	    sinrba = SIN ( rbang )
C
C*	    Draw flags.
C
	    DO  i = 1, nflag
C
C*		Compute 1st base of flag.
C
		ixfbs1 = NINT ( ( tlen - nbase * sp ) * cosror )
     +						      * ispanx + ix
		iyfbs1 = NINT ( ( tlen - nbase * sp ) * sinror )
     +						      * ispany + iy
C
C*		Compute 2nd base of flag.
C
		ixfbs2 = NINT ( ( tlen - ( nbase + 1 ) * sp ) * cosror )
     +						       * ispanx + ix
		iyfbs2 = NINT ( ( tlen - ( nbase + 1 ) * sp ) * sinror )
     +						       * ispany + iy
C
C*		Move pen to 2nd base.
C
		ipx (1) = ixfbs2
		ipy (1) = iyfbs2
C
C*		Compute tip of flag and draw.
C
		ipx (2) = NINT (blen * cosrba) * ispanx + ixfbs2
		ipy (2) = NINT (blen * sinrba) * ispany + iyfbs2
C
C*		Draw to 1st base.
C
		ipx (3) = ixfbs1
		ipy (3) = iyfbs1
C
C*		Draw flag.
C
		CALL ILINE  ( 3, ipx, ipy, ier )
	  	IF ( i3typ (1) .eq. 2 .or. i3typ (1) .eq. 4 )  THEN
		    mmfltyp = mfltyp
		    CALL DSFILL ( 0., 1, fsiz, jtyp, ier )
		    CALL IFILL  ( 3, ipx, ipy, ier )
		    CALL DSFILL ( 0., mmfltyp, fsiz, jtyp, ier )
		END IF
C
C*		Increment base number counter.
C
		nbase = nbase + 2
	    END DO
C
C*	    Draw whole barbs.
C
	    DO  i = 1, nbarb
C
C*		Compute base of whole barb and move pen.
C
		ipx (1) = NINT ( ( tlen - nbase * sp ) * cosror ) 
     +						       * ispanx + ix
		ipy (1) = NINT ( ( tlen - nbase * sp ) * sinror )
     +						       * ispany + iy
C
C*		Compute tip of barb and draw.
C
		ipx (2) = NINT (blen * cosrba) * ispanx + ipx (1)
		ipy (2) = NINT (blen * sinrba) * ispany + ipy (1)
		CALL ILINE ( 2, ipx, ipy, ier )
C
C*		Increment base count.
C
		nbase = nbase + 1
	    END DO
C
C*	    Draw half barbs.
C
	    DO  i = 1, nhbarb
C
C*		If no bases have been used, start half barb at 2nd base.
C
		IF ( NBASE .EQ. 0 ) nbase = 1
C
C*		Compute base of half barb.
C
		ipx (1) = NINT ( ( tlen - nbase * sp ) * cosror ) 
     +						       * ispanx + ix
		ipy (1) = NINT ( ( tlen - nbase * sp ) * sinror )
     +		 				       * ispany + iy
C
C*		Compute tip of half barb.
C
		ipx (2) = NINT (hblen * cosrba) * ispanx + ipx (1)
		ipy (2) = NINT (hblen * sinrba) * ispany + ipy (1)
		CALL ILINE ( 2, ipx, ipy, ier )
C
C*		Increment base count.
C
		nbase = nbase + 1
	    END DO
	END IF
C*
	RETURN
	END
