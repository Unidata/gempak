	SUBROUTINE IPATLN ( np, ix, iy, iret )
C************************************************************************
C* IPATLN								*
C* 									*
C* This subroutine draws the special line patterns.			*
C* 									*
C* IPATLN ( NP, IX, IY, IRET )						*
C* 									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	IX (NP)		INTEGER		X coordinates			*
C*	IY (NP)		INTEGER		Y coordinates			*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98						*
C* S. Jacobs/NCEP	 4/98	Fixed color setting			*
C* S. Jacobs/NCEP	 4/98	Fixed direction of tick marks		*
C* S. Jacobs/NCEP	 4/98	Fixed end points of box and X		*
C* S. Jacobs/NCEP	 4/98	Added color reset at end of routine	*
C* S. Jacobs/NCEP	 5/98	Fixed arrow for multiple identical pts	*
C* S. Danz/AWC		 5/98	Changed to adjust blanks & reg line len	*
C* S. Jacobs/NCEP	 6/98	Changed the direction of arrow lines	*
C* S. Jacobs/NCEP	 6/98	Removed reading of pattern table	*
C* S. Jacobs/NCEP	 9/98	Removed check for dup end pts for arrows*
C* S. Jacobs/NCEP	 9/98	Added check for all points the same	*
C* A. Hardy/GSC          9/98   Added RBK device driver                 *
C* S. Jacobs/NCEP	11/98	Added calc of smallest space/line size	*
C* S. Jacobs/NCEP	 3/99	Added pattern 15 for arrow heads	*
C* S. Jacobs/NCEP	 4/99	Added ability to "flip" pattern 15	*
C* S. Jacobs/NCEP	12/99	Added center line to pattern 15		*
C* S. Jacobs/NCEP	12/99	Changed use of msldir to reorder points	*
C* S. Jacobs/NCEP	 2/00	Fixed pt reorder and drct flag usage	*
C* A. Hardy,GSC		 6/00   Added endpts parameter to IARC          *
C* J. Wu/SAIC		10/01   Added pattern 16 - double line        	*
C* S. Gilbert/NCEP	 6/05	Added pattern 17 - "Z" line		*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GBUFFT.CMN'
C*
	INTEGER		ix (*), iy (*)
C*
	INTEGER		jx (LLMXPT), jy (LLMXPT)
	INTEGER		ixp (LLMXPT), iyp (LLMXPT), i3typ (3), 
     +			jxp (5), jyp (5), mmcolr (2)
	REAL		sppat (LLNSEG), patsp (LLNSEG), x (5), y (5)
        REAL		endpts(5)
	LOGICAL		flgfil, flgcls, found, done
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
	IF  ( np .le. 1 ) RETURN
C
C*	Find the requested pattern from the common area.
C*	If the pattern number does not exist, return.
C
	found = .false.
	kpat  = 0
	DO WHILE  ( ( .not. found ) .and. ( kpat .le. LLNPAT ) )
	    kpat = kpat + 1
	    IF  ( msltyp .eq. isptyp (kpat) )  found = .true.
	END DO
	IF  ( .not. found )  RETURN
C
C*	If the direction flag is negative, and the line type is not
C*	a front, swap the points.
C
	IF  ( ( msldir .lt. 0 ) .and. ( msltyp .lt. 900 ) )  THEN
	    i = np
	    DO  j = 1, np
		jx (j) = ix (i)
		jy (j) = iy (i)
		i = i - 1
	    END DO
	    jdir = 1
	  ELSE
	    DO  i = 1, np
		jx (i) = ix (i)
		jy (i) = iy (i)
	    END DO
	    jdir = msldir
	END IF
C
C*	Save the current colors.
C
	mmcolr(1) = mcolr
	mmcolr(2) = mcolr2
C
C*	Set the current special line pattern.
C
	nspcs = 0
	DO  i = 1, LLNSEG
	    sppat(i) = 0.
	    IF  ( isppat (i,kpat) .ge. 1000000 )  THEN
		sppat (i) = isppat (i,kpat)
	      ELSE
		sppat (i) = isppat (i,kpat) * bscalp * tslsiz
	    END IF
	    IF  ( sppat(i) .ne. 0. )  THEN
		nspsg = i
	    	iptype = ABS ( MOD ( ispatt(i,kpat), 100 ) )
		IF (( iptype .eq. 0 ) .or. ( iptype .eq. 1 )) THEN
		    nspcs = nspcs + 1
		END IF
	    END IF
	END DO
	IF  ( sppat(1) .eq. 0. ) RETURN
C
C*	Compute the total distance of the line.
C
	tdist = 0.
	DO  i = 2, np
	    ddist = SQRT ( ( FLOAT ( jx(i) - jx(i-1) ) **2 ) +
     +			   ( FLOAT ( jy(i) - jy(i-1) ) **2 ) )
	    tdist = tdist + ddist
	END DO
C
C*	If a filled arrow head is added to the end of the line, stop
C*	the pattern before the arrow head.
C
	jpat = ABS ( MOD ( ispatt(nspsg+1,kpat), 100 ) )
	IF  ( jpat .eq. 8 )  THEN
	    tdist = tdist - ( 12 * bscalp * tslsiz )
	END IF
C
C*	Adjust the length of the pattern segments.
C
	IF  ( sppat(1) .ge. 1000000 )  THEN
	    IF  ( tdist .gt. 0. )  THEN
		npat = 1
	      ELSE
		npat = 0
	    END IF
	  ELSE
C
C*	    Find the smallest size for spaces and lines that will
C*	    allow at least one full pattern in the total distance.
C*	    If the pattern will not fit in the distance, and the loop
C*	    has been executed 5*bscalp times, stop the loop.
C
	    npat = 0
	    mcnt = 0
	    DO WHILE  ( ( npat .eq. 0 ) .and.
     +			( mcnt .le. bscalp * 5 ) )
		mcnt = mcnt + 1
		DO  i = 1, nspsg
		    iptype = ABS ( MOD ( ispatt(i,kpat), 100 ) )
		    IF  ( ( iptype .eq. 0 ) .or.
     +			  ( iptype .eq. 1 ) )  THEN
			sppat(i) = sppat(i) / mcnt
		    END IF
		END DO
		dp = 0.
		DO  j = 1, nspsg
		    dp = dp + sppat(j)
		END DO
		npat = INT ( tdist / dp )
	    END DO
	END IF
C
C*	If the pattern is too large for the total distance,
C*	just draw a regular line.
C
	IF  ( npat .le. 0 )  THEN
	    icolr = mmcolr (1)
	    CALL HSCOLR ( icolr, ier )
	    CALL ILINE ( np, jx, jy, ier )
	    RETURN
	END IF
C
C*	Compute the adjusted distances.
C
	IF  ( sppat(1) .ge. 1000000 )  THEN
	    patsp (1) = sppat (1)
	  ELSE
	    IF ( nspcs .gt. 0 ) THEN
		tused = dp * npat
		trem = tdist - tused
		ppp = trem / FLOAT ( npat )
		pps = ppp / FLOAT ( nspcs )
		DO  j = 1, nspsg
		    iptype = ABS ( MOD ( ispatt(j,kpat), 100 ) )
		    IF (( iptype .eq. 0 ) .or. ( iptype .eq. 1 )) THEN
			patsp (j) = sppat (j) + pps
		    ELSE
			patsp (j) = sppat (j)
		    END IF
		END DO
	      ELSE
		dpat = tdist / FLOAT ( npat )
		DO  j = 1, nspsg
		    patsp (j) = sppat (j) / dp * dpat
		END DO
	    END IF
	END IF
C
C*	Start at the first pattern segment
C
	kseg = 1
	dlen = patsp(1)
C
C*	Move first point to the buffer.
C
	ixp (1) = jx (1)
	iyp (1) = jy (1)
	npt = 1
C
C*	Loop through the rest of the points.
C
	icolr = mmcolr ( ispclr(kseg,kpat) )
	CALL HSCOLR ( icolr, ier )
	icsav = icolr
	xsav = RMISSD
	ysav = RMISSD
	xb = FLOAT ( jx (1) )
	yb = FLOAT ( jy (1) )
	DO  i = 2, np
	    xa = xb
	    ya = yb
	    xb = FLOAT ( jx (i) )
	    yb = FLOAT ( jy (i) )
C
C*	    Compute distance to this point.
C
	    dist = SQRT ( ( xb - xa ) **2 + ( yb - ya ) **2 )
C
C*	    Loop until this line segment is used.
C
	    DO WHILE ( dist .gt. 0. )
		jpat = ABS ( MOD ( ispatt(kseg,kpat), 100 ) )
		jnum = ispatt(kseg,kpat) / 100
		icolr = mmcolr ( ispclr(kseg,kpat) )
		CALL HSCOLR ( icolr, ier )
C
C*	    	Check whether this distance will fit in this line
C*		segment.
C
	      	IF  ( ( dlen - dist ) .ge. 0.005 ) THEN
C
C*		    Add entire line on odd segments; otherwise,
C*		    leave space.
C
		    IF  ( jpat .eq. 1 )  THEN
			IF  ( icsav .ne. icolr )  THEN
			    CALL HSCOLR ( icsav, ier )
			    CALL ILINE  ( npt, ixp, iyp, ier )
			    ixp ( 1 ) = ixp ( npt )
			    iyp ( 1 ) = iyp ( npt )
			    npt = 1
			    CALL HSCOLR ( icolr, ier )
			    icsav = icolr
			END IF
			npt = npt + 1
			ixp (npt) = jx (i)
			iyp (npt) = jy (i)
			xsav = RMISSD
			ysav = RMISSD
		      ELSE
C
C*			Update the saved point only if it is missing.
C
			IF  ( ERMISS ( xsav ) .or.
     +			      ERMISS ( xsav ) )  THEN
			    xsav = xa
			    ysav = ya
			END IF
		    END IF
		    xa = FLOAT ( jx (i) )
		    ya = FLOAT ( jy (i) )
C
C*		    Reset length of current segment to be drawn.
C
		    dlen = dlen - dist
		    dist = 0.
C*
		  ELSE
C
C*		    Process arc and circles.
C
		    IF  ( ( ( jpat .ge. 2 ) .and.
     +			    ( jpat .le. 6 ) ) .or.
     +			  ( jpat .eq. 13 ) .or.
     +			  ( jpat .eq. 14 ) .or.
     +			  ( jpat .eq. 15 ) )  THEN
C
			flgfil = .false.
			flgcls = .false.
			IF  ( ( jpat .eq. 3 ) .or.
     +			      ( jpat .eq. 5 ) )  flgfil = .true.
			IF  ( jpat .eq. 6 )  flgcls = .true.
C
			IF  ( ( ( ddev .eq. 'UTF' ) .or. 
     +                      ( ddev .eq. 'RBK' ) ) .and. flgfil )  THEN
			    flgfil = .false.
			    flgcls = .true.
			END IF
C
			xc = xa + dlen / dist * ( xb - xa )
			yc = ya + dlen / dist * ( yb - ya )
			adist = SQRT ( ( xc - xa ) **2 +
     +				       ( yc - ya ) **2 )
C
			IF  ( ERMISS ( xsav ) .and.
     +			      ERMISS ( ysav ) )  THEN
			    xpt = xa
			    ypt = ya
			    bdist = adist
			  ELSE
			    xpt = xsav
			    ypt = ysav
			    bdist = SQRT ( ( xc - xpt ) **2 +
     +					   ( yc - ypt ) **2 )
			END IF
C
C*		    	Find point for center of circle/arc.
C
			IF  ( ( jpat .eq. 2 ) .or.
     +			      ( jpat .eq. 3 ) )  THEN
			    radh = patsp(kseg) / 2.
			    radl = radh
			    xd   = xpt + radl / bdist * ( xc - xpt )
			    yd   = ypt + radh / bdist * ( yc - ypt )
			    ang1 = 0.
			    ang2 = 360.
			  ELSE
			    IF  ( jpat .eq. 13 )  THEN
				aang =  90.
			      ELSE IF  ( jpat .eq. 14 )  THEN
				aang = 270.
			      ELSE IF  ( jpat .eq. 15 )  THEN
				aang = 120.
			      ELSE
				aang = 180.
			    END IF
			    radh = bdist / 2.
			    radl = radh
			    xd   = xpt + radl / bdist * ( xc - xpt )
			    yd   = ypt + radh / bdist * ( yc - ypt )
			    tang = ATAN2 ( ( yc - yd ) * ispany,
     +					   ( xc - xd ) * ispanx )
			    idir = ISIGN(1,jnum) * ISIGN(1,jdir)
			    IF  ( jpat .eq. 15 )  THEN
				angt = tang * RTD
				IF  ( idir .gt. 0 )  THEN
				    ang1 = angt - aang
				    ang2 = angt + aang
				  ELSE
				    ang1 = angt - aang - 180.
				    ang2 = angt + aang - 180.
				END IF
			      ELSE
				ang1 = tang * RTD
				IF  ( jpat .eq. 14 )
     +				    ang1 = ang1 - ( 45 * idir )
				ang2 = ang1 + ( idir * aang )
			    END IF
			END IF
C
C*			Draw the arc/circle.
C
			ixc = NINT ( xd )
			iyc = NINT ( yd )
			ns = ABS ( jnum )
			CALL HSCOLR ( icolr, ier )
			CALL IARC  ( ixc, iyc, radl, radh, ns,
     +				     ang1, ang2, endpts, flgfil, 
     +				     flgcls, ier )
			IF  ( jpat .eq. 5 )  THEN
			    jxp (1) = NINT ( xpt )
			    jyp (1) = NINT ( ypt )
			    jxp (2) = NINT ( xc )
			    jyp (2) = NINT ( yc )
			    CALL ILINE ( 2, jxp, jyp, ier )
			END IF
			xa = xc
			ya = yc
			xsav = RMISSD
			ysav = RMISSD
C
C*			Allow the line to pass through the 90 and 270
C*			degree arcs.
C
			IF  ( ( jpat .ne. 13 ) .and.
     +			      ( jpat .ne. 14 ) )  THEN
			    npt = 1
			    ixp (npt) = xa
			    iyp (npt) = ya
			END IF
C
C*			Add the center line to the arrow head pattern
C*			element.
C
			IF  ( jpat .eq. 15 )  THEN
			    jxp (1) = NINT ( xpt )
			    jyp (1) = NINT ( ypt )
			    jxp (2) = NINT ( xa )
			    jyp (2) = NINT ( ya )
			    CALL ILINE ( 2, jxp, jyp, ier )
			END IF
C
C*		      Process X's, boxes and tick marks.
C
		      ELSE IF  ( ( ( jpat .ge.  9 ) .and.
     +				   ( jpat .le. 12 ) ) .or. 
     +                            ( jpat .eq. 16 ) .or.
     +                            ( jpat .eq. 17 ) )  THEN
C
			hght = ABS ( jnum ) * bscalp * tslsiz
			xc = xa + dlen / dist * ( xb - xa )
			yc = ya + dlen / dist * ( yb - ya )
			adist = SQRT ( ( xc - xa ) **2 +
     +				       ( yc - ya ) **2 )
C
			IF  ( ERMISS ( xsav ) .and.
     +			      ERMISS ( ysav ) )  THEN
			    xpt = xa
			    ypt = ya
			  ELSE
			    xpt = xsav
			    ypt = ysav
			END IF
C
			tang = ATAN2 ( ( yc - ypt ) * ispany,
     +				       ( xc - xpt ) * ispanx )
			x(1) = xpt + hght * COS (tang+HALFPI) * ispanx
			y(1) = ypt + hght * SIN (tang+HALFPI) * ispany
			x(2) = xpt + hght * COS (tang-HALFPI) * ispanx
			y(2) = ypt + hght * SIN (tang-HALFPI) * ispany
			x(3) = xc + hght * COS (tang-HALFPI) * ispanx
			y(3) = yc + hght * SIN (tang-HALFPI) * ispany
			x(4) = xc + hght * COS (tang+HALFPI) * ispanx
			y(4) = yc + hght * SIN (tang+HALFPI) * ispany
			x(5) = x(1)
			y(5) = y(1)
			IF  ( ( jpat .eq. 10 ) .or.
     +			      ( jpat .eq. 11 ) )  THEN
			    DO  k = 1, 5
				jxp (k) = NINT ( x(k) )
				jyp (k) = NINT ( y(k) )
			    END DO
			    IF  ( jpat .eq. 11 )  THEN
				IF  ( ( ddev .ne. 'UTF' ) .and.
     +                                ( ddev .ne. 'RBK' ) ) THEN
				    CALL HSCOLR ( icolr, ier )
     				    CALL IFILL ( 5, jxp, jyp, ier )
				END IF
			    END IF
			    CALL ILINE ( 5, jxp, jyp, ier )
			  ELSE IF  ( jpat .eq. 9 )  THEN
			    jxp (1) = NINT ( x(1) )
			    jyp (1) = NINT ( y(1) )
			    jxp (2) = NINT ( x(3) )
			    jyp (2) = NINT ( y(3) )
			    CALL ILINE ( 2, jxp, jyp, ier )
			    jxp (1) = NINT ( x(2) )
			    jyp (1) = NINT ( y(2) )
			    jxp (2) = NINT ( x(4) )
			    jyp (2) = NINT ( y(4) )
			    CALL ILINE ( 2, jxp, jyp, ier )
			  ELSE IF  ( jpat .eq. 12 )  THEN
			    IF  ( ERMISS ( xsav ) .or.
     +				  ERMISS ( ysav ) )  THEN
				jxp (1) = NINT ( xa )
				jyp (1) = NINT ( ya )
				jxp (2) = NINT ( xc )
				jyp (2) = NINT ( yc )
				IF  ( jdir .gt. 0 )  THEN
				    jxp (3) = NINT ( x(3) )
				    jyp (3) = NINT ( y(3) )
				  ELSE
				    jxp (3) = NINT ( x(4) )
				    jyp (3) = NINT ( y(4) )
				END IF
				numt = 3
			      ELSE
				jxp (1) = NINT ( xsav )
				jyp (1) = NINT ( ysav )
				jxp (2) = NINT ( xa )
				jyp (2) = NINT ( ya )
				jxp (3) = NINT ( xc )
				jyp (3) = NINT ( yc )
				IF  ( jdir .gt. 0 )  THEN
				    jxp (4) = NINT ( x(3) )
				    jyp (4) = NINT ( y(3) )
				  ELSE
				    jxp (4) = NINT ( x(4) )
				    jyp (4) = NINT ( y(4) )
				END IF
				numt = 4
			    END IF
			    CALL ILINE ( numt, jxp, jyp, ier )
			  ELSE IF  ( jpat .eq. 16 )  THEN
			    jxp (1) = NINT ( x(1) )
			    jyp (1) = NINT ( y(1) )
			    jxp (2) = NINT ( x(4) )
			    jyp (2) = NINT ( y(4) )
			    CALL ILINE ( 2, jxp, jyp, ier )
			    jxp (1) = NINT ( x(2) )
			    jyp (1) = NINT ( y(2) )
			    jxp (2) = NINT ( x(3) )
			    jyp (2) = NINT ( y(3) )
			    CALL ILINE ( 2, jxp, jyp, ier )
			  ELSE IF  ( jpat .eq. 17 )  THEN
			    jxp (1) = NINT ( x(1) )
			    jyp (1) = NINT ( y(1) )
			    jxp (2) = NINT ( x(4) )
			    jyp (2) = NINT ( y(4) )
			    CALL ILINE ( 2, jxp, jyp, ier )
			    jxp (1) = NINT ( x(2) )
			    jyp (1) = NINT ( y(2) )
			    jxp (2) = NINT ( x(3) )
			    jyp (2) = NINT ( y(3) )
			    CALL ILINE ( 2, jxp, jyp, ier )
			    jxp (1) = NINT ( x(2) )
			    jyp (1) = NINT ( y(2) )
			    jxp (2) = NINT ( x(4) )
			    jyp (2) = NINT ( y(4) )
			    CALL ILINE ( 2, jxp, jyp, ier )
			END IF
			xa = xc
			ya = yc
			xsav = RMISSD
			ysav = RMISSD
			npt = 1
			ixp (npt) = xa
			iyp (npt) = ya
C
C*		      Process regular lines and blanks.
C
		      ELSE
C
C*		    	Find point to break line.
C
			xc = xa + dlen / dist * ( xb - xa )
			yc = ya + dlen / dist * ( yb - ya )
			adist = SQRT ( ( xc - xa ) **2 +
     +				       ( yc - ya ) **2 )
C
C*		    	Draw line on odd segment when linflg is set.
C*		    	Otherwise, leave space and start new line.
C
			IF  ( jpat .eq. 1 )  THEN
			    IF  ( icsav .ne. icolr )  THEN
				CALL HSCOLR ( icsav, ier )
				CALL ILINE  ( npt, ixp, iyp, ier )
				ixp ( 1 ) = ixp ( npt )
				iyp ( 1 ) = iyp ( npt )
				npt = 1
				CALL HSCOLR ( icolr, ier )
			    END IF
			    npt = npt + 1
			    ixp ( npt ) = NINT ( xc )
			    iyp ( npt ) = NINT ( yc )
			    CALL ILINE  ( npt, ixp, iyp, ier )
			  ELSE
			    npt = 1
			    ixp ( npt ) = NINT ( xc )
			    iyp ( npt ) = NINT ( yc )
			END IF
			xa = xc
			ya = yc
			xsav = RMISSD
			ysav = RMISSD
		    END IF
C
C*		    Start on next segment.
C
            	    kseg = kseg + 1
            	    IF  ( kseg .gt. nspsg ) THEN
		        kseg = 1
		    END IF
		    icsav = icolr
C*
		    IF  ( dlen .eq. dist ) THEN
		        dist = 0.
		      ELSE
		        dist = dist - adist
		    END IF
C
C*		    Compute new total length of segments.
C
	            dlen = patsp ( kseg )
	    	END IF
	    END DO
	END DO
C
C*	Flush last buffer.  
C
	IF  ( npt .gt. 1 )  THEN
	    CALL ILINE ( npt, ixp, iyp, ier )
	END IF
C
C*	Draw an arrow head on the end of the line.
C
	jpat = ABS ( MOD ( ispatt(nspsg+1,kpat), 100 ) )
	IF  ( ( jpat .eq. 7 ) .or. ( jpat .eq. 8 ) )  THEN
C
C*	    Set the direction for the arrow head. The speed is zero.
C
	    spd  = 0.
	    done = .false.
C
C*	    Check if the arrow head goes on the end of the line
C*	    or at the beginning.
C
	    IF  ( jdir .gt. 0 )  THEN
		ie = np
	      ELSE
		ie = 1
	    END IF
	    inc = ISIGN ( 1, jdir )
C
C*	    Check for a duplicate end point. Do not draw an arrow head
C*	    if the last two points are the same.
C
	    IF  ( ( jx(ie) .eq. jx(ie-inc) ) .and.
     +		  ( jy(ie) .eq. jy(ie-inc) ) )  THEN
C
C*		Do not plot an arrow head.
C
	      ELSE
C
C*	    	Set the direction using the arctangent.
C
		dir = ATAN2 ( FLOAT ( jy(ie) - jy(ie-inc) ) * ispany, 
     +			      FLOAT ( jx(ie) - jx(ie-inc) ) * ispanx )
		dir = dir * RTD
C
C*	    	If requested, set the type to fill the arrow head.
C
		i3typ(3) = 0
		i3typ(2) = 0
		i3typ(1) = 1
		IF  ( jpat .eq. 8 )  i3typ(1) = 2
C
C*	    	Set the size and arrow head size.
C
		size  = 0.
		hdsiz = tslsiz * bscalh * 1.5
C
C*	    	Draw the arrow head at the last point of the line.
C
		CALL IARRW ( jx(ie), jy(ie), spd, dir, i3typ, size,
     +			     hdsiz, ier )
	    END IF
	END IF
C
C*	Reset the color to color 1.
C
	CALL HSCOLR ( mmcolr (1), ier )
C*
	RETURN
	END
