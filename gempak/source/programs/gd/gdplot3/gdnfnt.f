	SUBROUTINE GDNFNT ( ictyp, line, loci, info, ifrnt, iret )
C************************************************************************
C* GDNFNT								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* T. Lee/SAIC		01/06	Copied from GPNFNT			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
C*
	CHARACTER*(*)	line, loci, info
C*
	PARAMETER	( NCPTS = 55 )
C*
	REAL		dist(NCPTS), ang(NCPTS), pds(NCPTS),
     +			pdx(NCPTS), pdy(NCPTS)
	INTEGER		npts(NCPTS), jx(NCPTS), jy(NCPTS),
     +			kx(NCPTS), ky(NCPTS), lx (NCPTS), ly (NCPTS)
C*
	CHARACTER	sys*1, crvpts(20)*24, pinfo(4)*72
     	REAL 		xcrv (NCPTS), ycrv (NCPTS), xval (NCPTS),
     +			yval (NCPTS), value(2), tmpt (2),
     +			xx (NCPTS), yy (NCPTS)
     	REAL 		smooth, linfltr
	INTEGER		icolor(2),iwidth(2), jtype(2), ilabel(2)
C------------------------------------------------------------------------
C*	Get the plot bounds in normal coordinates.
C
	CALL GQBND ( 'D', xleft, ybot, xrght, ytop, ier )
C
C*	Parse the fill input.
C
	CALL IN_LINE ( line, value, 2, icolor, jtype, iwidth, ilabel,
     +		       smooth, linfltr, scflag, ier )
C
	IF  ( icolor(1) .ne. 0 )  THEN
C
C*	    Set the coordinate system to be used.
C
	    sys = 'N'
	    IF  ( loci(1:1) .eq. '@' )  THEN
		sys = 'G'
		loci = loci(2:)
	    ELSE IF  ( loci(1:1) .eq. '#' ) THEN
		sys = 'M'
		loci = loci(2:)
	    END IF
C
C*	    Get the point(s) for the plot.
C
	    CALL ST_CLST ( loci, '/', ' ', 20, crvpts, num, ier )
C
C*	    Get the user points along the curve.
C
	    DO  i = 1, num
		CALL ST_RLST ( crvpts(i), ';', 0., 2, tmpt, num2, ier )
C
C*		If using Normal coords, correct for PS device.
C
		xcrv(i) = tmpt(1)
		ycrv(i) = tmpt(2)
	    END DO
C
C*          Transform to Normal coords in order to compute a smooth
C*          curve. NCPTS defines how many points to compute on the
C*          curve.
C
	    CALL GTRANS ( sys, 'D', num, xcrv, ycrv, xval, yval,
     +			  ier )
	    DO i = 1, num
		lx(i) = NINT ( xval (i) )
		ly(i) = NINT ( yval (i) )
	    END DO
C
C*	    Separate the shape information.
C
	    CALL ST_CLST ( info, '/', ' ', 4, pinfo, nump, ier )
C
C*	    Get the number of pips.
C
	    CALL ST_NUMB ( pinfo(1), npip, ier )
	    iside = ISIGN ( 1, npip )
	    npip = ABS(npip)
C
C*	    Get the pip size.
C
	    CALL ST_CRNM ( pinfo(2), psiz, ier )
C
C*	    Get the line type
C
	    CALL ST_NUMB ( pinfo(3), itype, ier )
C
C*          Get number of smoothing passes.
C
	    CALL ST_NUMB ( pinfo(4), ipsnum, ier )
C
C*	    Set the line attributes.
C

        CALL GSLINE ( itype, 0, iwidth, 0, ier )
C
	ipip = npip
	IF  ( num .ge. 2 )  THEN
C
C*	    Draw the line for the front.
C
	    tdist = 0.
	    knt = 0
	    jx(1) = lx(1)
	    jy(1) = ly(1)
	    DO  i = 1, num-1
		dx = lx(i+1) - lx(i)
		dy = ly(i+1) - ly(i)
		dist(i) = SQRT ( dx*dx + dy*dy )
		ang(i) = ATAN2(dy,dx)
		tdist = tdist + dist(i)
	    END DO
	    i = 1
	    DO  WHILE ( i .lt. num .and. knt .le. NCPTS )
		npts(i) = NINT ( (dist(i)/tdist) * FLOAT(NCPTS) )
		ds = dist(i)/(npts(i)-1)
		j = 2+knt
		DO  WHILE ( j .le. npts(i)+knt .and. j .le. NCPTS )
		    jx(j) = lx(i) + NINT (COS(ang(i)) * ds*(j-knt-1) )
		    jy(j) = ly(i) + NINT (SIN(ang(i)) * ds*(j-knt-1) )
		    j = j + 1
		END DO
		knt = knt + npts(i) - 1
		i = i + 1
	    END DO
	    knt = knt + 1
	    IF  ( knt .gt. NCPTS ) knt = NCPTS
C
C*	    Smooth the line for the front.
C
	    CALL LINESM ( ipsnum, knt, jx, jy, ier )
C
	    tdist = 0.
	    DO  i = 1, knt-1
		pdx(i) = jx(i+1) - jx(i)
		pdy(i) = jy(i+1) - jy(i)
		pds(i) = SQRT ( pdx(i)*pdx(i) + pdy(i)*pdy(i) )
		tdist = tdist + pds(i)
	    END DO
	    ptdist = tdist / (knt-1)
	    num = knt
C
C*	    Draw pips on front.
C
	    IF ( iside .eq. 1 ) THEN
		jside = -1
	    ELSE
		jside = 1
	    END IF
C
C*	    Determin the size of the pips and spaces.
C
C	    nppts = INT ( 30 * psiz * 30. )
	    nppts = INT ( psiz * 30. )
	    kpip = INT (tdist - ( nppts*ipip ))
	    DO WHILE ( kpip .le. nppts )
		ipip = ipip - 1
		kpip = INT ( tdist - ( nppts*ipip ) )
	    END DO
	    lpdist = nppts - 1
	    lsdist = INT ( FLOAT ( kpip ) /  ipip ) - 1
	    lsdist = lsdist / 2
C
            CALL GSCOLR ( icolor(1), ier )
C
	    IF  ( ( ifrnt .eq. 1 ) .and. ( ipip .gt. 0 ) )  THEN
C
C*		Cold front.
C
	        i = 1
	        kpdist = 0
	        ktmp = 0
	        kx(1) = jx(1)
	        ky(1) = jy(1)
	        lmid = INT ( lpdist / 2. )
	        knt = 0
	        DO  k = 1, ipip
C
 	            CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			           kpdist, knt, num )
C
C*		    Create Cold front pip.
C
		    CALL MKCPIP ( i, kx, ky, jx, jy, lpdist, kpdist, 
     +				  lmid, jside, knt )
C
	            CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			           kpdist, knt, num )
	        END DO
C*
	    ELSE IF  ( ( ifrnt .eq. 2 ) .and. ( ipip .gt. 0 ) )  THEN
C
C*		Warm front.
C
	        i = 1
	        kpdist = 0
	        ktmp = 0
	        kx(1) = jx(1)
	        ky(1) = jy(1)
	        lmid = INT ( lpdist / 2. )
	        DO  k = 1, ipip
C
	            CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			           kpdist, knt, num )
C
C*		    Create Warm front pip.
C
		    kside = -1
		    CALL MKWPIP ( i, kx, ky, jx, jy, lpdist, kpdist, 
     +				  lmid, jside, kside, knt )
C
	            CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			           kpdist, knt, num )
C
	        END DO
C
	    ELSE IF  ( ( ifrnt .eq. 4 ) .and. ( ipip .gt. 0 ) )  THEN
C
C*		Occluded front.
C
		i = 1
		kpdist = 0
		ktmp = 0
		kx(1) = jx(1)
		ky(1) = jy(1)
		lmid = INT ( lpdist / 2. )
	        DO  k = 1, ipip
		    IF  ( MOD(k,2) .eq. 1 )  THEN
C
	        	CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			       	       kpdist, knt, num )
C
C*		        Create Cold front pip.
C
		        CALL MKCPIP ( i, kx, ky, jx, jy, lpdist, kpdist,
     +				      lmid, jside, knt )
C
	        	CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			       	       kpdist, knt, num )
C*
		    ELSE
C
	        	CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			       	       kpdist, knt, num )
C
C*		        Create Warm front pip.
C
			kside = -1
		        CALL MKWPIP ( i, kx, ky, jx, jy, lpdist, kpdist,
     +				      lmid, jside, kside, knt )
C
	        	CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			       	       kpdist, knt, num )
		    END IF
		END DO
C
	    ELSE IF  ( ( ifrnt .eq. 3 ) .and. ( ipip .gt. 0 )  )  THEN
C
C*		Stationary front.
C
		i = 1
		kpdist = 0
		ktmp = 0
		kx(1) = jx(1)
		ky(1) = jy(1)
		lmid = INT ( lpdist / 2. )
		lside = -jside
	        DO  k = 1, ipip
		    IF  ( MOD(k,2) .eq. 1 )  THEN
		        CALL GSCOLR ( icolor(2), ier )
C
	        	CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			       	       kpdist, knt, num )
C
C*		        Create Cold front pip.
C
		        CALL MKCPIP ( i, kx, ky, jx, jy, lpdist, kpdist,
     +				      lmid, lside, knt )
C
	        	CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			       	       kpdist, knt, num )
C*
		    ELSE
		        CALL GSCOLR ( icolor(1), ier )
C
	        	CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			       	       kpdist, knt, num )
C
C*		        Create Warm front pip.
C
			kside = 1
		        CALL MKWPIP ( i, kx, ky, jx, jy, lpdist, kpdist,
     +				      lmid, lside, kside, knt )
C
	        	CALL MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist,
     +			       	       kpdist, knt, num )
		    END IF
		END DO
C
	    ELSE
		DO kkk = 1, num
		    xx (kkk) = kx (kkk)
		    yy (kkk) = ky (kkk)
		END DO
	        CALL GLINE ( 'D', num, xx, yy, ier )
	    END IF
	END IF
	END IF
C*
	RETURN
	END
C
	SUBROUTINE MKSPACE ( i, kx, ky, jx, jy, lsdist, lpdist, kpdist, 
     +			     knt, num )
C************************************************************************
C									*
C									*
C									*
C									*
C									*
C									*
C									*
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INTEGER		kx(*), ky(*), jx(*), jy(*)
	REAL		xx (55), yy (55)
	PARAMETER	( PI = 3.14159265 )
C------------------------------------------------------------------------
	knt = 1
	ksdist = kpdist 
	DO WHILE ( ksdist .lt. lsdist .and. ( i .le. (num - 1) ) )
	    xdx = jx(i+1) - jx(i)
	    xdy = jy(i+1) - jy(i)
	    xds = SQRT ( xdx*xdx + xdy*xdy )
	    ktmp = ksdist
	    ksdist = ksdist + NINT ( xds )
	    i = i + 1
	    knt = knt + 1
	    kx(knt) = jx(i)
	    ky(knt) = jy(i)
	END DO
	jpdist = lsdist - ktmp
	ptang = ATAN2 ( xdy, xdx )
	kx(knt) = jx(i-1) + NINT ( jpdist * COS(ptang) )
	ky(knt) = jy(i-1) + NINT ( jpdist * SIN(ptang) )
	DO kkk = 1, knt
	    xx (kkk) = kx (kkk)
	    yy (kkk) = ky (kkk)
	END DO
	CALL GLINE ( 'D', knt, xx, yy, ier )
	kx(1) = kx(knt)
	ky(1) = ky(knt)
	kpdist = ksdist - lsdist
C
	RETURN
	END 
C
	SUBROUTINE MKCPIP ( i, kx, ky, jx, jy, lpdist, kpdist, lmid, 
     +			    jside, knt )
C************************************************************************
C									*
C									*
C									*
C									*
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INTEGER		kx(*), ky(*), jx(*), jy(*)
	LOGICAL		plot
	REAL		xx (55), yy (55)
	PARAMETER	( PI = 3.14159265 )
C------------------------------------------------------------------------
	knt = 1
	ifirst = i
	plot = .true.
	DO WHILE ( kpdist .lt. lpdist )
	    xdx = jx(i+1) - jx(i)
	    xdy = jy(i+1) - jy(i)
	    xds = SQRT ( xdx*xdx + xdy*xdy )
	    ktmp = kpdist
	    kpdist = kpdist + INT ( xds )
	    i = i + 1
	    knt = knt + 1
	    kx(knt) = jx(i)
	    ky(knt) = jy(i)
	END DO
C	
	jpdist = lpdist - ktmp
	ptang = ATAN2 ( xdy, xdx )
	kx(knt) = jx(i-1) + INT ( jpdist * COS(ptang) )
	ky(knt) = jy(i-1) + INT ( jpdist * SIN(ptang) )
	DO kkk = 1, knt
	    xx (kkk) = kx (kkk)
	    yy (kkk) = ky (kkk)
	END DO
	CALL GLINE ( 'D', knt, xx, yy, ier )
	itmpx = kx(knt)
	itmpy = ky(knt)
	dx = kx(knt) - kx(1)
	dy = ky(knt) - ky(1)
	dd = SQRT ( dy*dy + dx*dx )
	dmid = dd / 2.
	angl1 = ATAN2 ( dy, dx )
	angl2 = angl1 - ( jside *  PI / 2. )
	imidx = kx(1) + INT ( dmid * COS(angl1) )
	imidy = ky(1) + INT ( dmid * SIN(angl1) )
	ixrad = INT ( dmid * COS(angl2) )
	iyrad = INT ( dmid * SIN(angl2) )
	knt = knt + 1
	kx(knt) = imidx + ixrad
	ky(knt) = imidy + iyrad
C
	DO kkk = 1, knt
	    xx (kkk) = kx (kkk)
	    yy (kkk) = ky (kkk)
	END DO
	CALL GFILL ( 'D', knt, xx, yy, ier )
	kx(1) = itmpx
	ky(1) = itmpy
	kpdist = kpdist - lpdist
	RETURN
	END
C
	SUBROUTINE MKWPIP ( i, kx, ky, jx, jy, lpdist, kpdist, lmid,
     +			    jside, iside, knt )
C************************************************************************
C									*
C									*
C									*
C									*
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INTEGER		kx(*), ky(*), jx(*), jy(*)
	LOGICAL		plot
	REAL		xx (55), yy (55)
	PARAMETER	( PI = 3.14159265 )
C------------------------------------------------------------------------
	knt = 1
	plot = .true.
	npcr = 55
	DO WHILE ( kpdist .lt. lpdist )
	    xdx = jx(i+1) - jx(i)
	    xdy = jy(i+1) - jy(i)
	    xds = SQRT ( xdx*xdx + xdy*xdy )
	    ktmp = kpdist
	    kpdist = kpdist + INT ( xds )
	    i = i + 1
	    knt = knt + 1
	    kx(knt) = jx(i)
	    ky(knt) = jy(i)
	END DO
C	
	jpdist = lpdist - ktmp
	ptang = ATAN2 ( xdy, xdx )
	kx(knt) = jx(i-1) + INT ( jpdist * COS(ptang) )
	ky(knt) = jy(i-1) + INT ( jpdist * SIN(ptang) )
	DO kkk = 1, knt
	    xx (kkk) = kx (kkk)
	    yy (kkk) = ky (kkk)
	END DO
	CALL GLINE ( 'D', knt, xx, yy, ier )
	itmpx = kx(knt)
	itmpy = ky(knt)
	dx = kx(knt) - kx(1)
	dy = ky(knt) - ky(1)
	dd = SQRT ( dy*dy + dx*dx )
	dmid = dd / 2.
	angl = ATAN2 ( dy, dx )
	imidx = kx(1) + NINT ( dmid * COS(angl) )
	imidy = ky(1) + NINT ( dmid * SIN(angl) )
	pang = iside * jside * ( PI / (npcr-knt-1) )
	cosp = COS (pang)
	sinp = SIN (pang)
	xc = dx / 2
	yc = dy / 2
	ixc = ( ( kx(knt) - kx(1) ) / 2 ) + kx (1)
	iyc = ( ( ky(knt) - ky(1) ) / 2 ) + ky (1)
	jjj = knt + 1
	DO  m = jjj, npcr
	    xcr = cosp * xc - sinp * yc
	    ycr = sinp * xc + cosp * yc
	    xc = xcr	
	    yc = ycr	
	    kx(m) = NINT ( ixc + xc )
	    ky(m) = NINT ( iyc + yc )
	END DO
	kx(npcr) = kx(1)
	ky(npcr) = ky(1)
	DO kkk = 1, npcr
	    xx (kkk) = kx (kkk)
	    yy (kkk) = ky (kkk)
	END DO
	CALL GFILL ( 'D', npcr, xx, yy, ier )
	kx(1) = itmpx
	ky(1) = itmpy
	kpdist = kpdist - lpdist
C
	RETURN
	END
	SUBROUTINE LINESM  ( ipsnum, npts, ixpt, iypt, iret )
C************************************************************************
C* CSMTHN								*
C*									*
C* This subroutine applies a 3-point smoothing to the buffered contour	*
C* line.  The smoothing is performed NSMOTH times.			*
C*									*
C* CSMTHN  ( NSMOTH, NPTS, XPT, YPT, IRET )				*
C*									*
C* Input parameters:							*
C*	NSMOTH		INTEGER		Number of smoothing passes	*
C*	NPTS		INTEGER		Number of points		*
C*									*
C* Input and output parameters:						*
C*	XPT (NPTS)	REAL		X grid points			*
C*	YPT (NPTS)	REAL		Y grid points			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	From old CSMOTH				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	INTEGER		ixpt (*), iypt (*)
C
C------------------------------------------------------------------------
	iret = 0
C
C*	Check that there are at least two points.
C
	IF  ( npts .lt. 2 )  RETURN
C
C*	Loop through the points NSMOTH times.
C
	DO  ii = 1, ipsnum
	  xim1 = ixpt (1)
	  yim1 = iypt (1)
	  DO  i = 2, npts - 1 
	    xi = ixpt (i)
	    yi = iypt (i)
	    ixpt (i) = NINT ( ( xim1 + ixpt (i+1) + xi + xi ) / 4. )
	    iypt (i) = NINT ( ( yim1 + iypt (i+1) + yi + yi ) / 4. )
	    xim1 = xi
	    yim1 = yi
	  END DO
	END DO
C*
	RETURN
	END
