	SUBROUTINE GG_TCDA ( rlat, rlon, f34kt, ocean, radii, elat,
     +			     elon, iret )
C************************************************************************
C* GG_TCDA								*
C*									*
C* This subroutine plots the danger area for a single tropical system.  *
C* The danger area is determined by adding 50-100-150-200-300 nm (for   *
C* the TPC) or 50-80-125-180-250 nm (for the CPHC) to the 12-24-36-48-72*
C* hour forecast position and maximum 34kt wind radii respectively, and *
C* by the 34kt wind radii at the current position.                      *
C*									*
C* GG_TCDA ( RLAT, RLON, F34KT, OCEAN, RADII, ELAT, ELON, IRET )        *
C*									*
C* Input parameters:							*
C*	RLAT (6)    	REAL	    Track point latitudes               *
C*	RLON (6)   	REAL	    Track point longitudes              *
C*	F34KT (6)	CHAR*	    Current and fcst 34kt wind radii    *
C*	OCEAN		CHAR*	    Basin indicator - AL, EP or CP      *
C*	RADII (6)	REAL	    Values to be added to 34kt winds    *
C*									*
C* Output parameters:							*
C*	ELAT (4)	REAL	    Latitudes of danger area extrema    *
C*	ELON (4)	REAL	    Longitudes of danger area extrema   *
C*	IRET		INTEGER	    Return code			        *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/01	                                        *
C* D. Kidwell/NCEP	 2/02	Rewrote to improve interpolation &      *
C*				smoothing; added CPHC and extrema       *
C* D. Kidwell/NCEP	 5/03	Added argument iptsm to GG_TCSM call    *
C* D. Kidwell/NCEP	 5/04	Initialized elat, elon                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	f34kt (*), ocean
	REAL		rlat (*), rlon (*), elat (*), elon (*),
     +			radii (*)
C*
C*	MXPTS should have the same value as MAXPTS in gemprm.h
C*
	PARAMETER	( ITRACK = 6, MXINTP = 1000, MXWORK = 10000,
     +			  MXPTS = 500, IANG1 = 0, IANG2 = 90 ,
     +			  IPOLY = ( (IANG2-IANG1+1) * 4 ),
     +			  RADCUR = 40. )
C*
	CHARACTER	ddev*2, sys*2
	INTEGER		intarr (5), ino12 (IPOLY), ino21 (IPOLY),
     +			ipt1 (2,100), ipt2 (2,100), iptsv (ITRACK)
	LOGICAL		good (ITRACK), out
	REAL		tlat (ITRACK), tlon (ITRACK), quad (4,ITRACK),
     +			angle (4), dist (5),
     +			xlat (IPOLY,ITRACK), xlon (IPOLY,ITRACK),
     +			dlat (MXINTP), dlon (MXINTP),
     +			dse (MXINTP), dne (MXINTP), dnw (MXINTP),
     +			dsw (MXINTP), wlat (MXWORK), wlon (MXWORK),
     +			olat (MXWORK), olon (MXWORK),
     +			blat (IPOLY), blon (IPOLY)
C*
	INCLUDE		'ERMISS.FNC'
C*
	DATA		angle / 45., 135., 225., 315. /
C-----------------------------------------------------------------------
	iret = 0
	DO ii = 1, 4
	    elat ( ii ) = RMISSD
	    elon ( ii ) = RMISSD
	END DO
C
	CALL GQLINE ( jltyp, jlthw, jwidth, jlwhw, ier )
C
	DO ii = 1, ITRACK
	    tlat ( ii ) = rlat ( ii )
	    tlon ( ii ) = rlon ( ii )
	END DO
C
C*	Check for systems above 60N (Atlantic) or 40N (Pacific).
C
	IF ( ocean .eq. 'AL' ) THEN
	    xmxlat = 60.
	  ELSE
	    xmxlat = 40.
	END IF
	IF ( tlat ( ITRACK ) .gt. xmxlat ) tlat ( ITRACK ) = RMISSD
	IF ( ERMISS ( tlat ( ITRACK ) ) ) THEN
	    out = .true.
	    ii  = ITRACK - 1
	    DO WHILE ( out )
		IF ( tlat ( ii ) .gt. xmxlat ) THEN
		    tlat ( ii ) = RMISSD
		  ELSE
		    IF ( .not. ERMISS ( tlat ( ii ) ) ) out = .false.
		END IF
		ii = ii - 1
		IF ( ii .lt. 1 ) out = .false.
	    END DO
	END IF
C
C*	Check for systems west of the dateline and do not plot the
C*	danger area.
C
	IF ( ocean .ne. 'AL' ) THEN
	    ii = 1
	    out = .true.
	    DO WHILE ( out )
		IF ( tlon ( ii ) .gt. 0. ) THEN
		    DO jj = ii, ITRACK 
			tlon ( jj ) = RMISSD
		    END DO
		    out = .false.
		  ELSE
		    ii = ii + 1
		    IF ( ii .gt. ITRACK ) out = .false.
		END IF
	    END DO
	END IF
C
C*	Get the quadrant radii for the danger area.
C
	out = .false.
	DO ii = 1, ITRACK
	    CALL ST_C2I ( f34kt ( ii ), 5, intarr, num, ier )
	    IF ( ( ier .lt. 0 ) .or. ( num .lt. 5 ) .or.
     +		 ERMISS ( tlat (ii) ) .or. ERMISS ( tlon (ii) ) ) THEN
		good ( ii ) = .false.
	      ELSE
		good ( ii ) = .true.
		DO jj = 1, 4
		    IF ( intarr ( jj + 1 ) .ge. 0 ) THEN
		        quad ( jj, ii ) = FLOAT ( intarr ( jj+1 ) ) +
     +					  radii ( ii )
		      ELSE
			quad ( jj, ii ) = radii ( ii )
			IF ( ii .eq. 2 ) out = .true.
		    END IF
		    IF ( ii .eq. 1 ) THEN
			IF ( quad ( jj, ii ) .lt. RADCUR )
     +			     quad ( jj, ii ) = RADCUR
		    END IF
		END DO
		IF ( ( ii .gt. 2 ) .and. out ) good ( 2 ) = .false.
	    END IF
	END DO
C
C*	Loop to get the bounds for each track point.
C*      This section of code was adapted from routine GH_SWLN.
C
       	DO ii = 1, ITRACK
            IF ( good ( ii ) ) THEN
		dist ( 1 ) = quad ( 2, ii ) 
		dist ( 2 ) = quad ( 1, ii )
		dist ( 3 ) = quad ( 4, ii ) 
		dist ( 4 ) = quad ( 3, ii ) 
		dist ( 5 ) = dist ( 1 )
C
C*	 	Loop through each quadrant of a track point.
C
                nn = 0
                DO jj = 1, 4
                    DO iangle = IANG1, IANG2
                        nn = nn + 1
			theta = FLOAT ( iangle ) 
			rtheta = DTR * theta
                        rad = COS (rtheta) * COS (rtheta) * dist (jj) 
     +                      + SIN (rtheta) * SIN (rtheta) * dist (jj+1)
C
			rtheta = DTR * ( theta + 270.0 + angle ( jj ) )
                        y1 = rad * SIN ( rtheta ) / 60.0
                        x1 = (-rad) * COS ( rtheta ) / 60.0 
     +                       / COS ( DTR * tlat ( ii ) )
C
                        xlat ( nn, ii ) = tlat ( ii ) + y1
                        xlon ( nn, ii ) = tlon ( ii ) - x1
                    END DO
                END DO
	    END IF
	END DO
C
C*      Determine if the bounds for one track point lie completely
C*	inside the bounds for another.  If so, the inner bounds can be
C*	eliminated.
C
        sys = 'M'
        CALL ST_NULL ( sys, sys, lens, ier )
C
	DO ii = 1, ITRACK - 1
	    IF ( good ( ii ) ) THEN
		DO jj = ii + 1, ITRACK
		    IF ( good ( jj ) ) THEN
                        CALL CGR_INPOLY ( sys, IPOLY, xlat (1,ii),
     +				   xlon (1,ii), sys, IPOLY, xlat (1,jj),
     +				   xlon (1,jj), ino12, ier )
        		CALL GG_TCEP ( xlat (1,ii), xlon (1,ii), IPOLY,
     +				       ino12, ipt1, nseg1, ier )
        		IF ( nseg1 .eq. 0 ) THEN
            		    good ( ii ) = .false.
          		  ELSE
            		    CALL CGR_INPOLY ( sys, IPOLY, xlat (1,jj),
     +			           xlon (1,jj), sys, IPOLY, xlat (1,ii),
     +                             xlon (1,ii), ino21, ier )
            		    CALL GG_TCEP ( xlat (1,jj), xlon (1,jj),
     +			                IPOLY, ino21, ipt2, nseg2, ier )
            		    IF ( nseg2 .eq. 0 ) THEN
                		good ( jj ) = .false.
            		    END IF
        		END IF
		    END IF
		END DO
	    END IF
	END DO
C
	CALL GQDEV ( ddev, junit, jatyp, ier )
	IF ( ddev .ne. 'PS' ) THEN
	    ilwid = 4
	  ELSE
	    ilwid = 8
	END IF
 	CALL GSLINE ( 0, 0, ilwid, 0, ier ) 
C
C*	Get all the "good" track points consecutively for ease of
C*	interpolation and plotting.
C
	igood = 0
	DO ii = 1, ITRACK
	    IF ( good ( ii ) ) THEN
		igood = igood + 1
		tlat ( igood ) = tlat ( ii )
		tlon ( igood ) = tlon ( ii )
		DO jj = 1, 4
		    quad ( jj, igood ) = quad ( jj, ii )
		END DO
		DO jj = 1, IPOLY
		    xlat ( jj, igood ) = xlat ( jj, ii )
		    xlon ( jj, igood ) = xlon ( jj, ii )
		END DO
	    END IF
	END DO
C
	IF ( igood .eq. 0 ) THEN
	    RETURN
	END IF
C
C*	Interpolate the radii between the polygons for a smooth 
C*	boundary.
C
	ipt = 1
	DO ip = 1, igood
	    dlat ( ipt ) = tlat ( ip )
	    dlon ( ipt ) = tlon ( ip )	
	    dse ( ipt )  = quad ( 2, ip )
	    dne ( ipt )  = quad ( 1, ip )
	    dnw ( ipt )  = quad ( 4, ip ) 
	    dsw ( ipt )  = quad ( 3, ip ) 
	    iptsv ( ip ) = ipt
C
	    IF ( ip .lt. igood ) THEN
		CALL CLO_DIST ( tlat (ip), tlon (ip), 1, tlat (ip+1),
     +				tlon (ip+1), dd, ier )
		IF ( dd .gt. 5000. ) THEN
		    radmin = AMIN1 ( quad (1,ip), quad (2,ip),
     +				     quad (3,ip), quad (4,ip),
     +				     quad (1,ip+1), quad (2,ip+1),
     +				     quad (3,ip+1), quad (4,ip+1) )
		    radavg = NINT ( ( quad (1,ip) + quad (2,ip) +
     +				      quad (3,ip) + quad (4,ip) + 
     +				      quad (1,ip+1) + quad (2,ip+1) +
     + 				      quad (3,ip+1) + quad (4,ip+1) )
     +				      / 8. )
		    space = PR_HGNM ( radavg )
                    intrp = NINT ( dd / space + .6 )
                    incr  = NINT ( radavg - radmin ) / 9
                    intrp = intrp + incr
		    IF ( ( ipt + intrp ) .gt. MXINTP ) 
     +				 intrp = MXINTP - ipt
                    intvl = intrp + 1
                    delta = dd / FLOAT ( intvl )
                    CALL CLO_DIRECT ( tlat (ip+1), tlon (ip+1),
     +                             tlat ( ip ), tlon ( ip ), ddir, ier )
                    ddist = delta
                    DO ii = 1, intrp
                        ipt = ipt + 1
                        CALL CLO_DLTLN ( tlat (ip), tlon (ip), ddist,
     +                             ddir, dlat (ipt), dlon (ipt), ier )
                        wt2 = FLOAT ( ii ) / FLOAT ( intvl )
                        wt1 = 1. - wt2
                        dse ( ipt ) = quad ( 2,ip ) * wt1 +
     +                                quad ( 2, ip+1 ) * wt2
                        dne ( ipt ) = quad ( 1,ip ) * wt1 +
     +                                quad ( 1, ip+1 ) * wt2
                        dnw ( ipt ) = quad ( 4,ip ) * wt1 +
     +                                quad ( 4, ip+1 ) * wt2
                        dsw ( ipt ) = quad ( 3,ip ) * wt1 +
     +                                quad ( 3, ip+1 ) * wt2
                        ddist = ddist + delta
                    END DO
                END IF
                ipt = ipt + 1
		IF ( ipt .gt. MXINTP ) THEN
		    ipt = MXINTP
		END IF
            END IF
        END DO
C
C*      Calculate polygon bounds along the interpolated track.
C
	jj = 1
        DO ip = 1, ipt
C
	    IF ( ip .eq. iptsv ( jj ) ) THEN
C
C*		Get the bounds for the track point.
C
		DO ii = 1, IPOLY
		    blat ( ii ) = xlat ( ii, jj )
		    blon ( ii ) = xlon ( ii, jj ) 
		END DO
		jj = jj + 1
	      ELSE
C
C*		Calculate the bounds for an interpolated track point.
C
                dist ( 1 ) = dse ( ip )
                dist ( 2 ) = dne ( ip )
                dist ( 3 ) = dnw ( ip )
                dist ( 4 ) = dsw ( ip )
                dist ( 5 ) = dist (1)
C
C*	        Loop through each quadrant of the polygon.  The radii
C*              are currently calculated to generate a smooth contour
C*              when sweeping from one quadrant to the next in 1 degree
C*	        azimuthal increments.  
C
                nn = 0
                DO ii = 1, 4
                    DO iangle  = IANG1, IANG2
                        nn     = nn + 1
		        theta  = FLOAT ( iangle )
		        rtheta = DTR * theta
                        rad    = COS (rtheta) * COS (rtheta) * dist (ii) 
     +                     + SIN (rtheta) * SIN (rtheta) * dist (ii + 1)
C
		        rtheta = DTR * ( theta + 270.0 + angle(ii) )
                        y1     = rad * SIN ( rtheta ) / 60.0
                        x1     = (-rad) * COS ( rtheta ) / 60.0
     +                           / COS ( DTR * dlat ( ip ) )
C
                        blat ( nn ) = dlat ( ip ) + y1
                        blon ( nn ) = dlon ( ip ) - x1
                    END DO
                END DO
	    END IF
C
	    IF ( ip .eq. 1 ) THEN
C
C*		Save the first polygon.
C
		DO ii = 1, IPOLY
		    wlat ( ii ) = blat ( ii )
		    wlon ( ii ) = blon ( ii ) 
		END DO
		npt = IPOLY
	      ELSE
C
C*	        Combine the new polygon, defined by (blat, blon, IPOLY),
C*	        with the preceding one (wlat, wlon, npt).  The points
C*	        for the combined polygons are defined by (olat, olon, 
C*		npts).
C
	        CALL GG_TCSH ( wlat, wlon, npt, blat, blon, IPOLY,
     +			       olat, olon, npts, ier )
		IF ( ier .eq. 0 ) THEN
C
C*		    The polygons intersect.  Move the combined
C*		    polygons to the first polygon array.
C
		    iskip = 1
		    IF ( npts .ge. ( MXWORK - IPOLY ) ) iskip = 2
		    nn = 0
		    DO ii = 1, npts, iskip
			nn = nn + 1
			wlat ( nn ) = olat ( ii )
			wlon ( nn ) = olon ( ii ) 
		    END DO
		    npt = nn
	    	    wlat ( npt ) = wlat ( 1 )
	    	    wlon ( npt ) = wlon ( 1 )
		  ELSE
C
C*		    The polygons do not intersect.
C
		    CALL ER_WMSG ( 'GG', 4, ' ', ier )
		END IF
	    END IF
	END DO
C
	IF ( npt .gt. MXPTS ) THEN
C
C*	    Thin the points for vgf.
C
	    iskip = ( npt - 1 ) / MXPTS + 1
	    nn    = 0
	    DO ii = 1, npt, iskip
	        nn = nn + 1
		wlat ( nn ) = wlat ( ii ) 
		wlon ( nn ) = wlon ( ii )
	    END DO
	    IF ( nn .gt. MXPTS ) nn = MXPTS
	    npt = nn
	    wlat ( npt ) = wlat ( 1 )
	    wlon ( npt ) = wlon ( 1 )
	END IF
C
	nsmth = 50
	iptsm = 5
	CALL GG_TCSM ( nsmth, npt, iptsm, wlat, wlon, ier )
	CALL GFILL ( 'M', npt, wlat, wlon, ier )
	IF ( ddev .ne. 'VG' ) THEN
C
C*	    The above GFILL call generates bounding lines for the
C*	    polygon for vgf, but not for other devices.
C
   	    CALL GLINE ( 'M', npt, wlat, wlon, ier )
	END IF
	CALL GSLINE ( jltyp, jlthw, jwidth, jlwhw, ier )
C
C*	Get the bottommost, rightmost, topmost and leftmost points.
C
	elat ( 1 ) = wlat ( 1 )
	elat ( 3 ) = wlat ( 1 )
	elon ( 2 ) = wlon ( 1 )
	IF ( elon ( 2 ) .gt. 0. ) elon ( 2 ) = -180.
	elon ( 4 ) = wlon ( 1 )
	i1 = 1
	i2 = 1
	i3 = 1
	i4 = 1
	DO ii = 2, npt
	    IF ( wlat ( ii ) .lt. elat ( 1 ) ) THEN
		elat ( 1 ) = wlat ( ii )
		i1 = ii
	    END IF
	    IF ( wlat ( ii ) .gt. elat ( 3 ) ) THEN
		elat ( 3 ) = wlat ( ii )
		i3 = ii
	    END IF
	    IF ( ( wlon ( ii ) .gt. elon ( 2 ) ) .and. 
     +		 ( wlon ( ii ) .le. 0. ) ) THEN
		elon ( 2 ) = wlon ( ii )
		i2 = ii
	    END IF
	    IF ( wlon ( ii ) .lt. elon ( 4 ) ) THEN
		elon ( 4 ) = wlon ( ii )
		i4 = ii
	    END IF
	END DO
	elat ( 2 ) = wlat ( i2 )
	elat ( 4 ) = wlat ( i4 ) 
	elon ( 1 ) = wlon ( i1 )
	elon ( 3 ) = wlon ( i3 )
C*
	RETURN
	END
