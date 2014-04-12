	SUBROUTINE GH_KGAT ( xlat, xlon, npnts, dside, mxpts, rlat,
     +			     rlon, nptot, iret )
C************************************************************************
C* GH_KGAT 								*
C*									*
C* This subroutine calculates the boundary points for an area defined   *
C* as the average track error for the forecast positions of a tropical  *
C* storm for either the Atlantic or Pacific Oceans.			*
C*                                                                      *
C* GH_KGAT ( XLAT, XLON, NPNTS, DSIDE, MXPTS, RLAT, RLON, NPTOT, IRET )	*
C*									*
C* Input parameters:							*
C*	XLAT(*)		REAL		Latitude points of axis lines   *
C*	XLON(*)		REAL		Longitude points of axis lines  *
C*	NPNTS		INTEGER		Number of lat/lon points        *
C*	DSIDE(*)	REAL		Track error distances (nm)      *
C*	MXPTS		INTEGER		Maximum number of bound points  *
C*									*
C* Output parameters:							*
C*	RLAT(*)		REAL		Latitude points of boundary     *
C*	RLON(*)		REAL		Longitude points of boundary    *
C*	NPTOT 		INTEGER		Number of points in boundary    *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01	Modified from IS_ESOL                   *
C* D. Kidwell/NCEP	 6/01	Handled dup points & errtrk over init pt*
C* D. Kidwell/NCEP	 7/01	Changed angle check from 1 to 5 degrees *
C* D. Kidwell/NCEP	10/01	Rewrote to interpolate for smoothing    *
C* D. Kidwell/NCEP	 2/02	Removed idrop from GG_TCSH call sequence*
C* D. Kidwell/NCEP	 3/03	Increased num. points; check msg dside  *
C* D. Kidwell/NCEP	 5/03	Added argument iptsm to GG_TCSM call    *
C* S. Jacobs/NCEP	 4/13	Increased working array sizes		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        PARAMETER       ( MXINTP = 2000, MXWORK = 20000, INCDEG = 3 )
	PARAMETER	( NUMHR = 8 )
	REAL		xlat (*), xlon (*), rlat (*), rlon (*),
     +	                dside (*)
C*
        LOGICAL         okay
	REAL		side (LLMXPT), tlat (LLMXPT), tlon (LLMXPT), 
     +			tside (LLMXPT),
     +			dlat (MXINTP), dlon (MXINTP), xside (MXINTP),
     +			blat (361), blon (361), olat (MXWORK), 
     +			olon (MXWORK), wlat (MXWORK), wlon (MXWORK)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
	npos  = npnts
	nsmth = 10
	iptsm = 3
C
        DO ii = npnts, 1, -1
	    IF ( .not. ERMISS ( dside ( ii ) ) ) THEN
	        tside ( ii ) = PR_HGNM ( dside ( ii ) )
	      ELSE
		npos = ii - 1
	    END IF
        END DO
C
C*	Check for and eliminate small areas completely contained in 
C*	larger areas.
C
	np = 0
	IF ( npos .gt. 1 ) THEN
	    DO ii = 1, npos - 1
	        okay = .true.
	        DO jj = ii + 1, npos
		    CALL CLO_DIST ( xlat ( ii ), xlon ( ii ), 1,
     +				xlat ( jj ), xlon ( jj ), dist, ier )
		    IF ( ( dist + tside ( ii ) ) .le. tside ( jj ) ) 
     +		           okay = .false.
	        END DO
	        IF ( okay ) THEN
		    np = np + 1
	            tlat ( np ) = xlat ( ii )
	            tlon ( np ) = xlon ( ii )
	            side ( np ) = tside ( ii )
	        END IF
	    END DO
	END IF
C
        np = np + 1
	tlat ( np ) = xlat ( npos )
	tlon ( np ) = xlon ( npos )
	side ( np ) = tside ( npos )
C
C*	Interpolate track points and radii for a smooth plot.
C
	ipt = 1
	DO ip = 1, np
	    dlat ( ipt )  = tlat ( ip )
	    dlon ( ipt )  = tlon ( ip )
	    xside ( ipt ) = side ( ip )
	    IF ( ip .lt. np ) THEN
	        CALL CLO_DIST ( tlat ( ip ), tlon ( ip ), 1,
     +				tlat ( ip + 1 ), tlon ( ip + 1 ),
     +			        dd, ier )
		radavg = ( side ( ip ) + side ( ip + 1 ) ) * .5
	        intrp  = NINT ( dd / radavg + .6 ) + 1
		rintvl = FLOAT ( intrp + 1 ) 
		delta  = dd / rintvl
		CALL CLO_DIRECT ( tlat ( ip+1 ), tlon ( ip+1 ),
     +				  tlat ( ip ), tlon ( ip ),
     +				  ddir, ier )
		ddist = delta
		DO ii = 1, intrp
		    ipt = ipt + 1
		    CALL CLO_DLTLN ( tlat ( ip ), tlon ( ip ),
     +				     ddist, ddir, dlat ( ipt ),
     +				     dlon ( ipt ), ier )
		    wt2 = FLOAT ( ii ) / rintvl 
		    wt1 = 1. - wt2
	 	    xside ( ipt ) = side ( ip ) * wt1 + 
     +				    side ( ip + 1 ) * wt2
   		    ddist = ddist + delta
		END DO
	        ipt = ipt + 1    
	    END IF
	END DO
C
C*      Calculate average track error boundary.
C
        DO ip = 1, ipt
C
	    curlat = dlat ( ip ) 
	    curlon = dlon ( ip ) 
            dist   = xside ( ip )
            icnt   = 0
C
C*	    Get the coordinates for an interpolated circle.
C
            DO ideg = 0, 360, INCDEG
                icnt = icnt + 1
                bear = FLOAT ( ideg )
                CALL CLO_DLTLN ( curlat, curlon, dist, bear,
     +                           blat ( icnt ), blon ( icnt ), ier )
            END DO
	    blat ( icnt ) = blat ( 1 )
 	    blon ( icnt ) = blon ( 1 )
C
	    IF ( ip .eq. 1 ) THEN
C
C*	        Save the first polygon.
C
		DO ii = 1, icnt
		    wlat ( ii ) = blat ( ii )
		    wlon ( ii ) = blon ( ii ) 
		END DO
		nptot = icnt
	      ELSE
C
C*	        Combine the new polygon, defined by (blat, blon, icnt),
C*	        with the preceding one (wlat, wlon, nptot).  The points
C*	        for the combined polygons are defined by
C*	        (olat, olon, npts).
C
	        CALL GG_TCSH ( wlat, wlon, nptot, blat, blon, icnt, 
     +			       olat, olon, npts, ier )
C
C*	        Move the combined polygons to the first polygon array.
C
		nptot = npts
		DO ii = 1, npts
		    wlat ( ii ) = olat ( ii )
		    wlon ( ii ) = olon ( ii ) 
		END DO
	    END IF
	END DO
C
C*	If necessary, thin the points for the area.  mxpts is currently
C*	set to the vgf value of MAXPTS defined in vgstruct.h
C
	iskip = nptot / mxpts + 1
	IF ( ( iskip * mxpts ) .eq. nptot ) iskip = iskip - 1
	nn    = 0
	DO ii = 1, nptot, iskip
	    IF ( nn .lt. mxpts ) nn = nn + 1
	    rlat ( nn ) = wlat ( ii ) 
	    rlon ( nn ) = wlon ( ii )
	END DO
C
	nptot = nn
	rlat ( nptot ) = rlat ( 1 )
	rlon ( nptot ) = rlon ( 1 )
C
C*	Apply a smoother.
C
  	IF ( ipt .gt. 1 ) THEN 
 	    CALL GG_TCSM ( nsmth, nptot, iptsm, rlat, rlon, ier )
C
C*	    Set the final lat/lon position equal to the first position.
C
            rlat ( nptot ) = rlat ( 1 ) 
 	    rlon ( nptot ) = rlon ( 1 )
 	END IF
C*
	RETURN
	END
