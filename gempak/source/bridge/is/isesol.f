	SUBROUTINE IS_ESOL ( tlat, tlon, np, iside, rlat, rlon, npt,
     +			     iret )
C************************************************************************
C* IS_ESOL 								*
C*									*
C* This subroutine calculates the boundary points for an area defined   *
C* as being on either side of a line.                                   *
C*                                                                      *
C* IS_ESOL ( TLAT, TLON, NP, ISIDE, RLAT, RLON, NPT, IRET )             *
C*									*
C* Input parameters:							*
C*	TLAT(*)		REAL		Latitude points of axis lines   *
C*	TLON(*)		REAL		Longitude points of axis lines  *
C*	NP		INTEGER		Number of points                *
C*	ISIDE		INTEGER		Dist. (nm) either side of line  *
C*									*
C* Output parameters:							*
C*	RLAT(*)		REAL		Latitude points of boundary     *
C*	RLON(*)		REAL		Longitude points of boundary    *
C*	NPT 		INTEGER		Number of points in boundary    *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	10/99	Replaced is_ calls with clo_ calls;     *
C*				checked for crossing dateline, equator  *
C* M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
C* D. Kidwell/NCEP	11/99	Rewrote based on cds_sig; called PR_HGNM*
C* D. Kidwell/NCEP	 1/00	Added error check for IS_INTX call      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		tlat (*), tlon (*), rlat (*), rlon (*)
C*
	REAL		s1lat (2), s1lon (2), s2lat (2), s2lon (2)
	LOGICAL		intrp
C------------------------------------------------------------------------
	iret = 0
	side = PR_HGNM ( FLOAT ( iside ) )
	npt  = np * 2
C
C*	Get the points on either side of the first point.
C
	CALL CLO_DIRECT ( tlat ( 2 ), tlon ( 2 ), tlat ( 1 ),
     +			  tlon ( 1 ), ang1, ier )
	ang1 = ang1 - 90.
	CALL CLO_DLTLN ( tlat ( 1 ), tlon ( 1 ), side, ang1, 
     +			 rlat ( npt ), rlon ( npt ), ier )
	ang1 = ang1 - 180.
	CALL CLO_DLTLN ( tlat ( 1 ), tlon ( 1 ), side, ang1,
     +			 rlat ( 1 ), rlon ( 1 ), ier )
C
C*	Get the points on either side of the interior points.
C
	DO ii = 2, np - 1
	    CALL CLO_DIRECT ( tlat ( ii-1 ), tlon ( ii-1 ), tlat ( ii ),
     +			      tlon ( ii ), ang1, ier )
	    ang1 = MOD ( ( ang1 + 270. ), 360. )
	    CALL CLO_DLTLN ( tlat ( ii ), tlon ( ii ), side, ang1, 
     +			     s1lat ( 2 ), s1lon ( 2 ), ier )
	    CALL CLO_DIRECT ( tlat ( ii+1 ), tlon ( ii+1 ), tlat ( ii ),
     +			      tlon ( ii ), ang2, ier )
	    ang2 = MOD ( ( ang2 + 90. ), 360. )
	    CALL CLO_DLTLN ( tlat ( ii ), tlon ( ii ), side, ang2,
     +			     s2lat ( 1 ), s2lon ( 1 ), ier )
C
	    intrp = .false.
	    IF ( ABS ( ang1 - ang2 ) .gt. 1 ) THEN
		CALL CLO_DLTLN ( tlat ( ii-1 ), tlon ( ii-1 ), side, 
     +				 ang1, s1lat ( 1 ), s1lon ( 1 ), ier )
		CALL CLO_DLTLN ( tlat ( ii+1 ), tlon ( ii+1 ), side,
     +				 ang2, s2lat ( 2 ), s2lon ( 2 ), ier )
		CALL IS_INTX ( s1lat, s1lon, s2lat, s2lon, xint, yint,
     +			       ier )
		IF ( ier .lt. 0 ) intrp = .true.
	      ELSE
		intrp = .true.
	    END IF
	    IF ( intrp ) THEN
		xint = ( s1lat ( 2 ) + s2lat ( 1 ) ) * .5
		yint = ( s1lon ( 2 ) + s2lon ( 1 ) ) * .5
	    END IF
C
	    rlat ( ii ) = xint
	    rlon ( ii ) = yint
C
	    ang1 = MOD ( ( ang1 + 180. ), 360. )
	    ang2 = MOD ( ( ang2 + 180. ), 360. )
	    CALL CLO_DLTLN ( tlat ( ii ), tlon ( ii ), side, ang1,
     +			     s1lat ( 2 ), s1lon ( 2 ), ier )
	    CALL CLO_DLTLN ( tlat ( ii ), tlon ( ii ), side, ang2,
     +			     s2lat ( 1 ), s2lon ( 1 ), ier )
C
	    intrp = .false.
	    IF ( ABS ( ang1 - ang2 ) .gt. 1 ) THEN
		CALL CLO_DLTLN ( tlat ( ii-1 ), tlon ( ii-1 ), side, 
     +				 ang1, s1lat ( 1 ), s1lon ( 1 ), ier )
		CALL CLO_DLTLN ( tlat ( ii+1 ), tlon ( ii+1 ), side,
     +				 ang2, s2lat ( 2 ), s2lon ( 2 ), ier )
		CALL IS_INTX ( s1lat, s1lon, s2lat, s2lon, xint, yint,
     +			       ier )
		IF ( ier .lt. 0 ) intrp = .true.
	      ELSE
		intrp = .true.
	    END IF
	    IF ( intrp ) THEN
		xint = ( s1lat ( 2 ) + s2lat ( 1 ) ) * .5
		yint = ( s1lon ( 2 ) + s2lon ( 1 ) ) * .5
	    END IF
C
	    kk = npt - ii + 1
	    rlat ( kk ) = xint
	    rlon ( kk ) = yint
	END DO
C
C*	Get the points on either side of the last point.
C
	CALL CLO_DIRECT ( tlat ( np-1 ), tlon ( np-1 ), tlat ( np ),
     +			  tlon ( np ), ang2, ier )
	ang2 = ang2 - 90.
	CALL CLO_DLTLN ( tlat ( np ), tlon ( np ), side, ang2, 
     +			 rlat ( np ), rlon ( np ), ier )
	ang2 = MOD ( ( ang2 + 180. ), 360. )
	CALL CLO_DLTLN ( tlat ( np ), tlon ( np ), side, ang2,
     +			 rlat ( np + 1 ), rlon ( np + 1 ), ier )
C*
	RETURN
	END
