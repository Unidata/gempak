	SUBROUTINE UDMBND  ( xl, yb, xr, yt, rlats, rlonw, rlatn, rlone,
     +			     iret )
C************************************************************************
C* UDMBND								*
C*									*
C* This subroutine determines the range of latitude and longitude	*
C* covered by the rectangular region specified by the input values	*
C* of the L coordinate.  This routine is used for updating map		*
C* projections.								*
C*									*
C* If the dateline is crossed, the full range of longitude is returned. *
C*									*
C* UDMBND  ( XL, YB, XR, YT, RLATS, RLONW, RLATN, RLONE, IRET )		*
C*									*
C* Input parameters:							*
C*	XL		REAL		Left linear coordinate bound	*
C*	YB		REAL		Bottom linear coordinate bound	*
C*	XR		REAL		Right linear coordinate bound	*
C*	YT		REAL		Top linear coordinate bound	*
C*									*
C* Output parameters:							*
C*	RLATS		REAL		Southern-most latitude		*
C*	RLONW		REAL		Western-most longitude		*
C*	RLATN		REAL		Northern-most latitude		*
C*	RLONE		REAL		Eastern-most longitude		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/EMC		 3/96						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xv (101), yv (101)
	REAL		tlt (101), tln (101)
	LOGICAL		datlin
C-------------------------------------------------------------------------
	iret = NORMAL
C*
	datlin = .false.
	rminlt = 1.e31
	rmaxlt = -1.e31
	rminln = 1.e31
	rmaxln = -1.e31
C
C*	Traverse the rectangle computing max/min longitude and latitude.
C
C*	Start with bottom and top edges.
C
	dx = .01 * ( xr - xl )
	y = yb
	DO j = 1, 2
	    x = xl
	    DO i = 1, 101
	        xv (i) = x 
	        yv (i) = y
	        x = x + dx
	    END DO
	    CALL GTRANS ( 'L', 'M', 101, xv, yv, tlt, tln, iret )
	    IF ( iret .ne. 0 ) RETURN
	    DO i = 1, 101
	        IF ( i .ne. 1 ) THEN
		    df = ABS ( tln (i) - tln (i-1) )
C
C*	            Test for a date line crossing.
C
		    IF ( df .gt. 180 ) THEN
		        datlin = .true.
		    END IF
	        END IF
C*
	        IF ( tlt (i) .lt. rminlt ) rminlt = tlt (i)
	        IF ( tlt (i) .gt. rmaxlt ) rmaxlt = tlt (i)
	        IF ( tln (i) .lt. rminln ) rminln = tln (i)
	        IF ( tln (i) .gt. rmaxln ) rmaxln = tln (i)
	    END DO
	    y = yt
	END DO
C
C*	Do the sides.
C
	dy = .01 * ( yt - yb )
	x = xl
	DO j = 1, 2
	    y = yb
	    DO i = 1, 101
	        xv (i) = x 
	        yv (i) = y
	        y = y + dy
	    END DO
	    CALL GTRANS ( 'L', 'M', 101, xv, yv, tlt, tln, iret )
	    IF ( iret .ne. 0 ) RETURN
	    DO i = 1, 101
C
C*	        Test for a date line crossing.
C
	        IF ( i .ne. 1 ) THEN
		    df = ABS ( tln (i) - tln (i-1) )
C
C*	            Test for a date line crossing.
C
		    IF ( df .gt. 180 ) THEN
		        datlin = .true.
		    END IF
	        END IF
C*
	        IF ( tlt (i) .lt. rminlt ) rminlt = tlt (i)
	        IF ( tlt (i) .gt. rmaxlt ) rmaxlt = tlt (i)
	        IF ( tln (i) .lt. rminln ) rminln = tln (i)
	        IF ( tln (i) .gt. rmaxln ) rmaxln = tln (i)
	    END DO
	    x = xr
	END DO
C
C*	Round to nearest whole latitude and pad one deg.
C
	rminlt = FLOAT ( NINT ( rminlt ) ) - 1.
	rmaxlt = FLOAT ( NINT ( rmaxlt ) ) + 1.
	IF ( rminlt .lt. -90. ) rminlt = -90.
	IF ( rmaxlt .gt. 90.  ) rmaxlt =  90.
C
C*	Check to see if a Pole is inside.
C
	CALL GTRANS ( 'M', 'L', 1, -90., 0., xp, yp, ier )
	IF ( ( ( xp .le. xr .and. xp .ge. xl ) .or.
     +	       ( xp .ge. xr .and. xp .le. xl ) ) .and.
     +	     ( ( yp .le. yt .and. yp .ge. yb ) .or.
     +	       ( yp .ge. yt .and. yp .le. yb ) ) ) THEN
	    rminlt = -90.
	    datlin = .true.
	END IF
	CALL GTRANS ( 'M', 'L', 1, 90., 0., xp, yp, ier )
	IF ( ( ( xp .le. xr .and. xp .ge. xl ) .or.
     +	       ( xp .ge. xr .and. xp .le. xl ) ) .and.
     +	     ( ( yp .le. yt .and. yp .ge. yb ) .or.
     +	       ( yp .ge. yt .and. yp .le. yb ) ) ) THEN
	    rmaxlt = 90.
	    datlin = .true.
	END IF
C
C*	Set full range for date line crossing.
C
	IF ( datlin ) THEN
	    rminln = -180.
	    rmaxln =  180.
	ELSE
C
C*	    Round to nearest whole longitude and pad one deg.
C
	    rminln = FLOAT ( NINT ( rminln ) ) - 1.
	    rmaxln = FLOAT ( NINT ( rmaxln ) ) + 1.
	END IF
	rlats = rminlt
	rlonw = rminln
	rlatn = rmaxlt
	rlone = rmaxln
C*
	RETURN
	END
