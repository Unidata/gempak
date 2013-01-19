	SUBROUTINE IS_INTX ( y1, x1, y2, x2, yy, xx, iret )
C************************************************************************
C* IS_INTX 								*
C*									*
C* This subroutine calculates the intersection point of two lines whose *
C* coordinates are latitudes and longitudes.                            *
C*                                                                      *
C* IS_INTX ( Y1, X1, Y2, X2, YY, XX, IRET )                             *
C*									*
C* Input parameters:							*
C*	Y1(*)		REAL		Latitude points of first line   *
C*	X1(*)		REAL		Longitude points of first line  *
C*	Y2(*)		REAL		Latitude points of second line  *
C*	X2(*)		REAL		Longitude points of second line *
C*									*
C* Output parameters:							*
C*	YY		REAL		Latitude point of intersection  *
C*	XX		REAL		Longitude point of intersection *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = points not calculable     *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	11/99	                                        *
C* D. Kidwell/NCEP	 1/00	Added error check                       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		y1 (*), x1 (*), y2 (*), x2 (*)
C*
	REAL		xx1 (2), xx2 (2)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	yy   = RMISSD
	xx   = RMISSD
C
	DO ii = 1, 2
	    xx1 ( ii ) = x1 ( ii )
	    xx2 ( ii ) = x2 ( ii )
	    IF ( xx1 ( ii ) .lt. 0. ) xx1 ( ii ) = xx1 ( ii ) + 360.
	    IF ( xx2 ( ii ) .lt. 0. ) xx2 ( ii ) = xx2 ( ii ) + 360.
	END DO
C
	IF ( xx1 ( 1 ) .eq. xx1 ( 2 ) ) THEN
	    xx = xx1 ( 1 )
	    IF ( xx2 ( 1 ) .ne. xx2 ( 2 ) ) THEN
		s2 = ( y2 ( 2 ) - y2 ( 1 ) ) / ( xx2 ( 2 ) - xx2 ( 1 ) )
		b2 = y2 ( 1 ) - s2 * xx2 ( 1 )
		yy = s2 * xx + b2
	    END IF
	  ELSE IF ( xx2 ( 1 ) .eq. xx2 ( 2 ) ) THEN
	    xx = xx2 ( 1 )
	    s1 = ( y1 ( 2 ) - y1 ( 1 ) ) / ( xx1 ( 2 ) - xx1 ( 1 ) )
	    b1 = y1 ( 1 ) - s1 * xx1 ( 1 )
	    yy = s1 * xx + b1
	  ELSE
	    s1 = ( y1 ( 2 ) - y1 ( 1 ) ) / ( xx1 ( 2 ) - xx1 ( 1 ) ) 
	    b1 = y1 ( 1 ) - s1 * xx1 ( 1 )
	    s2 = ( y2 ( 2 ) - y2 ( 1 ) ) / ( xx2 ( 2 ) - xx2 ( 1 ) ) 
	    b2 = y2 ( 1 ) - s2 * xx2 ( 1 ) 
	    IF ( s1 .ne. s2 ) THEN
		xx = ( b2 - b1 ) / ( s1 - s2 )
		yy = s1 * xx + b1
	    END IF
	END IF
	IF ( xx .gt. 180. ) xx = xx - 360.
	IF ( ( ERMISS ( xx ) ) .or. ( ERMISS ( yy ) ) ) iret = -1
C*
	RETURN
	END
