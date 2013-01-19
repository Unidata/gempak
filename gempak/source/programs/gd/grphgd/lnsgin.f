        SUBROUTINE LNSGIN ( xlin, ylin, xlins, ylins, npts, mxint,
     +                      numint, xint, yint, iret )
C************************************************************************
C* LNSGIN                                                               *
C*                                                                      *
C* This subroutine calculates and returns all the intersection points	*
C* of a straight line, (XLIN(1),YLIN(1))-(XLIN(2),YLIN(2)), with a	*
C* 'curved' line, (XLINS(1),YLINS(1)) - (XLINS(2),YLINS(2)) - ... -	*
C* (XLINS(NPTS),YLINS(NPTS)) consisting of NPTS points.			*
C*                                                                      *
C* LNSGIN ( XLIN, YLIN, XLINS, YLINS, NPTS, MAXINT, 			*
C*	    NUMINT, XINT, YINT, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      XLIN (2)        REAL            Straight line X input coords	*
C*      YLIN (2)        REAL            Straight line Y input coords	*
C*      XLINS (NPTS)    REAL            Curved line X input coords	*
C*      YLINS (NPTS)    REAL            Curved line Y input coords	*
C*	NPTS		INTEGER		Number of points along X/YLINS	*
C*	MXINT		INTEGER		Max number of intersections	*
C*                                                                      *
C* Output parameters:                                                   *
C*	NUMINT		INTEGER		Number of intersections		*
C*      XINT (NUMINT)   REAL            X intersection coordinates      *
C*      YINT (NUMINT)   REAL            Y intersection coordinates      *
C*      IRET            INTEGER         Return code                     *
C*					= 3 - mxint exceeded		*
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP	 9/98						*
C* D.W.Plummer/NCEP	 9/01	Implement check for maxint		*
C* D.W.Plummer/NCEP	 7/03	Chg variable maxint to mxint		*
C* D.W.Plummer/NCEP	 8/05	Rm rounding of ints to nearest 100th 	*
C************************************************************************
C
	REAL		xlin(*), ylin(*), xlins(*), ylins(*)
	REAL		xint(*), yint(*)
C
	REAL*8		m, ms, b, bn, bn1, bs
C
	iret = 0
	numint = 0
C
C*	Check for vertical line and get intersects for crossing segments.
C
	IF ( xlin(1) .eq. xlin(2) )  THEN
C
	    DO  n = 1, npts-1
C
		IF ( xlins(n  ) .eq. xlin(1) .and. 
     +		     xlins(n+1) .eq. xlin(1) )  THEN
C
		    IF ( numint+1 .le. mxint )  THEN
C
		      numint = numint + 1
		      xint(numint) = xlin(1)
		      yint(numint) = ( ylins(n) + ylins(n+1) ) / 2.0
C
		    ELSE
C
		      iret = 3
		      RETURN
C
		    END IF
C
		ELSE IF ( ( xlins(n  ) .le. xlin(1) .and.
     +			    xlins(n+1) .ge. xlin(1) )  .or.
     +			  ( xlins(n  ) .ge. xlin(1) .and.
     +                      xlins(n+1) .le. xlin(1) ) )  THEN
C
		    IF ( numint+1 .le. mxint )  THEN
C
		      numint = numint + 1
		      xint(numint) = NINT( xlin(1) * 100.0 ) / 100.0
		      m = (ylins(n+1)-ylins(n)) / (xlins(n+1)-xlins(n))
		      b = ylins(n) - m * xlins(n)
		      yint(numint) = m * xint(numint) + b
		      yint(numint) = NINT( yint(numint) * 100.0 ) / 100.0
C
		    ELSE
C
		      iret = 3
		      RETURN
C
		    END IF
C
		END IF
C
	    END DO
C
	ELSE
C
	    m = ( ylin(2)-ylin(1) ) / ( xlin(2)-xlin(1) )
	    b = ylin(1) - m * xlin(1)
C
	    DO  n = 1, npts-1
C
		bn  = ylins(n  ) - m * xlins(n  )
		bn1 = ylins(n+1) - m * xlins(n+1)
C
		IF ( bn .eq. b .and. bn1 .eq. b )  THEN
C
		    IF ( numint+1 .le. mxint )  THEN
C
		      numint = numint + 1
		      xint(numint) = ( xlins(n) + xlins(n+1) ) / 2.0
		      yint(numint) = ( ylins(n) + ylins(n+1) ) / 2.0
C
		    ELSE
C
		      iret = 3
		      RETURN
C
		    END IF
C
		ELSE IF ( ( bn .ge. b .and. bn1 .le. b ) .or.
     +			  ( bn .le. b .and. bn1 .ge. b ) )  THEN
C
		    IF ( numint+1 .le. mxint )  THEN
C
		      numint = numint + 1
C
		      IF ( xlins(n+1) .ne. xlins(n) )  THEN
C
			ms = (ylins(n+1)-ylins(n))/(xlins(n+1)-xlins(n))
			bs = ylins(n) - ms * xlins(n)
			xint(numint) = ( bs - b ) / ( m - ms )
C
		      ELSE
C
			xint(numint) = xlins(n)
C
		      END IF
C
		      yint(numint) = m * xint(numint) + b
C
		    ELSE
C
		      iret = 3
		      RETURN
C
		    END IF
C
		END IF
C
	    END DO
C
 	END IF
C
	RETURN
	END
