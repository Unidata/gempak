	SUBROUTINE GG_TCSM ( nsmoth, npts, iptsm, xpt, ypt, iret )
C************************************************************************
C* GG_TCSM								*
C*									*
C* This subroutine applies a 3 or 5-point smoothing to a closed curve.  *
C* The smoothing is performed NSMOTH times.				*
C*									*
C* GG_TCSM  ( NSMOTH, NPTS, IPTSM, XPT, YPT, IRET )			*
C*									*
C* Input parameters:							*
C*	NSMOTH		INTEGER		Number of smoothing passes	*
C*	NPTS		INTEGER		Total number of points		*
C*	IPTSM		INTEGER		3 or 5 point smoothing          *
C*									*
C* Input and output parameters:						*
C*	XPT (NPTS)	REAL		Latitude points                 *
C*	YPT (NPTS)	REAL		Longitude points                *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = too many points           *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/01	From CSMTHN                             *
C* D. Kidwell/NCEP	10/01	Fixed dateline bug, prolog; check npts  *
C* D. Kidwell/NCEP	 5/03	Added arg. iptsm for 3 or 5 pt smoothing*
C************************************************************************
	REAL		xpt (*), ypt (*)
C*
	REAL		ywork (2000)
C------------------------------------------------------------------------
	iret = 0
	IF ( npts .gt. 2000 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Check that there are enough points.
C
	IF ( npts .lt. iptsm )  RETURN
C
	DO ii = 1, npts
	    ywork ( ii ) = ypt ( ii )
	    IF ( ywork ( ii ) .gt. 90. ) ywork ( ii ) = ypt (ii) - 360.
	END DO
C
C*	Loop through the points NSMOTH times.
C
	DO ii = 1, nsmoth
	    xim2 = xpt (npts-2)
	    yim2 = ywork (npts-2)
	    xim1 = xpt (npts-1)
	    yim1 = ywork (npts-1)
	    DO i = 1, npts - 1 
	        xi = xpt (i)
	        yi = ywork (i)
		xip1 = xpt (i+1)
		yip1 = ywork (i+1)
		IF ( (i+1) .lt. npts ) THEN
		    xip2 = xpt (i+2)
		    yip2 = ywork (i+2)
		  ELSE
		    xip2 = xpt (2)
		    yip2 = ywork (2)
		END IF
C
		IF ( iptsm .eq. 3 ) THEN
		    xpt (i)   = (xim1 + 2*xi + xip1) / 4.
		    ywork (i) = (yim1 + 2*yi + yip1) / 4.
		  ELSE IF ( iptsm .eq. 5 ) THEN
	            xpt (i)   = (xim2 + 2*xim1 + 4*xi + 2*xip1 + xip2)
     +				 / 10.
	            ywork (i) = (yim2 + 2*yim1 + 4*yi + 2*yip1 + yip2) 
     +				 / 10.
		END IF
		IF ( i .eq. 1 ) THEN
		    xpt (npts)   = xpt (1)
		    ywork (npts) = ywork (1)
		END IF
C
		xim2 = xim1
		yim2 = yim1
	        xim1 = xi
	        yim1 = yi
	    END DO
	END DO
C
	DO ii = 1, npts
	    ypt ( ii ) = ywork ( ii )
	    IF ( ywork (ii) .lt. (-180.) ) ypt (ii) = ywork (ii) + 360.
	END DO
C*
	RETURN
	END
