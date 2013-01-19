	SUBROUTINE GG_ARC ( sys, xcent, ycent, xcrm, ycrm, npts,
     +			    ang1, ang2, iaccf, xout, yout, iret )
C************************************************************************
C* GG_ARC								*
C*									*
C* This routine computes the points around an arc. The center and the	*
C* point on the radius can be any coordinate system. The output points	*
C* will be in the same coordinates. If the points are in Map		*
C* coordinates, then the angles are degrees from North. For all other	*
C* coordinates, the angles are oriented with 0 degrees along the	*
C* X axis. The accuracy factor controls the amount of "course		*
C* correction" that is applied to the lat/lon point calculation. A	*
C* value of 0 means that the true great arc calculation is performed.	*
C* A value larger than 0 is the number of intermediary points to 	*
C* compute along the radial distance.					*
C*									*
C* GG_ARC ( SYS, XCENT, YCENT, XCRM, YCRM, NPTS, ANG1, ANG2, IACCF,	*
C*	    XOUT, YOUT,	IRET )						*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*					  'S' = screen coordinates	*
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	XCENT		REAL		X / Lat of center point		*
C*	YCENT		REAL		Y / Lon of center point		*
C*	XCRM		REAL		X / Lat of radius point		*
C*	YCRM		REAL		Y / Lon of radius point		*
C*	NPTS		INTEGER		Number of points on arc		*
C*	ANG1		REAL		Start angle			*
C*					  North relative for M coord	*
C*	ANG2		REAL		End angle			*
C*					  North relative for M coord	*
C*	IACCF		INTEGER		Accuracy factor for M coord	*
C*									*
C* Output parameters:							*
C*	XOUT(*)		REAL		X / Lat of output points	*
C*	YOUT(*)		REAL		Y / Lon of output points	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/01	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sys
	REAL		xout (*), yout (*)
C*
	CHARACTER	sysuc*1
C------------------------------------------------------------------------
	iret = 0
C*
	np = npts
	IF  ( np .lt. 2 )  np = 2
C
C*	Compute the arc in Map coordinates.
C
	CALL ST_LCUC ( sys, sysuc, ier )
	IF  ( sysuc .eq. 'M' )  THEN
	    dtheta = ang2 - ang1
	    IF  ( ABS (dtheta) .gt. 360. )  dtheta = 360.
C
	    thta = dtheta / FLOAT ( np - 1 )
C
	    nn = 1
	    CALL CLO_DIST ( xcent, ycent, nn, xcrm, ycrm, dist, ier )
C
	    IF  ( iaccf .gt. 100 )  THEN
	    	jaccf = 100
	      ELSE IF  ( iaccf .lt. 0 )  THEN
	    	jaccf = 1
	      ELSE
		jaccf = iaccf + 1
	    END IF
C
	    DO  i = 1, np
	    	dir = ang1 + thta * (i-1)
		xx = xcent
		yy = ycent
		dst = dist / FLOAT (jaccf)
		DO  j = 1, jaccf
		    CALL CLO_DLTLN ( xx, yy, dst, dir,
     +				     xout(i), yout(i), ier )
		    xx = xout(i)
		    yy = yout(i)
		END DO
	    END DO
	  ELSE
C
C*	    Compute the arc in all other coordinates, by tranforming
C*	    to Normal coordinates.
C
	    CALL GTRANS ( sysuc, 'N', 1, xcent, ycent, xc, yc, ier )
	    CALL GTRANS ( sysuc, 'N', 1, xcrm, ycrm, xr, yr, ier )
C
	    theta1 = ang1 * DTR
	    theta2 = ang2 * DTR
	    dtheta = theta2 - theta1
	    IF  ( ABS (dtheta) .gt. TWOPI )  dtheta = TWOPI
C
	    thta = dtheta / FLOAT ( np - 1 )
C
	    dist = SQRT ( (xc-xr)**2 + (yc-yr)**2 )
C
	    DO  i = 1, np
	    	dir = theta1 + thta * (i-1)
	    	xout (i) = xc + dist * COS ( theta1 + dir )
	    	yout (i) = yc + dist * SIN ( theta1 + dir )
	    END DO
C
	    CALL GTRANS ( 'N', sysuc, np, xout, yout, xout, yout, ier )
	END IF
C*
	RETURN
	END
