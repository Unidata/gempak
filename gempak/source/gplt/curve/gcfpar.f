	SUBROUTINE GCFPAR  ( xx, yy, c, iret )
C************************************************************************
C* GCFPAR								*
C*									*
C* This subroutine fits a second degree polynomial to three points.	*
C* It is called by the cubic spline program.  Therefore, coefficients	*
C* for the two segments are returned as third degree polynomial 	*
C* coefficients with the cubic term set to 0.				*
C*									*
C* GCFPAR ( XX, YY, C, IRET )						*
C*									*
C* Input parameters:							*
C*	XX (3)		REAL		Input x coordinates		*
C*	YY (3)		REAL		Input y coordinates		*
C*									*
C* Output parameters:							*
C*	C (4,2)		REAL		Coefficients			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* T. Lee/GSC		 9/97	Included ERROR.PRM			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	REAL		xx (*), yy (*), c (4,*)
C------------------------------------------------------------------------
	iret = NOPNTS
C
C*	Do the computations for the first interval.
C
	y1 = yy (1)
	y2 = yy (2)
	y3 = yy (3)
	x1 = 0.
	x2 = xx (2) - xx (1)
	x3 = xx (3) - xx (1)
	y2y1 = y2 - y1
	y3y1 = y3 - y1
	y2y1x3 = y2y1 * x3
	y3y1x2 = y3y1 * x2
	fac = x3**2 * x2 - x2**2 * x3
	IF  ( fac .eq. 0. ) RETURN
	ca = ( y3y1x2 - y2y1x3 ) / fac
	cb = ( y2y1 - ca * x2**2 ) / x2
	c (1,1) = 0
	c (2,1) = ca
	c (3,1) = cb
	c (4,1) = y1
C
C*	Do the computations in the second interval.
C
	x1 = xx (2) - xx (1)
	x2 = 0
	x3 = xx (3) - xx (1)
	y1y2 = y1 - y2
	y3y2 = y3 - y2
	y1y2x3 = y1y2 * x3
	y3y2x1 = y3y2 * x1
	fac = x3**2 * x1 - x1**2 * x3
	IF  ( fac .eq. 0. ) RETURN
	ca = ( y3y2x1 - y1y2x3 ) / fac
	cb = ( y1y2 - ca * x1**2 ) / x1
	c (1,2) = 0
	c (2,2) = ca
	c (3,2) = cb
	c (4,2) = y2
C*
	RETURN
	END
