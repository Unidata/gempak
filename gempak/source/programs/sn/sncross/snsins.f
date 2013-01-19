	SUBROUTINE SNSINS ( x1, x2, yl1, yl2, ys1, ys2, xint, 
     +				yint, iret )
C************************************************************************
C* SNSINS								*
C*									*
C* This subroutine computes the intersection point for an isentrope	*
C* with the surface.							*
C*									*
C* SNSINS ( X1, X2, YL1, YL2, YS1, YS2, XINT, YINT, IRET )		*
C*									*
C* Input parameters:							*
C*	X1		REAL		First x coordinate		*
C*	X2		REAL		Second x coordinate		*
C*	YL1		REAL		Value of line at x1		*
C*	YL2		REAL		Value of line at x2		*
C*	YS1		REAL		Value of surface at x1		*
C*	YS2		REAL		Value of surface at x2		*
C*									*
C* Output parameters:							*
C*	XINT		REAL		X coordinate at intersection	*
C*	YINT		REAL		Y coordinate at intersection	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C************************************************************************
C------------------------------------------------------------------------
	iret = 0
C
C*	Compute logarithms of the y points.  The lines are assumed to 
C*	intersect in LOG-P, X space.
C
	yyl1 = ALOG ( yl1 )
	yyl2 = ALOG ( yl2 )
	yys1 = ALOG ( ys1 )
	yys2 = ALOG ( ys2 )
C
C*	Find slope and y-intercept of line.
C
	al = ( yyl1 - yyl2 ) / ( x1 - x2 )
	bl =   yyl1 - al * x1
C
C*	Find slope and y-intercept of surface.
C
	as = ( yys1 - yys2 ) / ( x1 - x2 )
	bs =   yys1 - as * x1
C
C*	Find the x and y coordinate of the intersection point.
C
	xint = ( bs - bl ) / ( al - as )
	yint =   al * xint + bl
	yint =   EXP ( yint )
C*
	RETURN
	END
