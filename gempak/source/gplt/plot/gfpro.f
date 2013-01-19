	SUBROUTINE GFPRO ( xdis, ydis, x, y, a, b, iret )
C************************************************************************
C* GFPRO								*
C*									*
C* Given two points (x1, y1) and (x2, y2) in L coordinates that define	*
C* a line that would circle the world the wrong way, this subroutine	*
C* splits the line in two.  It calculates two new points, (a1, b1) and	*
C* (a2, b2), to be paired with the first two points.  Points (x1, y1)	*
C* and (a1, b1) define one new line segment while points (x2, y2) and	*
C* (a2, b2) define the other.  The two new points are outside of the L	*
C* bounds and it is assumed that the resulting line segments will be	*
C* subsequently clipped.						*
C*									*
C* GFPRO ( XDIS, YDIS, X, Y, A, B, IRET )				*
C*									*
C* Input parameters:							*
C*	XDIS		REAL		Distance between X1 and X2	*
C*	YDIS		REAL		Distance between Y1 and Y2	*
C*	X (2)		REAL		X coordinates in L coordinates	*
C*	Y (2)		REAL		Y coordinates in L coordinates	*
C*									*
C* Output parameters:							*
C*	A (2)		REAL		X coordinates in L coordinates	*
C*	B (2)		REAL		Y coordinates in L coordinates	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 2/97	Based on GLINE				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		x(2), y(2), a(2), b(2)
C------------------------------------------------------------------------
	iret = NORMAL
C
	a ( 1 ) = x ( 2 )
	b ( 1 ) = y ( 2 )
	a ( 2 ) = x ( 1 )
	b ( 2 ) = y ( 1 )
C
C*	If horizontal bounds exceeded, then project the X coordinate.
C
	IF  ( xdis .gt. ueps ) THEN
C
	    xdel = ABS ( ABS ( xbndrl - xbndll ) - xdis )
C
	    xl1 = ABS ( x ( 1 ) - xbndll )
	    xr1 = ABS ( x ( 1 ) - xbndrl )
	    xl2 = ABS ( x ( 2 ) - xbndll )
	    xr2 = ABS ( x ( 2 ) - xbndrl )
C
	    IF  ( xl1 .lt. xr1 ) THEN
		a ( 1 ) = x ( 1 ) - xdel
	      ELSE
		a ( 1 ) = x ( 1 ) + xdel
	    END IF
C
	    IF  ( xl2 .lt. xr2 ) THEN
		a ( 2 ) = x ( 2 ) - xdel
	      ELSE
		a ( 2 ) = x ( 2 ) + xdel
	    END IF
	END IF
C
C*	If vertical bounds exceeded, then project the Y coordinate.
C
	IF  ( ydis .gt. veps ) THEN
C
	    ydel = ABS ( ABS ( ybndbl - ybndtl ) - ydis )
C
	    yt1 = ABS ( y ( 1 ) - ybndtl )
	    yb1 = ABS ( y ( 1 ) - ybndbl )
	    yt2 = ABS ( y ( 2 ) - ybndtl )
	    yb2 = ABS ( y ( 2 ) - ybndbl )
C
	    IF  ( yt1 .lt. yb1 ) THEN
		b ( 1 ) = y ( 1 ) + ydel
	      ELSE
		b ( 1 ) = y ( 1 ) - ydel
	    END IF
C
	    IF  ( yt2 .lt. yb2 ) THEN
		b ( 2 ) = y ( 2 ) + ydel
	      ELSE
		b ( 2 ) = y ( 2 ) - ydel
	    END IF
	END IF
C*
	RETURN
	END
