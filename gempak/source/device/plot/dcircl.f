	SUBROUTINE DCIRCL  ( iwndw, x, y, xrad, yrad, np, iret )
C************************************************************************
C* DCIRCL								*
C*									*
C* This subroutine will draw a circle with points xrad and yrad on the  *
C* circumference.    							*
C*									*
C* DCIRCL  ( IWNDW, X, Y, XRAD, YRAD, NP, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER	 	Clipping window 		*
C*	X		REAL		X coordinate 			*
C*	Y		REAL		Y coordinate 			*
C*	XRAD            REAL		X circumference radius coord. 	*
C*	YRAD            REAL		Y circumference radius coord. 	*
C*	NP		INTEGER		Number of points		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/86						*
C* A. Hardy/GSC		11/98	Added radius calculation        	*
C* A. Hardy/GSC		12/98	Added VG driver & ncirhw check  	*
C* S. Jacobs/NCEP	 9/99	Added check for edges outside clip area	*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
C*
	REAL		x, y, xrad, yrad, rad 
	INTEGER		iwndw, ix, iy, np
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Checking for VG driver.
C
	IF ( ddev .eq. 'VG' ) THEN
	    CALL HCIRC ( x, y, xrad, yrad, np, iret )
	    RETURN
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP  ( iwndw, ier )
	rad = SQRT ( (x-xrad)**2 + (y-yrad)**2 )
	xlr = ( x - icleft ) * ispanx - rad
	xrr = ( x - icrght ) * ispanx + rad
	ybr = ( y - icbot  ) * ispany - rad
	ytr = ( y - ictop  ) * ispany + rad
C
C*	Checking for clipped circle or software circle.
C
	IF  ( ( xlr .lt. 0.0 ) .or. ( xrr .gt. 0.0 ) .or.
     +	      ( ybr .lt. 0.0 ) .or. ( ytr .gt. 0.0 ) .or.
     +	      ( ncirhw .ne. 2 ) )  THEN
C
C*          Calculating the radius.
C
	    ix = NINT (x)
	    iy = NINT (y)
	    CALL ICIRC   ( ix, iy, rad, np, iret )
	  ELSE
C
C*	    Draw a circle using the hardware commands.
C
	    CALL HCIRC ( x, y, xrad, yrad, np, iret )
	END IF
C*
	RETURN
	END
