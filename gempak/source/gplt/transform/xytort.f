	SUBROUTINE XYTORT  ( x, y, r, theta, iret )
C************************************************************************
C* XYTORT								*
C* 									*
C* This subroutine converts x, y in linear coordinates to r, theta 	*
C* in polar coordinates.						*
C* 									*
C* XYTORT  ( X, Y, R, THETA, IRET )					*
C*									*
C* Input parameters:							*
C*	X		REAL		Value of x			*
C*	Y		REAL		Value of y			*
C* 									*
C* Output parameters: 							*
C*	R		REAL		Value of r			*
C*	THETA		REAL		Value of theta in degrees	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/86						*
C* K. Brill/EMC		 3/96	Input and output variables can be same	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C--------------------------------------------------------------------------
	iret = NORMAL
C
C*	Do transformations.
C
	rr  =  SQRT ( x**2 + y**2 )
	IF  ( (x .eq. 0.) .and. (y .eq. 0.) ) THEN
	    th = 0.
	  ELSE
	    th = ( ATAN2 ( y, x ) ) * RTD
	    IF ( th .lt. 0. ) th = th + TWOPI
	END IF
	r = rr
	theta = th
C*
	RETURN
	END
