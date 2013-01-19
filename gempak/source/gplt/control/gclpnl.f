	SUBROUTINE GCLPNL  ( xllf, yllf, xurf, yurf, iret )
C************************************************************************
C* GCLPNL								*
C* 									*
C* This subroutine will clear a particular sub-region of the screen.	*
C* The sub-region is specified using fractions of the available area on	*
C* the plot device.  The point (0., 0.) is the lower left corner of the	*
C* device; (1., 1.) is the upper right corner of the device.		*
C* For example:								*
C*									*
C*     call gclpnl  ( .5, .5, 1., 1., iret ) 				*
C*									*
C* will clear the plot in the upper right quadrant of the device.  	*
C*									*
C* Note that the fractions describing the view region are not equal 	*
C* to coordinate values, except for a square device.			*
C*								 	*
C* GCLPNL  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Input parameters:							*
C*	XLLF		REAL		Lower left x fraction		*
C*	YLLF		REAL		Lower left y fraction		*
C*	XURF		REAL		Upper right x fraction		*
C*	YURF		REAL		Upper right y fraction		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Tyle/GSC		 2/97	Based on GSVIEW				*
C* K. Tyle/GSC		 2/97	Multiply by x and y bounds		*
C************************************************************************
	REAL		x(2), y(2)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Check that points are ordered properly.
C
	IF ( xllf .gt. xurf ) THEN
	    x(1) = xurf
	    x(2) = xllf
	  ELSE
	    x(1) = xllf
	    x(2) = xurf
	END IF
	IF ( yllf .gt. yurf ) THEN
	    y(1) = yurf
	    y(2) = yllf
	  ELSE
	    y(1) = yllf
	    y(2) = yurf
	END IF
C
C*	Scale the values of the points so they conform
C*	to the bounds of the plot region.
C
	CALL GQBND  ( 'N', xlq, ybq, xrq, ytq, ier )
	x(1) = x(1) * xrq
	x(2) = x(2) * xrq
	y(1) = y(1) * ytq
	y(2) = y(2) * ytq
C
C*	Translate to device coordinates.
C
	CALL GTRANS ( 'N', 'D', 2, x, y, x, y, iret )
	xlld = x(1)
	xurd = x(2)
	ylld = y(1)
	yurd = y(2)
C
C*	Clear the panel.
C
	CALL DCLPNL ( xlld, ylld, xurd, yurd, iret )
C*
	RETURN
	END
