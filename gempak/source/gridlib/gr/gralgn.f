	SUBROUTINE GR_ALGN  ( grdin, deltax, deltay, grdout, kx, ky, 
     +			      iret )
C************************************************************************
C* GR_ALGN								*
C*									*
C* This subroutine aligns a grid on grid points.  The lower left	*
C* corner specified in the input grid corners is moved to the left	*
C* and down if necessary.  The input and output grid corners are	*
C* arrays ordered as follows:  lower left lat, lower left lon, upper 	*
C* right lat, upper right lon.						*
C*									*
C* GR_ALGN  ( GRDIN, DELTAX, DELTAY, GRDOUT, KX, KY, IRET )		*
C*									*
C* Input parameters:							*
C*	GRDIN  (4)	REAL		Input grid corners		*
C*	DELTAX		REAL		X grid spacing			*
C*	DELTAY		REAL		Y grid spacing			*
C*									*
C* Output parameters:							*
C*	GRDOUT (4)	REAL		Aligned grid corners		*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = invalid grid spacing	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* M. desJardins/GSFC	 8/88	Modified from OAGAGN			*
C************************************************************************
	REAL		grdin (*), grdout (*)
C------------------------------------------------------------------------
C*	Check for valid deltax and deltay.
C
	IF  ( ( deltax .le. 0. ) .or. ( deltay .le. 0. ) )  THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Compute the number of grid points in the x direction.
C*	Correct the northeast grid corner to lie on a grid line.
C
	nlon = IFIX  ( ( grdin (4) - grdin (2) ) / deltax )
	kx   = nlon + 1
	grdout (2) = grdin  (2)
	grdout (4) = grdout (2) + nlon * deltax
C
C*	Do the same computations for the latitude.
C
	nlat = IFIX  ( ( grdin (3) - grdin (1) ) / deltay )
	ky   = nlat + 1
	grdout (1) = grdin  (1)
	grdout (3) = grdout (1) + nlat * deltay
C
C*	Check that kx and ky are at least 2.
C
	IF  ( ( kx .lt. 2 ) .or. ( ky .lt. 2 ) )  THEN
	    iret = -5
	END IF
C*
	RETURN
	END
