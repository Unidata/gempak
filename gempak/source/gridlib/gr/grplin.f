	SUBROUTINE GR_PLIN  ( endpts, npmx, npts, rgx, rgy, 
     +                                            rlat, rlon, iret )
C************************************************************************
C* GR_PLIN								*
C*									*
C* This subroutine translates the user input for the end points of a 	*
C* cross-section line through a grid into an array of locations along	*
C* the line segment.  The locations in the output array are evenly 	*
C* spaced, with the spacing being approximately the grid spacing.	*
C*									*
C* GR_PLIN  ( ENDPTS, NPMX, NPTS, RGX, RGY, RLAT, RLON, IRET )		*
C*									*
C* Input parameters:							*
C*	ENDPTS		CHAR*		User input for end points	*
C*	NPMX		INTEGER		Max allowed value for NPTS	*
C*									*
C* Output parameters:							*
C*	NPTS		INTEGER		Number of points along line	*
C*	RGX  (NPMX)	REAL		X grid point			*
C*	RGY  (NPMX)	REAL		Y grid point			*
C*	RLAT (NPMX)	REAL		Latitude			*
C*	RLON (NPMX)	REAL		Longitude			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-12 = invalid grid point	*
C*					-18 = endpoints too close	*
C*					-23 = output pts exceeds npmx	*
C**									*
C* Log:									*
C* K. F. Brill/GSC       5/89   Created from GR_PLOC			*
C* K Brill/GSC           4/90   Corrected iret = -11 to -12		*
C* K. Brill/NMC          8/90   Added -18 return code			*
C* K. Brill/NMC		 3/91	Add one to NPTS				*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* R. Tian/SAIC		10/02	Add NPMX				*
C* K. Brill/HPC		 2/03	Check for DINX close to zero		*
C************************************************************************
	CHARACTER*(*)	endpts
C*
        REAL 		rgx (*), rgy (*), rlat(*), rlon(*)
	CHARACTER	gpoint(2)*64, cdef(2)*1
C------------------------------------------------------------------------
	iret = 0
	cdef(1) = ' '
	cdef(2) = ' '
C
C* 	Split the input string into the expected substrings seperated by
C*	a greater than sign (>).
C
	CALL ST_CLST ( endpts, '>', cdef, 2, gpoint, nums, iret )
 	IF  ( iret .ne. 0 .or. nums .ne. 2 )  THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Find location of the two end points.
C
	DO i = 1, 2
	  CALL GR_PLOC ( gpoint (i), rgx (i), rgy (i), rlat (i),
     +                   rlon (i), ier )
	  IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'GR', ier, gpoint (i), ire )
	    iret = -12
	    RETURN
	  END IF
	END DO
C
C*	The end points in grid coordinates are now in rgx(1-2) and
C*	rgy(1-2).
C
C*      Compute the number of points along this line segment.
C
	dinx = rgx (2) - rgx (1)
	IF ( ABS (dinx) .lt. .0001 ) dinx = 0.0
	diny = rgy (2) - rgy (1)
	dlen = SQRT ( dinx * dinx + diny * diny )
	npts = INT ( dlen + .05 ) + 1
	IF ( npts .le. 1 ) THEN
	  iret = -18
	  npts = 0
	  RETURN
	END IF
	IF ( npts .gt. npmx ) THEN
	  iret = -23
	  npts = 0
	  RETURN
	END IF
C
C*	Create an array of grid points representing this line segment.
C
	rgx ( npts ) = rgx (2)
	rgy ( npts ) = rgy (2)
	rlat ( npts ) = rlat (2)
        rlon ( npts ) = rlon (2)
	IF ( dinx .ne. 0.0 ) slope = diny / dinx
        deltx = dinx / FLOAT ( npts - 1 ) 
	delty = diny / FLOAT ( npts - 1 )	
C*
	xgv = rgx (1)
	nst = npts - 1
	DO i = 2, nst
	  IF ( dinx .ne. 0.0 ) THEN
	    xgv = xgv + deltx
	    rgx (i) = xgv
	    rgy (i) = ( xgv - rgx (1) ) * slope + rgy (1)
	  ELSE
            rgx (i) = xgv
	    rgy (i) = rgy (i-1) + delty
	  END IF
C
C*	  Transform the grid coordinate to map to get lat/lon.
C	  
	  CALL GTRANS ( 'G', 'M', 1, 
     2		rgx (i), rgy (i), rlat (i), rlon (i), ier )
        END DO
C*       
	RETURN
	END
