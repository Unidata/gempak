	SUBROUTINE GSGGRF  ( ixtyp, iytyp, kx, ky, xll, yll, xur, yur,
     +			     iret )
C************************************************************************
C* GSGGRF								*
C* 									*
C* This subroutine defines the coordinate system for a grid which is	*
C* evenly spaced in a graph coordinate system.  If the grid is defined	*
C* in a polar coordinates system, the grid rows correspond to constant	*
C* THETA values; the grid columns correspond to constant values of R.	*
C* XLL, YLL, XUR and YUR correspond to the min R, min THETA, max R and	*
C* max THETA.  XLL must be greater than or equal to 0.			*
C*									*
C* GSGGRF  ( IXTYP, IYTYP, KX, KY, XLL, YLL, XUR, YUR, IRET )		*
C* 							        	*
C* Input parameters:					        	*
C*	IXTYP		INTEGER		X coordinate type 		*
C*					  1 = linear			*
C*					  2 = logarithmic		*
C*					  3 = ** KAPPA (2/7)		*
C*					  5 = polar (R)			*
C*	IYTYP		INTEGER		Y coordinate type		*
C*					  1 = linear			*
C*					  2 = logarithmic		*
C*					  3 = ** KAPPA (2/7)		*
C*					  5 = polar (THETA)		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	XLL 		REAL		Lower left x value		*
C*	YLL		REAL		Lower left y value		*
C*	XUR		REAL		Upper right x value		*
C*	YUR		REAL		Upper right y value		*
C*									*
C* Output parameters:							*
C* 	IRET 		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins	10/85	GEMPLT Version 3.1			*
C* M. desJardins	 6/86	Eliminated skew-t coordinates		*
C* M. desJardins	10/86	Modified to add polar coordinates	*
C* M. desJardins	 1/87	Corrected error in polar coordinates	*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
C*	Check for graph mode.
C
	ggset = .false.
	IF  ( igmode .ne. 2 )  THEN
	    iret = NIMODE
C
C*	  Check for invalid types.
C
	  ELSE IF  ( ( ( ixtyp .lt. 1 ) .or. ( ixtyp .gt. 3 ) .or.
     +		       ( iytyp .lt. 1 ) .or. ( iytyp .gt. 3 ) ) .and.
     +		     ( ( ixtyp .ne. 5 ) .or. ( iytyp .ne. 5 ) ) )  THEN
	    iret = NINVAL
C
C*	  Check for invalid values for logarithmic types.
C
	  ELSE IF  ( ( ( ixtyp .eq. 2 ) .and. 
     +		     ( (   xll .le. 0. ) .or. ( xur .le. 0. ) ) ) .or.
     +		     ( ( iytyp .eq. 2 ) .and.
     +		     ( (   yll .le. 0. ) .or. ( yur .le. 0. ) ) ) ) THEN
	    iret = NINVAL
C
C*	  Check for invalid range or scale factor.
C
	  ELSE IF  ( ( xll .eq. xur ) .or. ( yll .eq. yur ) )  THEN
	    iret = NINVAL
C
C*	  Check for invalid polar coordinate types.
C
	  ELSE IF ((( ixtyp .eq. 5 ) .and. ( iytyp .eq. 5 )).and.
     +		   (( xll .ge. xur ) .or. ( yll .ge. yur ) .or.
     +		    ( xll .lt. 0. ))) THEN
C
	    iret = NINVAL
C
C*	  Save parameters.
C
	  ELSE
	    iret    = NORMAL
	    jgxtyp  = ixtyp
	    jgytyp  = iytyp
	    gpxl    = 1.
	    gpyb    = 1.
	    gpxr    = FLOAT ( kx )
	    gpyt    = FLOAT ( ky )
	    gxlmg   = xll
	    gybmg   = yll
	    gxrmg   = xur
	    gytmg   = yur
	    ggset   = .true.
C
C*	    Update values in grid common area.
C
	    CALL UPDGDG  ( ier )
	END IF
C*
	RETURN
	END

