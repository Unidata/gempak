	SUBROUTINE GSGRAF ( ixtyp, iytyp, yszxsz, xl, yb, xr, yt, iret )
C************************************************************************
C* GSGRAF								*
C* 									*
C* This subroutine defines a coordinate system for plotting graphs.	*
C* The X and Y axis coordinate types are specified independently.	*
C* The user may control the height to width ratio of the plot by	*
C* setting the parameter YSZXSZ to a positive value.  If YSZXSZ is	*
C* not positive, the plot will fill the available space.		*
C* 									*
C* For polar plots, X and Y are the distance and angle (R,THETA)	*
C* respectively.  YSZXSZ is ignored so that R will be equidistant	*
C* in all directions.  A centered plot with radius R may be defined	*
C* by setting XL = R and YB = XR = YT = 0.				*
C* 									*
C* GSGRAF  ( IXTYP, IYTYP, YSZXSZ, XL, YB, XR, YT, IRET )		*
C* 									*
C* Input parameters:							*
C* 	IXTYP		INTEGER		X coordinate type 		*
C*					   1 = linear			*
C*					   2 = logarithmic		*
C*					   3 = ** kappa (2/7)		*
C*					   4 = skew			*
C*					   5 = polar (R)		*
C* 	IYTYP		INTEGER		Y coordinate type		*
C*					   1 = linear			*
C*					   2 = logarithmic		*
C*					   3 = ** kappa (2/7)		*
C*					   5 = polar (THETA)		*
C* 	YSZXSZ		REAL		Height to width ratio of plot	*
C* 	XL		REAL		Left limit of X axis 		*
C* 	YB		REAL		Bottom limit of Y axis 		*
C* 	XR		REAL		Right limit of X axis		*
C* 	YT		REAL		Top limit of Y axis 		*
C* 									*
C* Output parameters:							*
C* 	IRET 		INTEGER		Return code			*
C** 									*
C* Log:									*
C* M. Vilardo/RDS	 9/84	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/86	Added polar plot capability		*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 5/89	Add new error for LOG plots		*
C* S. Jacobs/NMC	 1/95	Added multi-window common block		*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
	INCLUDE 	'DEVWIN.CMN'
	INCLUDE		'GEMPRM.PRM'
C------------------------------------------------------------------------
C*	Check for graph mode.
C
	IF  ( igmode .ne. 2 )  THEN
	    iret = NIMODE
C
C*	  Check for polar plot.
C
	  ELSE IF  ( ( ixtyp .eq. 5 ) .and. ( iytyp .eq. 5 ) )  THEN
	    IF  ( (yb .eq. 0.) .and. (xr .eq. 0.) .and. (yt .eq. 0.)
     +		  .and. (xl .gt. 0.) )  THEN
C
C*		Set corners of square defined by xl = R.
C
		xlmg = xl * SQRT (2.)
		ybmg = 225.
		xrmg = xlmg
		ytmg = 45.
	      ELSE
C
C*		Check that lower left corner is to left and bottom of
C*		upper right.
C
		CALL RTTOXY  ( xl, yb, x1, y1, ier )
		CALL RTTOXY  ( xr, yt, x2, y2, ier )
		IF  ( ( x1 .ge. x2 ) .or. ( y1 .ge. y2 ) )  THEN
		    iret = NINVAL
		    RETURN
		  ELSE
		    xlmg = xl
		    ybmg = yb
		    xrmg = xr
		    ytmg = yt
		END IF
	    END IF
	    iret  = NORMAL
	    jxtyp = 5
	    jytyp = 5
	    gset  = .true.
	    CALL UPDGRF  ( ier )
	    CALL UPDPXY
C
C*	  Check for invalid types.
C
	  ELSE IF  ( ( ixtyp .lt. 1 ) .or. ( ixtyp .gt. 4 ) .or.
     +		     ( iytyp .lt. 1 ) .or. ( iytyp .gt. 3 ) )  THEN
	    iret = NINVAL
C
C*	  Check for invalid values for logarithmic types.
C
	  ELSE IF  ( ( ( ixtyp .eq. 2 ) .and. 
     +		     ( ( xl .le. 0. ) .or. ( xr .le. 0. ) ) ) .or.
     +		     ( ( iytyp .eq. 2 ) .and.
     +		     ( ( yb .le. 0. ) .or. ( yt .le. 0. ) ) ) )  THEN
	    iret = NILOGP
C
C*	  Check for invalid range or scale factor.
C
	  ELSE IF  ( ( xl .eq. xr ) .or. ( yb .eq. yt ) .or.
     +		     ( yszxsz .lt. 0. ) )  THEN
	    iret = NINVAL
C
C*	  Save parameters.
C
	  ELSE
	    iret   = NORMAL
	    jxtyp  = ixtyp
	    jytyp  = iytyp
	    yxgraf = yszxsz
	    xlmg   = xl
	    ybmg   = yb
	    xrmg   = xr
	    ytmg   = yt
	    gset   = .true.
	    CALL UPDGRF  ( ier )
	    CALL UPDPXY
	END IF
C
	IF  ( iret .eq. NORMAL )  THEN
C
c*	    Save common variables for window.
C
	    nmode  (ncurwn) = igmode
	    nxtyp  (ncurwn) = ixtyp
	    nytyp  (ncurwn) = iytyp
	    uyxrat (ncurwn) = iytyp
	    uxl (ncurwn) = xl
	    uyb (ncurwn) = yb
	    uxr (ncurwn) = xr
	    uyt (ncurwn) = yt
	END IF
C*
	RETURN
	END
