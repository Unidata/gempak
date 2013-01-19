	SUBROUTINE GP_DRAZ  ( dir, xm, ym, azim, iret )
C************************************************************************
C* GP_DRAZ								*
C*									*
C* This subroutine transforms a wind direction in N coordinates to the	*
C* correct direction to plot in M coordinates.				*
C*									*
C* GP_DRAZ ( DIR, XM, YM, AZIM, IRET )					*
C*									*
C* Input parameters:							*
C*	DIR		REAL		Input wind direction		*
C*	XM		REAL		Latitude			*
C*	YM		REAL		Longitude			*
C*									*
C* Output parameters:							*
C*	AZIM		REAL		Transformed direction		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 7/97	From GAZDRM				*
C* S. Jacobs/NCEP	10/98	Copied from GDRAZM			*
C* S. Jacobs/NCEP	10/98	Added call to GQMODE; removed common	*
C* I. Durham/GSC	10/98   Corrected header			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER 	( DISPL = 0.1 )
	REAL		x (2), y (2)
C------------------------------------------------------------------------
C*	Check for map mode.
C
	CALL GQMODE ( igmode, ier )
	IF  ( igmode .eq. 1 )  THEN
C
C*	    Get displacement to use.
C
	    dsp = DISPL
C
C*	    Get local north . . . set point 2 to DSP deg. of lat. north
C*	    of point 1 (note X is lat), then transform to N coordinates.
C
	    x (1) = xm 
	    y (1) = ym
	    x (2) = xm + dsp
	    y (2) = ym 
C
	    CALL GTRANS ( 'M', 'N', 2, x, y, x, y, ier )
C
C*	    Compute displacement in N coordinates, then get direction 
C*	    angle.  ROT > 0 corresponds to north rotated clockwise in
C*	    N coord.  The IF test avoids sending ( 0, 0 ) to ATAN2.
C*	    This occurs when GTRANS does not transform points at the
C*	    edges of a NPG image.  The direction returned (0) is really
C*	    an undefined value.  Since this is at the edge of the image,
C*	    in most cases nothing should be plotted.
C
	    delx = x (2) - x (1)
	    dely = y (2) - y (1)
C
	    IF  ( ( delx .eq. 0. ) .and. ( dely .eq. 0. ) )  THEN
	        dir = 0.
	      ELSE
	        rot  = ATAN2 ( delx, dely ) 
	        azim = dir - rot * RTD
	    END IF
C*
	END IF
	iret = 0
C*
	RETURN
	END
