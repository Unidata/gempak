	SUBROUTINE GP_AZDR  ( azim, xm, ym, dir, iret )
C************************************************************************
C* GP_AZDR								*
C*									*
C* This subroutine transforms a wind direction in M coordinates to the	*
C* correct direction to plot in N coordinates.				*
C*									*
C* GP_AZDR ( AZIM, XM, YM, DIR, IRET )					*
C*									*
C* Input parameters:							*
C*	AZIM		REAL		Input wind direction		*
C*	XM		REAL		Latitude			*
C*	YM		REAL		Longitude			*
C*									*
C* Output parameters:							*
C*	DIR		REAL		Transformed direction		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 8/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 4/86	Changes displ for NPG satellite trans.	*
C* M. desJardins/GSFC	 9/86	Intercepted 0.,0. inputs to ATAN2 func.	*
C* G. Huffman/GSC	 4/89	Recode to do DIR in N coord.		*
C* M. Linda/GSC		12/95	Removed NPGS sat nav			*
C* S. Jacobs/NCEP	10/98	Copied from GAZDRM			*
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
	        rot = ATAN2 ( delx, dely ) 
	        dir = azim + rot * RTD
	    END IF
C*
	END IF
	iret = 0
C*
	RETURN
	END
