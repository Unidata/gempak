	SUBROUTINE GAAXIS  ( iaxis, axpos, laxis, lbfrq, mtfrq, 
     +			     lgfrq, np, axary, chary, iret )     
C************************************************************************
C* GAAXIS								*
C* 									*
C* This subroutine draws a graph background with labels, tick marks, 	*
C* and grid lines.  The graph coordinates must be defined by a call to	*
C* GSGRAF before this subroutine is called.				*
C*     									*
C* Line attributes apply to grid lines.  The axis line will be a	*
C* solid line.  Tick mark attributes can be set in GSTICK.		*
C*      	   							*
C* The label string (CHARY) and position (AXARY) arrays are allowed a 	*
C* maximum of 530 elements.  Each CHARY element may contain up to 24	*
C* characters.								*
C* 									*
C* LBFRQ, MTFRQ and LGFRQ are specified in the form FF where		*
C* FF is the frequency with which to plot items in the AXARY		*
C* positions.  For example, LBFRQ = 3 plots every third element		*
C* of CHARY beginning with the first, that is, CHARY (1) at 		*
C* AXARY (1), CHARY (4) at AXARY (4), CHARY (7) at AXARY (7), and	*
C* so on.								*
C* 									*
C* In a polar coordinate system, the axis is not drawn.  When		*
C* IAXIS is 1 or 3, grid lines are circles with the radii specified	*
C* in AXARY.  When IAXIS is 2 or 4, AXARY specifies the angle for	*
C* radial lines which are drawn from the center of the circle to	*
C* AXPOS.  Tick marks are not drawn in polar coordinates.		*
C*									*
C* Except for CHARY, GAAXIS is identical to GDAXIS.			*
C*									*
C* GAAXIS  ( IAXIS, AXPOS, LAXIS, LBFRQ, MTFRQ, LGFRQ, NP,		*
C*           AXARY, CHARY, IRET )					*
C* 									*
C* Input parameters:							*
C* 	IAXIS		INTEGER 	Axis				*
C*				 	  1 = x axis labelled below	*
C*					  2 = y axis labelled left	*
C*					  3 = x axis labelled above	*
C*					  4 = y axis labelled right	*
C* 	AXPOS		REAL		Intersection with other axis	*
C* 	LAXIS		LOGICAL		Axis draw flag			*
C* 	LBFRQ		INTEGER		Frequency of labels		*
C*	MTFRQ		INTEGER		Frequency of tick marks		*
C* 	LGFRQ		INTEGER		Frequency of grid lines		*
C* 	NP		INTEGER		Number of values in AXARY	*
C* 	AXARY (NP)	REAL		Locations on other axis		*
C*	CHARY (NP)	CHAR*24		Label strings			*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	10/82						*
C* M. Vilardo/RDS	 6/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	12/86	Added polar coordinate axes		*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* G. Huffman/GSC	 1/89	Limit num. of label positions to 50	*
C* M. desJardins/GSFC	 5/89	Plot SKEWT line not intersecting x axis	*
C* K. Brill/GSC          5/90   Took out start for ticks, labels, and 	*
C*                              grid lines.				*
C* T. Lee/GSC		 9/97	Added a dummy argument in GDAXLB calls	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	chary (*)
	REAL 		axary(*)
	LOGICAL		laxis
C*
	LOGICAL		done, vis
	REAL		xan (2), yan (2), xam (2), yam (2)
	REAL		xn (3), yn (3), xm (3), ym (3), xng (2), yng (2)
C------------------------------------------------------------------------
C*	Check for graph mode and that device has been set.
C
	done = .true.
	IF  ( igmode .ne. 2 )  THEN
	    iret = NIMODE
	    RETURN
	  ELSE IF  ( ddev .eq. ' ' )  THEN
	    iret = NDVICE
	    RETURN
	END IF
C
C*	Check that there are no more than 530 label positions.  
C
	IF  ( np .gt. 530 )  THEN
	    iret = NLBLEX
	    RETURN
	END IF
C
C*	Get graph coordinate setup.
C
	CALL GQGRAF  ( ixtyp, iytyp, yszxsz, xlm, ybm, xrm, ytm, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get polar coordinates.
C
	IF  ( ( ixtyp .eq. 5 ) .and. ( iytyp .eq. 5 ) )  THEN
	    CALL GDPAXS  ( iaxis, axpos, lbfrq, mtfrq, lgfrq, np,
     +			   axary, chary, iret )
	    RETURN
	END IF
C
C*	Check for invalid input.
C
	IF  ( ( iaxis .lt. 1 ) .or. ( iaxis .gt. 4 ) .or.
     +	      ( lbfrq .lt. 0 ) .or. ( lbfrq .gt. 9999 ) .or.
     +	      ( mtfrq .lt. 0 ) .or. ( mtfrq .gt. 9999 ) .or.
     +	      ( lgfrq .lt. 0 ) .or. ( lgfrq .gt. 9999 ) )  THEN
	    iret = NINVAL
	    RETURN
	END IF
C
C*	Compute frequency for labels, ticks and grids.  (MOD is done
C*      for consistency with earlier version of this subroutine.)
C
	mtfq = MOD ( mtfrq, 100 )
	lgfq = MOD ( lgfrq, 100 )
	lbfq = MOD ( lbfrq, 100 )
C
C*	Query the bounds of the plot display region.
C
	CALL GQBND  ( 'P', xlp, ybp, xrp, ytp, ierr )
C
C*	If requested draw axis line.
C
	IF  ( laxis )  THEN
C
C*	    Determine the correct end points for the specified axis.
C*	    Note the boundaries of the plot display region are used to
C*	    insure the correct end point coordinates when a skew X 
C*	    axis is used.
C
	    IF  ( ( iaxis .eq. 1 ) .or. ( iaxis .eq. 3 ) )  THEN
		xam (1) = xlm
		yam (1) = axpos
		CALL GTRANS  ( 'M', 'N', 1, xam, yam, xan, yan, ier )
		xan (1) = xlp
		yan (1) = yan (1)
		xan (2) = xrp
		yan (2) = yan (1)
	      ELSE
		xam (1) = axpos
		yam (1) = ybm
		CALL GTRANS  ( 'M', 'N', 1, xam, yam, xan, yan, ier )
		xan (2) = xan (1)
		yan (2) = ytp
	    END IF
C
C*	    Save the current line attributes and set solid line.
C
	    CALL GQLINE  ( iltyp, ilthw, iwidth, iwhw, ier )
	    CALL GSLINE  (     1,     0,      0,    0, ier )
C
C*	    Plot the axis line.
C
	    CALL GLINE  ( 'P', 2, xan, yan, ier )
C
C*	    Restore the original line attributes.
C
	    CALL GSLINE  ( iltyp, 0, 0, 0, ier )
	END IF
C
C*      Do the tick marks.
C
	IF ( mtfq .ne. 0 ) 
     +       CALL GTICK  ( iaxis, axpos, mtfq, np, axary, ier )
C
C*	Loop through AXARY plotting labels and grid lines.
C
	DO  ip = 1, np
C
C*	    Transform coordinates of this axary element from graph 
C*	    to normalized device coordinates at the intersection 
C*	    with and the limits of the other axis.
C
	    IF  ( ( iaxis .eq. 1 ) .or. ( iaxis .eq. 3 ) )  THEN
		xm (1) = axary( ip )
		ym (1) = axpos
		xm (2) = axary( ip )
		ym (2) = ybm
		xm (3) = axary( ip )
		ym (3) = ytm
		CALL GTRANS  ( 'M', 'N', 3, xm, ym, xn, yn, ier )
	      ELSE 
		xm (1) = axpos
		ym (1) = axary (ip)
		xm (2) = axpos
		ym (2) = ybm
		CALL GTRANS  ( 'M', 'N', 2, xm, ym, xn, yn, ier )
		xn (1) = xn (2)
		yn (1) = yn (1)
		xn (2) = xlp
		yn (2) = yn (1)
		xn (3) = xrp
		yn (3) = yn (1)
	    END IF
C
C*	    Check to see if original point is visible.
C
	    CALL GPTVIS  ( 'P', 1, xn, yn, vis, ier )
C
C*	    Add labels and grid lines only if point is visible.
C
	    IF  ( vis )  THEN
C
C*		Plot labels at specified frequency. 
C
		IF  ( lbfq .gt. 0 ) THEN
     		    IF  ( MOD ( (ip - 1), lbfq ) .eq. 0  )  THEN
		      x = xn (1)
		      y = yn (1)
		      CALL GDAXLB ( iaxis, ixtyp, 0., x, y, chary (ip),
     +                              ier )
	            END IF
		END IF
	    END IF
C
C*	    Draw SKEWT grid lines whether visible or not.
C
	    IF  ( lgfq .gt. 0 ) THEN
     	      IF ( MOD ( (ip - 1), lgfq ) .eq. 0 )  THEN 
		IF  ( vis .or. ( ixtyp .eq. 4 ) )  THEN
		    xng (1) = xn (2)
		    yng (1) = yn (2)
		    xng (2) = xn (3)
		    yng (2) = yn (3) 
		    CALL GLINE  ( 'P', 2, xng, yng, ier )
		END IF
	      END IF
	    END IF
	END DO
C*
	RETURN
	END
