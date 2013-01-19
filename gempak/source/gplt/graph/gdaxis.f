	SUBROUTINE GDAXIS  ( iaxis, axpos, laxis, lbfrq, mtfrq, 
     +			     lgfrq, ndec, np, axary, iret )     
C************************************************************************
C* GDAXIS								*
C* 									*
C* This subroutine draws a graph axis with labels, tick marks, and	*
C* grid lines.  The graph coordinates must be defined by a call to	*
C* GSGRAF before this subroutine is called.				*
C*     									*
C* Line attributes apply to grid lines.  The axis line will be a	*
C* solid line.  Tick mark attributes can be set in GSTICK.		*
C*      	   							*
C* The label position array, AXARY, is allowed a maximum of 530		*
C* elements.								*
C*									*
C* If NDEC, the number of decimal places, is negative, the program	*
C* will use an appropriate number.					*
C* 									*
C* LBFRQ, MTFRQ and LGFRQ are specified in the form FF where		*
C* FF is the frequency with which to plot items in the AXARY		*
C* positions.  For example, LBFRQ = 03 plots every third element	*
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
C* 									*
C* GDAXIS  ( IAXIS, AXPOS, LAXIS, LBFRQ, MTFRQ, LGFRQ, NDEC, NP,	*
C*           AXARY, IRET )						*
C* 									*
C* Input parameters:							*
C* 	IAXIS		INTEGER 	Axis				*
C*				 	  1 = x axis labelled below	*
C*					  2 = y axis labelled left	*
C*					  3 = x axis labelled above	*
C*					  4 = y axis labelled right	*
C* 	AXPOS		REAL		Intersection with other axis	*
C* 	LAXIS		LOGICAL		Axis draw flag			*
C* 	LBFRQ		INTEGER		Frequency of labels	        *
C*	MTFRQ		INTEGER		Frequency of tick marks	        *
C* 	LGFRQ		INTEGER		Frequency of grid lines         *
C* 	NDEC		INTEGER		# of decimal places in labels	*
C* 	NP		INTEGER		Number of values in AXARY	*
C* 	AXARY (NP)	REAL		Locations on other axis		*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/88	Rewrote to call GAAXIS			*
C* K. Brill/GSC          5/90   Took out start for labels, ticks, and	*
C*                              grid lines -- documentation change	*
C* M. desJardins/GSFC	 7/90	Add check for large numbers in labels	*
C* S. Jacobs/EAI	10/93	Changed CLABEL --> GR_LABL		*
C*				Changed CNDECP --> GR_NDCP		*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
C*
	REAL 		axary(*)
	LOGICAL		laxis
C*
	CHARACTER	chary (530)*24
C------------------------------------------------------------------------
C*	Check that there are not more than 530 points.
C
	npt = np
	IF  ( npt .gt. 530 )  THEN
	    iret = NLBLEX
	    RETURN
	END IF
C
C*	Get the number of decimal points to use.
C
	IF  ( ndec .gt. 5 )  THEN
	    ndc = 5
	  ELSE IF  ( ndec .ge. 0 )  THEN
	    ndc = ndec
	  ELSE
	    CALL GR_NDCP  ( npt, axary, ndc, ier )
	END IF
	IF  ( ndc .eq. 0 )  THEN
	    iform = 1
	  ELSE IF  ( ndc .lt. 0 )  THEN
	    iform = 3
	    ndc   = 0
	  ELSE
	    iform = 2
	END IF
C
C*	Encode all the label values.
C
	DO  i = 1, npt
	    CALL GR_LABL ( axary (i), iform, ndc, chary (i), nc, ier )
	END DO
C
C*	Draw the axis.
C
	CALL GAAXIS  ( iaxis, axpos, laxis, lbfrq, mtfrq, lgfrq,
     +		       npt, axary, chary, iret )
C*
	RETURN
	END
