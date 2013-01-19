	SUBROUTINE GDPAXS  ( iaxis, axpos, lbfrq, mtfrq, lgfrq, np, 
     +			     axary, chary, iret )
C************************************************************************
C* GDPAXS								*
C*									*
C* This subroutine draws axes in polar coordinates.			*
C*									*
C* GDPAXS  ( IAXIS, AXPOS, LBFRQ, MTFRQ, LGFRQ, NP, AXARY, CHARY,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C* 	IAXIS		INTEGER 	Axis				*
C*				 	  1 = x axis labelled below	*
C*					  2 = y axis labelled left	*
C*					  3 = x axis labelled above	*
C*					  4 = y axis labelled right	*
C* 	AXPOS		REAL		Intersection with other axis	*
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
C* I. Graffman/RDS	12/86						*
C* M. desJardins/GSFC	 9/88	Lots of changes				*
C* G. Huffman/GSC	10/88	Test on AXARY for 360 overprinting 0	*
C*				protect MOD's from divide-by-zero	*
C* K. Brill/GSC          5/90   Took out start for ticks, labels, and   *
C*                              grid lines				*
C* T. Lee/GSC		 9/97	Added ax/ay in GDAXLB calling sequence	*
C* A. Hardy/GSC         10/98   Changed GCIRCL call, deleted norm coord *
C************************************************************************
	INCLUDE 'ERROR.PRM'
	INCLUDE 'XYDEF.CMN'
C*
	CHARACTER*(*)	chary (*)
	REAL		axary (*)
C*
	REAL		xp (2), yp (2)
C-----------------------------------------------------------------
C*	Check input parameters to see if action is to be taken.
C

	IF  ( ( iaxis  .lt. 1 ) .or. ( iaxis  .gt. 4 ) .or.
     +	      ( lbfrq .lt. 0 ) .or. ( lbfrq .gt. 9999 ) .or.
     +	      ( lgfrq .lt. 0 ) .or. ( lgfrq .gt. 9999 ) )  THEN
	    iret = NINVAL
	    RETURN
	END IF
C*
	iret = NORMAL
C
C*	Compute start position & frequency for labels and grids.
C
	lgfq = MOD ( lgfrq, 100 )
	lbfq = MOD ( lbfrq, 100 )
C
	IF  ( lgfq .eq. 0 )  THEN
	    iret = NINVAL
	    RETURN
	END IF
C
C*	Draw the grid lines which are circles.
C
	IF  ( ( iaxis .eq. 1 ) .or. ( iaxis .eq. 3 ) )  THEN
C
C*	    Loop through all the grid lines.
C
	    DO  ip = 1, np
		  IF  ( MOD ( ip - 1, lgfq ) .eq. 0 )  THEN
C
                    xend = axary (ip)
                    yend = 0.0
C
C*		    Draw a circle.
C
	            CALL GCIRCL  ( 'M', 0., 0., xend, yend, 50, ier )
C
C*		    Add label.
C
		    IF  ( lbfq .gt. 0 ) THEN
			IF  ( MOD ( ip - 1, lbfq ) .eq. 0 )  THEN
			    ax = axary (ip)
			    zx = 90.0
			    CALL GTRANS  ( 'M', 'V', 1, ax, zx, x, y,
     +					                          ier )
			    CALL GDAXLB  ( iaxis, 5, ax, x, y, 
     +					   chary (ip), ier )
			END IF
		    END IF
		  END IF
	    END DO
C
C*	    Draw radials.  The IF on NP deletes 360 when it would
C*	    overprint 0.
C
	  ELSE IF  ( ( iaxis .eq. 2 ) .or. ( iaxis .eq. 4 ) )  THEN
	    IF  ( (axary (1) .eq. 0.) .and. (axary (np) .eq. 360) )
     +                                                  np = np - 1
	    DO  ip = 1, np
	      IF ( MOD ( ip - 1, lgfq ) .eq. 0 )  THEN 
C
C*		    Draw line from center of circle to axpos.
C
		    xp (1) = 0.0
		    yp (1) = 0.0
		    xp (2) = axpos 
		    yp (2) = axary (ip)
		    CALL GLINE  ( 'M', 2, xp, yp, ier )
C
C*		    Add label.
C
		    IF  ( lbfq .gt. 0 ) THEN
			IF  ( MOD ( ip - 1, lbfq ) .eq. 0 )  THEN
			    ax = axpos
			    ay = axary (ip)
			    CALL GTRANS  ( 'M', 'V', 1, ax, ay, x, y, 
     +                                    ier )
			    CALL GDAXLB  ( iaxis, 5, ay, x, y, 
     +					   chary (ip), ier )
 			END IF
		    END IF
		END IF
	    END DO
	END IF
C*
	RETURN
	END
