	SUBROUTINE SNSBOR  ( border, ybot, ytop, yaxval, nyval, iylbfr, 
     +			     iyglfr, iytmfr, xmin, xmax, nstn, stns, 
     +			     sloc, x, pontha, toptmp, topwnd, xtlbl,
     +			     ctlbl, nxlbl, ixlbfr, ixglfr, ixtmfr, 
     +			     timflg, iret )
C************************************************************************
C* SNSBOR								*
C*									*
C* This stations draws the background and the surface for a cross	*
C* section plot.							*
C*									*
C*  The plot background consists of a pressure axis, a horizontal 	*
C*  axis with the station names, a filled region indicating the  	*
C*  part of the plot below the earth surface, and vertical lines	*
C*  at each station.  The station lines are the specified line type	*
C*  up to the level at which wind data stop, and are dotted from	*
C*  there to the level at which temperature data stop.  The color	*
C*  and other attributes for the background axses and labels are	*
C*  given by the first numbers separated by semicolons in the color	*
C*  number, line type and line width entry sections for BORDER.  The	*
C*  second set of numbers applies to the station lines, and the third	*
C*  set to the underground color fill (for which the line type and	*
C*  width do not apply).  To eliminate, the background, the station	*
C*  lines, or the underground fill, just set the corresponding color	*
C*  number to zero.  If one color number is entered, it is used for	*
C*  all three; if two are entered, the second is used for both the	*
C*  station lines and the underground fill.  So, BORDER has the		*
C*  following entries:							*
C*									*
C*   background color; station line color; underground fill color /	*
C*   background line type; station line type /				*
C*   background line width; station line width				*
C*									*
C*									*
C* SNSBOR  ( BORDER, YBOT, YTOP, YAXVAL, NYVAL, IYLBFR, IYGLFR, 	*
C*           IYTMFR, XMIN, XMAX, NSTN, STNS, SLOC, X, PONTHA, 		*
C*           TOPTMP, TOPWND, XTLBL, CTLBL, NXLBL, IXLBFR, IXGLFR,	*
C*           IXTMFR, TIMFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	BORDER		CHAR*		Border col/type/width (2,3 n/a)	*
C*	YBOT		REAL		Bottom y value			*
C*	YTOP		REAL		Top y value			*
C* 	YAXVAL (NYVAL)	REAL		Y axis values			*
C*	NYVAL		INTEGER		Number of y axis values		*
C*	IYLBFR		INTEGER		Label frequency			*
C*	IYGLFR		INTEGER		Grid line frequency		*
C*	IYTMFR		INTEGER		Tick mark frequency		*
C*	XMIN		REAL		Minimum x station value		*
C*	XMAX		REAL		Maximum x station value		*
C*	NSTN		INTEGER		Number of stations		*
C*	STNS (NSTN)	CHAR*		Station ids			*
C*	SLOC (NSTN)	REAL		Station locations		*
C*	X (NSTN)	REAL		Grid locations			*
C*	PONTHA(LLMAXD,*)REAL		Pressure on isentropes		*
C*	TOPTMP (NSTN)	REAL		Minimum pressure with temp	*
C*	TOPWND (NSTN)	REAL		Minimum pressure with wind	*
C*	XTLBL (NXLBL)	REAL		X label points			*
C*	CTLBL (NXLBL)	CHAR*		X labels			*
C*	NXLBL		INTEGER		Number of x axis labels		*
C*	IXLBFR		INTEGER		X label frequency		*
C*	IXGLFR		INTEGER		X grid line frequency		*
C*	IXTMFR		INTEGER		X tick mark frequency		*
C*	TIMFLG		LOGICAL		Time section flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* I. Graffman/RDS	 3/87	new missing values			*
C* G. Huffman/GSC	11/88	new BORDER value			*
C* M. desJardins/GSFC	 8/89	Use 0 for hardware lines		*
C* M. desJardins/GSFC	 9/90	Eliminate reset of graph coordinates	*
C* S. Schotz/GSC	10/90	Set ndec = -1 for GDAXIS		*
C* S. Schotz/GSC	10/90	Call IN_LINE for border			*
C* M. desJardins/GSFC	 3/91	Expand capabilities for time section	*
C* K. Brill/NMC		03/92	Check for missing sfc values		*
C* S. Jacobs/NCEP	10/96	Fixed loop for drawing grid lines and	*
C*				surface pressure border			*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* K. Brill/EMC		 4/99   Fill undergrd; 3 color #s in BORDER	*
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	border, stns (*), ctlbl (*)
	REAL		sloc(*), x(*), pontha (LLMAXD,*), toptmp (*),
     +			topwnd (*), yaxval (*), xtlbl (*)
	LOGICAL		timflg
C*
	INTEGER		icbor (3), ibtyp (3), ibwid (3), iblab (3)
	REAL		xx (4), yy (4)
	LOGICAL		scflag
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	values = 0.
C
C*	Get input values; the "surface" line is set to the background
C*	color.
C
	CALL IN_LINE  ( border, values, 3, icbor, ibtyp, ibwid, iblab,
     +                  smth, fltr, scflag, ier )
C
C*	Draw background.
C
	IF  ( icbor(1) .ne. 0 ) THEN
C
C*	    Set background color and line attributes.
C
	    CALL GSCOLR  ( icbor(1), ier )
	    CALL GSLINE  ( ibtyp(1), 0, ibwid(1), 0, ier )
C
C*	    Draw y axis.
C
	    CALL GDAXIS ( 2, xmin, .true., iylbfr, iytmfr, iyglfr, -1,
     +			   nyval, yaxval, ier )
C
C*	    Draw x axis.  
C
	    IF  ( timflg )  THEN
		CALL GAAXIS  ( 1, ybot, .true., ixlbfr, ixtmfr, ixglfr,
     +			       nxlbl, xtlbl, ctlbl, ier )
	      ELSE
		CALL GAAXIS ( 1, ybot, .true., 101, 0, 0, nstn, sloc,
     +	 		      stns, ier )
	    END IF
	    xx (1) = xmin
	    yy (1) = yaxval (nyval)
	    xx (2) = xmax
	    yy (2) = yy(1)
	    xx (3) = xx(2)
	    yy (3) = ybot
	    CALL GLINE ( 'M', 3, xx, yy, ier )
	END IF
	IF ( icbor (2) .ne. 0 ) THEN
C
C*	    Draw station line.
C
	    CALL GSCOLR  ( icbor(2), ier )
	    CALL GSLINE  ( ibtyp(2), 0, ibwid(2), 0, ier )
	    DO  i = 1, nstn
		xx (1) = sloc (i)
		xx (2) = sloc (i)
		yy (1) = ybot
		IF  ( .not. ERMISS ( topwnd (i) ) )  THEN
		    yy (2) = topwnd (i)
		    CALL GLINE  ( 'M', 2, xx, yy, ier )
		    yy (1) = yy (2)
		END IF
		IF  ( ( .not. ERMISS (toptmp (i) ) ) .and.
     +		      ( ( toptmp (i) .lt. topwnd (i) ) .or. 
     +			( ERMISS (topwnd (i) ) ) ) )  THEN
		    yy (2) = toptmp (i)
		    CALL GQLINE  ( i1, i2, i3, i4, ier )
		    CALL GSLINE  (  10,  0, i3,  0, ier )
		    CALL GLINE   ( 'M', 2, xx, yy, ier )
		    CALL GSLINE  ( i1,  0, i3,  0, ier )
		END IF
	    END DO
C
C*	    Reset solid line.
C
	    CALL GSLINE  (  i1, 0, i3, 0, ier )
	END IF
	IF ( icbor (3) .ne. 0 ) THEN
	    CALL GSCOLR  ( icbor(3), ier )
	    yy (1) = ybot
	    yy (4) = ybot
	    DO i = 1, LLMAXD-1
		IF ( ERMISS ( pontha(i,1) ) ) THEN
C
C*		    Skip this point.
C
		ELSE
C
C*		    Do the color fill for this quadrilateral.
C
		    xx (1) = x (i)
		    xx (2) = x (i)
		    yy (2) = pontha (i,1)
		    IF ( .not. ERMISS ( pontha (i+1,1) ) ) THEN
			xx (3) = x (i+1)
			yy (3) = pontha (i+1,1)
			xx (4) = xx (3)
	     		CALL GFILL  ( 'M', 4, xx, yy, ier )
		    END IF
		END IF
	    END DO
	END IF
C*
	RETURN
	END
