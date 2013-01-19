	SUBROUTINE GDTPLT  ( border, line,   marker, iyplot, ystrt, 
     +			     ystop,  ylbl,   ny,   xstrt,  xstop, 
     +			     xtlbl,  ctlbl,  nx,   npts, x, y, xmndst,
     +			     ratio,  rmargn, itlbfr, itglfr, ittmfr,
     +                       ilbfrq, iglfrq, itmfrq, iret )
C************************************************************************
C* GDTPLT								*
C*									*
C* This subroutine draws the background and lines in GDTSER.		*
C*									*
C* GDTPLT  ( BORDER, LINE,   MARKER, IYPLOT, YSTRT, YSTOP, YLBL, NY,	*
C*           XSTRT,  XSTOP,  XTLBL,  CTLBL,  NX,    NPTS,  X, Y, 	*
C*           XMNDST, RATIO, RMARGN, ITLBFR, ITGLFR, ITTMFR, 		*
C*           ILBFRQ, IGLFRQ, ITMFRQ, IRET )				*
C*									*
C* Input parameters:							*
C* 	BORDER		CHAR*		Background			*
C*	LINE		CHAR*		Line				*
C*	MARKER		CHAR*		Marker				*
C*	IYPLOT		INTEGER		Y coordinate type		*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*	YLBL (NY)	REAL		Y axis label values		*
C*	NY		INTEGER		Number of y labels		*
C*	XSTRT		REAL		Left x value			*
C*	XSTOP		REAL		Right x value			*
C*	XTLBL (NX)	REAL		X axis label positions		*
C*	CTLBL (NX)	CHAR*		X axis labels			*
C*	NX		INTEGER		Number of x labels		*
C*	NPTS		INTEGER		Number of points to plot	*
C*	X (NPTS)	REAL		X coordinates to plot		*
C*	Y (NPTS)	REAL		Y coordinates to plot		*
C*      XMNDST          REAL		Minimum x separation		*
C*	RATIO		REAL		Height to width ratio		*
C*	RMARGN (4)	REAL		Margins				*
C*      ITLBFR          INTEGER         T axis label frequency		*
C*      ITGLFR          INTEGER         T axis grid line frequency	*
C*      ITTMFR          INTEGER         T axis tick mark frequency	*
C*      ILBFRQ          INTEGER         Y axis label start & frequency	*
C*      IGLFRQ          INTEGER         Y axis grid line strt & freq.	*
C*      ITMFRQ          INTEGER         Y axis tick mark strt & freq.	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/GSC	 2/89	Adapted from GDPPLT			*
C* M. desJardins/GSFC	 8/89	Use 0 for hardware lines		*
C* K. Brill/GSC          5/90   Added label, grd ln, tic frequencies	*
C* S. Schotz/GSC	 7/90   Update for rmargn array			*
C* S. Schotz/GSC	10/90	Set ndec = -1 for GDAXIS		*
C* S. Schotz/GSC	10/90	Call IN_LINE to parse LINE input	*
C* S. Schotz/GSC	10/90	Call IN_LINE to parse BORDER input	*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* T. Lee/SAIC		 7/05	Removed time gaps checking		*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	border, line, marker, ctlbl (*)
	REAL		x (*), y (*), xtlbl (*), ylbl (*), rmargn (*)
	LOGICAL		scflag
C*
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	values = 0.
C
C*	Save line characteristics.
C
	CALL GQLINE  ( ityp, ihw, iwid, iwhw, ier )
C
C*	Set graph coordinate system.
C
	CALL GDTGRF  ( iyplot, ratio, xstrt, xstop, ystrt, ystop, 
     +		       rmargn, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Parse and set background information including tick marks.
C
	CALL GSTICK   ( 2, 1.0, ier )
	CALL IN_LINE  ( border, values, 1, ibcolr, ibtyp, ibwid, iblab, 
     +                  smth, fltr, scflag, ier )
	IF  ( ibcolr .ne. 0 )  THEN
	    CALL GSCOLR  ( ibcolr, ier )
	    CALL GSLINE  ( ibtyp, 0, ibwid, 0, ier )
C
C*	    Draw axes with or without labels according to user input.
C
            CALL GAAXIS  ( 1, ystrt, .true., itlbfr, ittmfr, itglfr,
     +			       nx, xtlbl, ctlbl, ier )
	    CALL GAAXIS  ( 3, ystop, .true., 000, ittmfr, 000, nx,
     +			       xtlbl, ctlbl, ier )
            CALL GDAXIS  ( 2,  xstrt, .true., ilbfrq, itmfrq,
     +                         iglfrq, -1, ny, ylbl,  ier )
            CALL GDAXIS  ( 4,  xstop, .true., 000, itmfrq,
     +                         000, 0, ny, ylbl,  ier )
	END IF
C
C*	Plot data.
C
	CALL IN_LINE ( line, values, 1, icolor, itype, iwidth, ilabel,
     +                 smth, fltr, scflag, ier )
C
C*	If line color is non-zero, draw line between non-missing points.
C
	IF  ( icolor .ne. 0 )  THEN
	    CALL GSCOLR  ( icolor, ier )
	    CALL GSLINE  ( itype, 0, iwidth, 0, ier )
C
	    jgood = 0
	    jstrt = 1
	    DO  j = 1, npts
C
C*		If current point is missing, plot current polyline
C*		(if any) and reset pointers.
C
		IF  ( ERMISS ( y (j) ) )  THEN
		    IF  ( jgood .gt. 1 )  CALL GLINE  ( 'M', jgood,
     +					  x (jstrt), y (jstrt), ier )
		    jgood = 0
		    jstrt = j + 1
C
C*		  Not missing - increment counter.
C
		  ELSE
		    jgood = jgood + 1
		END IF
	    END DO
C
C*	    Flush remaining polyline (if any).
C
	    IF  ( jgood .gt. 1 )  CALL GLINE  ( 'M', jgood, x (jstrt), 
     +					      y (jstrt), ier )
C
	END IF
C
C*	Plot markers if color is non-zero and the point is not missing.
C
	CALL IN_MARK  ( marker, mkcolr, ier )
	IF  ( mkcolr .ne. 0 )  THEN
	    CALL GSCOLR  ( mkcolr, ier )
C
	    jgood = 0
	    jstrt = 1
	    DO  j = 1, npts
C
C*		If current point is missing, plot current polymarker
C*		(if any) and reset pointers.
C
		IF  ( ERMISS ( y (j) ) )  THEN
		    IF  ( jgood .gt. 0 )  CALL GMARK  ( 'M', jgood,
     +					  x (jstrt), y (jstrt), ier )
		    jgood = 0
		    jstrt = j + 1
C
C*		  Not missing - increment counter.
C
		  ELSE
		    jgood = jgood + 1
		END IF
	    END DO
C
C*	    Flush remaining polymarker (if any).
C
	    IF  ( jgood .gt. 0 )  CALL GMARK  ( 'M', jgood, x (jstrt),
     +					      y (jstrt), ier )
	END IF
C
C*	Reset line type.
C
	CALL GSLINE  ( ityp, 0, iwid, 0, ier )
C*
	RETURN
	END
