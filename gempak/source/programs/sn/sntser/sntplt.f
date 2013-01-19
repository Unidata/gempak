	SUBROUTINE SNTPLT  ( border, line,   marker, iyplot, ystrt, 
     +			     ystop,  ylbl,   ny, iylbfr, iyglfr, iytmfr,
     +                       xstrt,  xstop, 
     +			     xtlbl,  ctlbl,  nx,     npts,   ixlbfr,
     +                       ixglfr, ixtmfr, x, y, ratio, rmargn, iret )
C************************************************************************
C* SNTPLT								*
C*									*
C* This subroutine draws the background and lines in SNTSER.		*
C*									*
C* SNTPLT  ( BORDER, LINE,   MARKER, IYPLOT, YSTRT, YSTOP, YLBL, NY,	*
C*           IYLBFR, IYGLFR, IYTMFR, XSTRT,  XSTOP,  XTLBL,  CTLBL,  	*
C*           NX, NPTS, IXLBFR, IXGLFR, IXTMFR, X, Y, RATIO, RMARGN, 	*
C*	     IRET )							*
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
C* 	IYLBFR		INTEGER		Y axis label frequency		*
C*	IYGLFR		INTEGER		Y axis grid line frequency	*
C*	IYTMFR		INTEGER		Y axis tick mark frequency	*
C*	XSTRT		REAL		Left x value			*
C*	XSTOP		REAL		Right x value			*
C*	XTLBL (NX)	REAL		X axis label positions		*
C*	CTLBL (NX)	CHAR*		X axis labels			*
C*	NX		INTEGER		Number of x labels		*
C*	NPTS		INTEGER		Number of points to plot	*
C*	IXLBFR		INTEGER		X axis label frequency		*
C* 	IXGLFR		INTEGER		X axis grid line frequency	*
C*	IXTMFR		INTEGER		X axis tick mark frequency	*
C*	X (NPTS)	REAL		X coordinates to plot		*
C*	Y (NPTS)	REAL		Y coordinates to plot		*
C*	RATIO		REAL		Height to width ratio		*
C*	RMARGN (4)	REAL		Margins				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = error setting graph	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from GDTPLT			*
C* S. Schotz/GSC	 7/90	Updates for parameters from IN_AXIS 	*
C*				and IN_TAXS				*
C* S. Schotz/GSC	10/90	Set hw flags to zero in GSLINE		*
C* S. Schotz/GSC	10/90	Set idec = -1 for GDAXIS		*
C* S. Schotz/GSC	10/90	Call IN_LINE for border			*
C* J. Whistler/SSAI	 6/91	Changed 1 to 1.0 in call to GSTICK	*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	border, line, marker, ctlbl (*)
	REAL		x (*), y (*), xtlbl (*), ylbl (*), rmargn (*)
C*
	INTEGER		iarr (4)
	LOGICAL		scflag
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
	CALL SNTGRF  ( iyplot, ratio, xstrt, xstop, ystrt, ystop, 
     +		       rmargn, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Parse and set background information including tick marks.
C
	CALL GSTICK   ( 2, 1.0, ier )
	CALL IN_LINE ( border, values, 1, ibcolr, ibtype, ibwid, iblab,
     +                 smth, fltr, scflag, ier )
	IF  ( ibcolr .ne. 0 )  THEN
	    CALL GSCOLR  ( ibcolr, ier )
	    CALL GSLINE  ( ibtyp, 0, ibwid, 0, ier )
C
C*	    Draw axes with or without labels according to user input.
C
            CALL GAAXIS  ( 1, ystrt, .true., ixlbfr, ixtmfr, ixglfr, nx,
     +			       xtlbl, ctlbl, ier )
	    CALL GAAXIS  ( 3, ystop, .true., 000, ixtmfr, 000, nx,
     +			       xtlbl, ctlbl, ier )
	    CALL GDAXIS  ( 2,  xstrt, .true., iylbfr, iytmfr, iyglfr,
     +			        -1, ny, ylbl,  ier )
	    CALL GDAXIS  ( 4,  xstop, .true., 000, 101, 000, 0, 
     +			       ny, ylbl,  ier )
	END IF
C
C*	Plot data.
C
	CALL IN_LINE  ( line, values, 1, iarr(1), iarr(2), iarr(3),
     +                  ilab, smth, fltr, scflag, ier )
C
C*	If line color is non-zero, draw line between non-missing points.
C
	IF  ( iarr (1) .ne. 0 )  THEN
	    CALL GSCOLR  ( iarr (1), ier )
	    CALL GSLINE  ( iarr (2), 0, iarr (3), 0, ier )
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
