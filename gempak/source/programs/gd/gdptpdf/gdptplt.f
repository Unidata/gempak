	SUBROUTINE GDPTPLT (  border, line, marker,
     +			    iyplot, ystrt, ystop, ylbl, ny, xstrt,
     +			    xstop, xlbl, nx, npts, x, y, ratio, rmargn,
     +			    ixlbsf, ixglsf, ixtmsf, iylbsf, iyglsf,
     +                      iytmsf, iret )
C************************************************************************
C* GDPTPLT								*
C*									*
C* This subroutine draws the background and probability in GDPTPDF.	*
C*									*
C* GDPTPLT  ( PARM, BORDER, LINE, MARKER, XAXIS, YAXIS, IYPLOT, YSTRT,	*
C*	     YSTOP, YLBL, NY, XSTRT,  XSTOP, XLBL, NX, NPTS,		*
C*	     X, Y, RATIO, RMARGN, IXLBSF, IXGLSF, IXTMSF,		*
C*	     IYLBSF, IYGLSF, IYTMSF, IRET )				*
C*									*
C* Input parameters:							*
C* 	PARM            CHAR*           Parameter being plotted		*
C* 	BORDER		CHAR*		Background			*
C*	LINE		CHAR*		Line				*
C*	MARKER		CHAR*		Marker				*
C*	XAXIS           CHAR*           User input for X axis		*
C*	YAXIS           CHAR*           User input for Y axis		*
C*	IYPLOT		INTEGER		Y coordinate type		*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*	YLBL (NY)	REAL		Y axis label values		*
C*	NY		INTEGER		Number of y labels		*
C*	XSTRT		REAL		Left x value			*
C*	XSTOP		REAL		Right x value			*
C*	XLBL (NX)	REAL		X axis label values		*
C*	NX		INTEGER		Number of x labels		*
C*	NPTS		INTEGER		Number of points to plot	*
C*	X (NPTS)	REAL		X coordinates to plot		*
C*	Y (NPTS)	REAL		Y coordinates to plot		*
C*	RATIO		REAL		Height to width ratio		*
C*	RMARGN (4)	REAL		Margins				*
C*	IXLBSF		INTEGER		X label frequency		*
C*	IXGLSF		INTEGER		X grid line frequency		*
C*	IXTMSF		INTEGER		X tick mark frequency		*
C*	IYLBSF		INTEGER		Y label frequency		*
C*      IYGLSF          INTEGER         Y grid line frequency	        *
C*      IYTMSF          INTEGER         Y tick mark frequency           *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. Li/SAIC		08/07						*
C************************************************************************
	CHARACTER*(*)	border, line, marker
	REAL		x (*), y (*), xlbl (*), ylbl (*), rmargn (*)
C*
	INTEGER		iarr (4)
	LOGICAL		scflag
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
	CALL GDPTGRF ( iyplot, ratio, xstrt, xstop, ystrt, ystop, 
     +		       rmargn, iret )

	IF  ( iret .ne. 0 )  RETURN
C
C*	Draw background.
C
	CALL IN_LINE  ( border, values, 1, ibcolr, ibtyp, ibwid, iblab,
     +                  smth, fltr, scflag, ier )

	IF  ( ibcolr .ne. 0 )  THEN
	    CALL GSCOLR  ( ibcolr, ier )
	    CALL GSLINE  ( ibtyp, 0, ibwid, 0, ier )
	    CALL GDAXIS  ( 1, ystrt, .true., ixlbsf, ixtmsf, ixglsf,
     +			   -1, nx, xlbl, ier )
	    CALL GDAXIS  ( 3, ystop, .true., 0, 0, 0, 0, nx, xlbl, ier )
	    CALL GDAXIS  ( 2, xstrt, .true., iylbsf, iytmsf, iyglsf,
     +                     -1, ny, ylbl, ier )
	    CALL GDAXIS  ( 4, xstop, .true., 0, 0, 0, 0, ny, ylbl, ier )
	END IF
C
C*	Plot data.
C
	CALL IN_LINE ( line, val, 1, iarr (1), iarr (2), iarr (3),
     +                 ilab, smth, fltr, scflag, ier )
	IF  ( iarr (3) .le. 0 )  iarr (3) = 1
C
C*	If line color is non-zero, draw line.
C
	IF  ( iarr (1) .ne. 0 )  THEN
	    CALL GSCOLR  ( iarr (1), ier )
	    CALL GSLINE  ( iarr (2), 0, iarr (3), 0, ier )
	    CALL GLINE   ( 'M', npts, x, y, ier )
	END IF
C
C*	If marker color is non-zero, plot markers.
C
	CALL IN_MARK  ( marker, mkcolr, ier )
	IF  ( mkcolr .ne. 0 )  THEN
	    CALL GSCOLR  ( mkcolr, ier )
	    CALL GMARK   ( 'M', npts, x, y, ier )
	END IF
C
C*	Reset line type.
C
	CALL GSLINE  ( ityp, 0, iwid, 0, ier )
C*
	RETURN
	END
