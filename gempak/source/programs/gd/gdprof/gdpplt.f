	SUBROUTINE GDPPLT ( parm, border, line, marker, xaxis, yaxis,
     +			    iyplot, ystrt, ystop, ylbl, ny, xstrt,
     +			    xstop, xlbl, nx, npts, x, y, ratio, rmargn,
     +			    ixlbsf, ixglsf, ixtmsf, iylbsf, iyglsf,
     +                      iytmsf, wbszx, wbszy, iret )
C************************************************************************
C* GDPPLT								*
C*									*
C* This subroutine draws the background and profile in GDPROF.		*
C*									*
C* GDPPLT  ( PARM, BORDER, LINE, MARKER, XAXIS, YAXIS, IYPLOT, YSTRT,	*
C*	     YSTOP, YLBL, NY, XSTRT,  XSTOP, XLBL, NX, NPTS,		*
C*	     X, Y, RATIO, RMARGN, IXLBSF, IXGLSF, IXTMSF,		*
C*	     IYLBSF, IYGLSF, IYTMSF, WBSZX, WBSZY, IRET )		*
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
C*      WBSZX           REAL	        Wind barb size along x		*
C*      WBSZY           REAL            Wind barb size along y		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 1/89	Added margins				*
C* M. desJardins/GSFC	 7/89	Specify sides for ticks and labels	*
C* M. desJardins/GSFC	 8/89	Use 0 for hardware lines		*
C* K. Brill/GSC         12/89   Return wind barb size			*
C* K. Brill/GSC          5/90   Added the frequency parms		*
C* K. Brill/NMC          7/90   Added GG_SKEW call			*
C* S. Schotz/NMC	 7/90   Update for rmargn array			*
C* K. Brill/NMC          9/90   Add IN_LINE				*
C* S. Schotz/GSC	10/90	Call IN_LINE for border			*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	CHARACTER*(*)	border, line, marker, xaxis, yaxis, parm
	REAL		x (*), y (*), xlbl (*), ylbl (*), rmargn (*)
C*
	CHARACTER	tparm*4
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
	CALL GDPGRF  ( iyplot, ratio, xstrt, xstop, ystrt, ystop, 
     +		       rmargn, wbszx, wbszy, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set Skew T if necessary.
C
	IF  ( iyplot .eq. 4 ) THEN
	  tparm = parm (1:4)
     	  CALL GG_SKEW  ( xaxis, yaxis, tparm, ratio, xstrt, ystrt,
     +                        xstop, ystop, xlbl, nx, iret )
	  IF ( iret .ne. 0 ) RETURN
	END IF
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
