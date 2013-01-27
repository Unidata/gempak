	SUBROUTINE GDXPLT  ( border, ystrt, ystop,
     +			     ylbl, ny, xstrt, xstop, xlbl, nx, ilbfrq,
     +			     iglfrq, itmfrq, iret )
C************************************************************************
C* GDXPLT								*
C*									*
C* This subroutine draws the background for a cross section.		*
C*									*
C* GDXPLT  ( BORDER, YSTRT, YSTOP, VCLSFC, HAVSFC, YLBL, NY, XSTRT,	*
C* 	     XSTOP, XLBL, NX, ILBFRQ, IGLFRQ, ITMFRQ, IRET )		*
C*									*
C* Input parameters:							*
C* 	BORDER		CHAR*		Background			*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*      VCLSFC (NX)	REAL		Vert coord location of sfc	*
C*	HAVSFC          LOGICAL		Flag for existence of sfc	*
C*	YLBL (NY)	REAL		Y axis label values		*
C*	NY		INTEGER		Number of y labels		*
C*	XSTRT		REAL		Left x value			*
C*	XSTOP		REAL		Right x value			*
C*	XLBL		CHAR*		Xsect endpts from user input	*
C*	NX		INTEGER		Number of x grd pts/tick marks	*
C*      ILBFRQ          INTEGER         Label frequency			*
C*      IGLFRQ          INTEGER         Grid line frequency		*
C*      ITMFRQ          INTEGER         Tick mark frequency   		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid vert coord type	*
C**									*
C* Log:									*
C* K. F. Brill/GSC	 6/98   Created from GDPPLT			*
C* K. Brill/GSC          2/90   Activated line width in BORDER		*
C* S. Schotz/GSC	 7/90   Pass in margin values instead of string	*
C* K. Brill/NMC         10/90   Pass zero down for hw flag in GSLINE	*
C* S. Schotz/GSC	10/90	Set ndec to -1 for gdaxis		*
C* S. Schotz/GSC	10/90	Call IN_LINE for border			*
C* K. Brill/NMC		01/92	Remove margin and graph setup		*
C* S. Jacobs/NMC	 6/94	Offset the end point labels		*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C************************************************************************
	CHARACTER*(*)	border, xlbl
	CHARACTER	gpoint(2)*24, cdef(2)*12
	REAL		ylbl (*), xtics ( 125 ), 
     +			xsub(2), ysub(2)
	LOGICAL		scflag
C------------------------------------------------------------------------
	iret = 0
	cdef(1) = ' '
	cdef(2) = ' '
	values = 0.
C
C*	Draw background.
C
	CALL IN_LINE  ( border, values, 1, ibcolr, ibtyp, ibwid, iblab, 
     +                  smth, fltr, scflag, ier )
C
C*      RETURN here if there is to be no border.
C
	IF ( ier .ne. 0 .or. ibcolr .eq. 0 ) RETURN
C
C*      Generate x axis tic mark locations.
C
        xtics(1) = xstrt
	ntics = nx
	IF ( ntics .gt. 125 ) ntics = 125
	dtic = ( xstop - xstrt ) / FLOAT ( ntics - 1 )
	DO i = 2, ntics
	  xtics ( i ) = xtics ( i - 1 ) + dtic
	END DO
C
C*	Draw the border.
C
	    CALL GSCOLR  ( ibcolr, ier )
	    CALL GQLINE  ( ilntyp, ilntsw, ilnwid, ilnwsw, ier )
	    CALL GSLINE  ( ibtyp, 0, ibwid, 0, ier ) 
	    CALL GDAXIS  ( 1, ystrt, .true., 000, 101, 000, 0, ntics,
     +			   xtics, ier )
	    CALL GDAXIS  ( 3, ystop, .true., 000, 000, 000, 0, 0,
     +			   xtics, ier )
	    CALL GDAXIS  ( 2, xstrt, .true., ilbfrq, itmfrq, iglfrq,
     +                     -1, ny, ylbl, ier )
	    CALL GDAXIS  ( 4, xstop, .true., 000, 000, 000, 0, 0,
     +			   ylbl, ier )
C
C*	Restore original line settings.
C
	CALL GSLINE  ( ilntyp, 0, ilnwid, 0, ier )
C
C*      Label the end points along the x axis.
C
C* 	Split the input string into the expected substrings seperated by
C*	a > .
C
	CALL ST_LCUC ( xlbl, xlbl, ier )
	CALL ST_CLST ( xlbl, '>', cdef, 2, gpoint, nums, iret )
	iret = iret + ier
 	IF  ( iret .ne. 0 .or. nums .ne. 2 )  THEN
	    iret = -11
	    RETURN
	END IF
C
C*	Offset the end points from the edges of the plot, so that
C*	the labels are plotted correctly.
C
	xoffset  = ( xtics(2) - xtics(1) ) / 4.
	xtics(1) = xtics(1) + xoffset
	xtics(2) = xtics(ntics) - xoffset
C
	CALL GAAXIS  ( 1, ystrt, .false., 101, 000, 000, 2,
     +			   xtics, gpoint, ier )
C*
	RETURN
	END
