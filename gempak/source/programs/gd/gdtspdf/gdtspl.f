	SUBROUTINE GDTSPL  ( border, iyplot, ystrt, ystop, vclsfc,
     +			     havsfc,  ylbl, ny, xstrt, xstop,  
     +			     xtlbl, ctlbl, nx, npts, ixlbfr, ixglfr,
     +			     ixtmfr, ratio, rmargn, ilbfrq, iglfrq,
     +                       itmfrq, iret )
C************************************************************************
C* GDTSPL								*
C*									*
C* This subroutine draws the background for GDTSPDF.			*
C*									*
C* GDTSPL  ( BORDER, IYPLOT, YSTRT, YSTOP, VCLSFC, HAVSFC, YLBL, NY,	*
C*           XSTRT,  XSTOP,  XTLBL, CTLBL, NX, NPTS, IXLBFR, IXGLFR,	*
C*	     IXTMFR, RATIO, RMARGN, ILBFRQ, IGLFRQ, ITMFRQ, IRET )	*
C*									*
C* Input parameters:							*
C* 	BORDER		CHAR*		Background			*
C*	IYPLOT		INTEGER		Y coordinate type		*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*      VCLSFC (NX)	REAL		Vert coord location of sfc	*
C*	HAVSFC          LOGICAL		Flag for existence of sfc	*
C*	YLBL (NY)	REAL		Y axis label values		*
C*	NY		INTEGER		Number of y labels		*
C*	XSTRT		REAL		Left x value			*
C*	XSTOP		REAL		Right x value			*
C*	XTLBL (NX)	REAL		X axis label			*
C*	CTLBL (NX)	CHAR*		Label strings			*
C*	NX		INTEGER		Number of x grd pts/tick marks	*
C*	NPTS		INTEGER		Number of total points		*
C*	IXLBFR		INTEGER		X coord label frequency		*
C*	IXGLFR		INTEGER		X coord grid line frequency	*
C*	IXTMFR		INTEGER		X coord tick mark frequency	*
C*	RATIO		REAL		Height to width ratio		*
C*	RMARGN (4)	REAL		Input Margins			*
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
C* M. Li/SAIC		10/07		Modified from gdtxpl		*
C************************************************************************
	CHARACTER*(*)	border, ctlbl (*)
	CHARACTER	cdef (2)*12
	REAL		vclsfc (*), ylbl (*), xtics ( 125 ), rmargn (*),
     +			xsub (2), ysub (2), xtlbl (*)
C*
	LOGICAL         havsfc, scflag
C------------------------------------------------------------------------
	iret = 0
	cdef (1) = ' '
	cdef (2) = ' '
	values = 0.
C
C*	Set graph margins.
C
	IF ( rmargn (1) .lt. 0 )  THEN
	    CALL GSGMGN  ( 6., 4., 4., 2., ier )
	ELSE
	    CALL GSGMGN  ( rmargn (1), rmargn (2), rmargn (3), 
     +			   rmargn (4), ier )
	END IF
C
C*	Set graph.
C
	jxtype = 1
	jytype = iyplot
	IF ( iyplot .eq. 4 )  THEN
	    iret = -7
            RETURN
	END IF
	CALL GSGRAF ( jxtype, jytype, ratio, xstrt, ystrt, xstop,
     +		      ystop,  iret )
	IF ( iret .ne. 0 )  THEN
	    iret = -7
	    RETURN
	END IF
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
C*	Draw the border.
C
	CALL GSCOLR  ( ibcolr, ier )
	CALL GQLINE  ( ilntyp, ilntsw, ilnwid, ilnwsw, ier )
	CALL GSLINE  ( ibtyp, 0, ibwid, 0, ier ) 
	CALL GAAXIS  ( 1, ystrt, .true., ixlbfr, ixtmfr, ixglfr, nx,
     +		       xtlbl, ctlbl, ier )
	CALL GDAXIS  ( 3, ystop, .true., 000, 000, 000, 0, 0, xtlbl,
     +		       ier )
	CALL GDAXIS  ( 2, xstrt, .true., ilbfrq, itmfrq, iglfrq, -1,
     +		       ny, ylbl, ier )
	CALL GDAXIS  ( 4, xstop, .true., 000, 000, 000, 0, 0, ylbl,
     +		       ier )
C
C*	If surface exists and number of points is 125 or less, plot it.
C
	IF ( havsfc .and. ( nx .le. 125 ) ) THEN
C
C*	    Reset surface values to zero if they are below plot.
C
	    CALL GQBND ( 'M', xl, yb, xr, yt, ier )
	    diftst = ABS ( yb - yt )
	    DO i = 1, nx
		test = ABS ( vclsfc (i) - yt )
		IF ( test .gt. diftst ) vclsfc (i) = yb
	    END DO
C*
	    CALL GLINE ( 'M', nx, xtics, vclsfc, ier )
C
C*        Draw regularly spaced vertical lines to fill underground
C*        region of cross section plane.
C
	    nvln = 7
	    xxx = xstrt
	    frctn = 1. / FLOAT ( nvln )
	    DO i = 2, nx
		difr = vclsfc (i) - vclsfc ( i - 1 )
		difr = frctn * difr
		yyy  = vclsfc ( i - 1 )
		DO j = 1, nvln
		    xxx = xxx + frctn
		    xsub (1) = xxx
		    xsub (2) = xxx
		    yyy = yyy + difr
		    ysub (1) = ystrt
		    ysub (2) = yyy
		    CALL GLINE ( 'M', 2, xsub, ysub, ier )
		END DO
	    END DO
	END IF
C
C*	Restore original line settings.
C
	CALL GSLINE  ( ilntyp, 0, ilnwid, 0, ier )
C
C
	RETURN
	END
