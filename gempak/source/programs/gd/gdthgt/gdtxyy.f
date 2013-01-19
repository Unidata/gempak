	SUBROUTINE GDTXYY ( ptype, yaxis, ivcord, iyaxis, ratio, 
     +			    ystrt, ystop, ylbl, nylbl, rmargn,
     +                      ilbfrq, iglfrq, itmfrq, iret )
C************************************************************************
C* GDTXYY								*
C*									*
C* This subroutine returns the parameters to use for the y axis 	*
C* in GDTHGT.								*
C*									*
C* GDTXYY  ( PTYPE, YAXIS, IVCORD, IYAXIS, RATIO, YSTRT, YSTOP, YLBL,	*
C*           NYLBL, RMARGN, ILBFRQ, IGLFRQ, ITMFRQ, IRET )		*
C*									*
C* Input parameters:							*
C*	PTYPE		CHAR*		Y axis type			*
C*	YAXIS		CHAR*		Ymin / ymax / yinc		*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*									*
C* Output parameters:							*
C*	IYAXIS		INTEGER		Y axis integer type		*
C*	RATIO		REAL		Height to width ratio		*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*	YLBL  (NYLBL)	REAL		Y axis label values		*
C*	NYLBL		INTEGER		Number of y axis labels		*
C*	RMARGN (4)	REAL		Margins				*
C*      ILBFRQ          INTEGER         Label frequency			*
C*      IGLFRQ          INTEGER         Grid line frequency		*
C*      ITMFRQ          INTEGER         Tick mark frequency 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid axis type		*
C*                                      -20 = invalid axis specifictn	*
C**									*
C* Log:									*
C* T.W.Barker/WR/SSD	 8/91	Created from GDXYAX (GDCROSS)		*
C************************************************************************
	CHARACTER*(*)	ptype, yaxis
	REAL		ylbl (*), rmargn (*)
C*
	CHARACTER	parm*4
	LOGICAL         skewt
C------------------------------------------------------------------------
	iret = 0
	skewt = .false.
	parm = ' '
C
C*      Get values from PTYPE.
C
	CALL IN_PTYP ( ptype, iyaxis, ratio, rmargn, ier )
	IF ( rmargn (1) .lt. 0.0 ) THEN
	    rmargn (1) = 5.
	    rmargn (2) = 5.
	    rmargn (3) = 5.
	    rmargn (4) = 3.
	END IF
C
C*      Check for invalid ptype or vertical coordinate selection.
C
        IF ( ier .ne. 0 .or. iyaxis .eq. 4 ) THEN
	    iret = -7
	    RETURN
	END IF	
C
C*	Get the range of values and labels to use for the y-axis.
C
	ilfdef = 1
	igfdef = 0
	itfdef = 1
	CALL IN_AXIS ( yaxis, ivcord, skewt, parm, dmin, dmax,
     +                 ilfdef, igfdef, itfdef, ystrt, ystop, ylbl, 
     +                 nylbl, ilbfrq, iglfrq, itmfrq, iret )
	IF ( iret .lt. 0 ) THEN
	    iret = -20
	    RETURN
	END IF
C
C*	Check order of values.
C
	IF ( ( ivcord .eq. 1 ) .and. ( ystrt .lt. ystop ) ) THEN
	    ysave = ystrt
	    ystrt = ystop
	    ystop = ysave
	ELSE IF ( ( ivcord .ne. 1 ) .and. ( ystrt .gt. ystop ) ) THEN
	    ysave = ystrt
	    ystrt = ystop
	    ystop = ysave
	END IF
C*
	RETURN
	END
