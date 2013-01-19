	SUBROUTINE SNTYAX  ( ptype,  yaxis,  npts,   y,
     +			     iyaxis, ratio,  ystrt,  ystop,  ylbl,
     +			     nylbl,  rmargn, datmin, datmax, 
     +			     iylbfr, iyglfr, iytmfr, iret )
C************************************************************************
C* SNTYAX								*
C*									*
C* This subroutine returns the parameters to use for the y axis 	*
C* in SNTSER.								*
C*									*
C* SNTYAX  ( PTYPE,  YAXIS, NPTS,  Y,      IYAXIS, RATIO,		*
C*           YSTRT,  YSTOP, YLBL,   NYLBL, RMARGN, DATMIN, DATMAX,	*
C*           IYLBFR, IYGLFR, IYTMFR, IRET )				*
C*									*
C* Input parameters:							*
C*	PTYPE		CHAR*		Y axis type			*
C*	YAXIS		CHAR*		Ymin / ymax / yinc		*
C*	NPTS		INTEGER		Number of y points		*
C*	Y (NPTS)	REAL		Values of y points		*
C*									*
C* Output parameters:							*
C*	IYAXIS		INTEGER		Y axis integer type		*
C*	RATIO		REAL		Height to width ratio		*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*	YLBL  (NYLBL)	REAL		Y axis label values		*
C*	NYLBL		INTEGER		Number of y axis labels		*
C*	RMARGN (4)	REAL		Margins				*
C*	DATMIN		REAL		Minimum of data values		*
C*	DATMAX		REAL		Maximum of data values		*
C*	IYLBFR		INTEGER		Y axis label frequency		*
C*	IYGLFR		INTEGER		Y axis grid line frequency	*
C*	IYTMFR		INTEGER		Y axis tick mark frequency	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid PTYPE		*
C*					 -2 = invalid YAXIS		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from GDTYAX			*
C* S. Schotz/GSC	 7/90	Changes for call to IN_PTYP and 	*
C*				and IN_AXIS				*
C* J. Whistler/SSAI	 6/91	Changed iret to -11 for invalid PTYPE	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	ptype, yaxis
	REAL		y (*), ylbl (*), rmargn (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get values from PTYPE
C
	CALL IN_PTYP ( ptype, iyaxis, ratio, rmargn, ier )
	IF  ( ier .ne. 0 ) THEN
            iret = +2
	    iyaxis = 1
        END IF
C
C*	Check the type of y axis.
C
	IF  ( (iyaxis .ne. 1) .and. (iyaxis .ne. 2) )  THEN
	    iret = +2
	    iyaxis = 1
	END IF
C
C*      Get range of y values
C
	CALL GR_STAT  ( y, 1, npts, 1, 1, 1, npts, datmin, datmax,
     +			ravg, rdev, ier )
C
C*	Get the values for YAXIS.
C
	CALL IN_AXIS ( yaxis, 0, .false., ' ', datmin, datmax, 1,
     +                 0, 1, ystrt, ystop, ylbl, nylbl, iylbfr, 
     +                 iyglfr, iytmfr, ier )
	IF  ( ier .ne. 0 ) iret = -11
C*
	RETURN
	END
