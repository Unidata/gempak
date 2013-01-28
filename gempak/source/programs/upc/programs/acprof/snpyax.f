	SUBROUTINE SNPYAX  ( ptype, yaxis, ivcord, iyaxis, ratio, 
     +			     ystrt, ystop, ylbl, nylbl, rmargn,
     +                       iylbfr, iyglfr, iytmfr, iret )
C************************************************************************
C* SNPYAX								*
C*									*
C* This subroutine returns the parameters to use for the y axis 	*
C* in SNPROF.								*
C*									*
C* SNPYAX  ( PTYPE, YAXIS, IVCORD, IYAXIS, RATIO, YSTRT, YSTOP, YLBL,	*
C*           NYLBL, RMARGN, IYLBFR, IYGLFR, IYTMFR, IRET )		*
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
C*      IYLBFR          INTEGER         Label frequency			*
C*      IYGLFR          INTEGER         Grid line frequency		*
C*      IYTMFR          INTEGER         Tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid axis type		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 1/89	Added margin				*
C* M. desJardins/GSFC	 5/89	Add mandatory levels above 100 mb	*
C* K. Brill/GSC          6/90   Changes for IN_AXIS			*
C* M. desJardins/GSFC	 7/90	Added IN_PTYP				*
C* S. Schotz/GSC	 7/90	Update for new IN_AXIS calling sequence	*
C* K. Brill/NMC		 1/91   Set default grid line freq to zero	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	ptype, yaxis
	REAL		ylbl (*), rmargn (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the values from PTYPE.
C
	CALL IN_PTYP  ( ptype, iyaxis, ratio, rmargn, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -4
	    RETURN
	END IF
C
	ymin = RMISSD
	ymax = RMISSD
	ilfdef = 1
	igfdef = 0
	itfdef = 1
	CALL IN_AXIS ( yaxis, ivcord, .false., ' ', ymin, ymax,
     +                 ilfdef, igfdef, itfdef, ystrt, ystop, ylbl, 
     +                 nylbl, iylbfr, iyglfr, iytmfr, iret )
C*
	RETURN
	END
