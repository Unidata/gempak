	SUBROUTINE GDPYAX  ( ptype, yaxis, ivcord, iyaxis, ratio, 
     +			     ystrt, ystop, ylbl, nylbl, rmargn,
     +			     iylbsf, iyglsf, iytmsf, iret )
C************************************************************************
C* GDPYAX								*
C*									*
C* This subroutine returns the parameters to use for the y axis 	*
C* in GDPROF.								*
C*									*
C* GDPYAX  ( PTYPE, YAXIS, IVCORD, IYAXIS, RATIO, YSTRT, YSTOP, YLBL,	*
C*           NYLBL, RMARGN, IYLBSF, IYGLSF, IYTMSF, IRET )		*
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
C*	IYLBSF		INTEGER		Label frequency			*
C*	IYGLSF		INTEGER		Grid line frequency		*
C*	IYTMSF          INTEGER		Tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid axis type		*
C*                                      -11 = invalid axis request	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 1/89	Added margin				*
C* M. desJardins/GSFC	 5/89	Add mandatory levels above 100 mb	*
C* M. desJardins/GSFC	 7/89	Added sides for labels and ticks	*
C* K. Brill/GSC          5/90   Changes for new IN_AXIS			*
C* S. Schotz/GSC	 7/90	Added IN_PTYP				*
C* S. Schotz/GSC	 7/90	Update for new IN_AXIS calling sequence	*
C* K. Brill/NMC          9/90   Fixed error for invalid PTYPE		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ptype, yaxis
	REAL		ylbl (*), rmargn (*)
C*
	CHARACTER	parm*4
	LOGICAL         skewt
C*
C------------------------------------------------------------------------
	iret = 0
	skewt = .false.
	parm = ' '
	dmin = RMISSD
	dmax = RMISSD
C
C*	Get the values from PTYPE.
C
	CALL IN_PTYP ( ptype, iyaxis, ratio, rmargn, ier )
	IF  ( ier .ne. 0 ) THEN
	    iret = -7
	    RETURN
        END IF
C
C*	Get the range of values to use for the y-axis.
C
	ilfdef = 1
        igfdef = 0
        itfdef = 1
	CALL IN_AXIS  ( yaxis, ivcord, skewt, parm, dmin, dmax, 
     +                  ilfdef, igfdef, itfdef, ystrt, ystop, ylbl, 
     +                  nylbl, iylbsf, iyglsf, iytmsf, ier )
C*
	IF ( ier .ne. 0 ) THEN
	   iret = -11
	   CALL ER_WMSG ( 'GDPROF', iret, yaxis, ier )
	END IF
C*
	RETURN
	END
