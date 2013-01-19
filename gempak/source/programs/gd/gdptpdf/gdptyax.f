	SUBROUTINE GDPTYAX  ( ptype, yaxis, ymin, ymax, iyaxis,
     +			      ratio, ystrt, ystop, ylbl, nylbl, rmargn,
     +	 		      iylbsf, iyglsf, iytmsf, iret )
C************************************************************************
C* GDPYAX								*
C*									*
C* This subroutine returns the parameters to use for the y axis 	*
C* in GDPTPDF								*
C*									*
C* GDPTYAX ( PTYPE, YAXIS, IVCORD, YMIN, YMAX, IYAXIS, RATIO, YSTRT,  	*
C*          YSTOP, YLBL, NYLBL, RMARGN, IYLBSF, IYGLSF, IYTMSF, IRET )	*
C*									*
C* Input parameters:							*
C*	PTYPE		CHAR*		Y axis type			*
C*	YAXIS		CHAR*		Ymin / ymax / yinc		*
C*	YMIN		REAL		Y min				*
C*	YMAX		REAL		Y MAX				*
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
C*					 -7 = invalid axis type		*
C*                                      -11 = invalid axis request	*
C**									*
C* Log:									*
C* M. Li/SAIC		08/07						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ptype, yaxis
	REAL		ylbl (*), rmargn (*)
C*
	CHARACTER	parm*32
	LOGICAL         skewt
C*
C------------------------------------------------------------------------
	iret = 0
	skewt = .false.
	parm = ' '
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
	ivc    = 0
	ilfdef = 1
        igfdef = 0
        itfdef = 1
	CALL IN_AXIS  ( yaxis, ivc, skewt, parm, ymin, ymax, 
     +                  ilfdef, igfdef, itfdef, ystrt, ystop, ylbl, 
     +                  nylbl, iylbsf, iyglsf, iytmsf, ier )

C*
	IF ( ier .ne. 0 ) THEN
	   iret = -11
	END IF
C*
	RETURN
	END
