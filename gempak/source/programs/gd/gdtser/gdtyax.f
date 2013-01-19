	SUBROUTINE GDTYAX  ( ptype,  yaxis,  ivcord, scale,  npts,  y,
     +			     iyaxis, ratio,  iscale, ystrt,  ystop, 
     +			     ylbl,   nylbl,  rmargn, rmin, rmax,
     +			     ilbfrq, iglfrq, itmfrq, iret )
C************************************************************************
C* GDTYAX								*
C*									*
C* This subroutine returns the parameters to use for the y axis 	*
C* in GDTSER.								*
C*									*
C* GDTYAX  ( PTYPE,  YAXIS,  IVCORD, SCALE,  NPTS,   Y,     IYAXIS,	*
C*           RATIO,  ISCALE, YSTRT,  YSTOP,  YLBL,   NYLBL, RMARGN,	*
C*	     RMIN,   RMAX,   ILBFRQ, IGLFRQ, ITMFRQ, IRET )		*
C*									*
C* Input parameters:							*
C*	PTYPE		CHAR*		Y axis type			*
C*	YAXIS		CHAR*		Ymin / ymax / yinc		*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	SCALE		CHAR*		Scale factor			*
C*	NPTS		INTEGER		Number of y points		*
C*									*
C* Input and output parameters:						*
C*	Y (NPTS)	REAL		Values of y points		*
C*									*
C* Output parameters:							*
C*	IYAXIS		INTEGER		Y axis integer type		*
C*	RATIO		REAL		Height to width ratio		*
C*	ISCALE		INTEGER		Integer version of scale factor	*
C*	YSTRT		REAL		Bottom y value			*
C*	YSTOP		REAL		Top y value			*
C*	YLBL  (NYLBL)	REAL		Y axis label values		*
C*	NYLBL		INTEGER		Number of y axis labels		*
C*	RMARGN (4)	REAL		Margins				*
C*	RMIN		REAL		Minimum data value		*
C*	RMAX		REAL		Maximum data value		*
C*      ILBFRQ          INTEGER         Label frequency			*
C*      IGLFRQ          INTEGER         Grid line frequency		*
C*      ITMFRQ          INTEGER         Tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid PTYPE input	*
C*                                       -9 = no points to plot		*
C**									*
C* Log:									*
C* G. Huffman/GSC	 2/89	Adapted from GDPYAX, GDPXAX		*
C* K. Brill/GSC		 3/90   Set default RATIO to zero		*
C* K. Brill/GSC          5/90   Added IN_AXIS				*
C* S. Schotz/GSC	 7/90	Added IN_PTYP				*
C* S. Schotz/GSC	 7/90	Update for new IN_AXIS calling sequence	*
C* L. Sager/NMC		 8/93   Replace GR_SCAL with IN_SCAL & GR_SSCL  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	ptype, yaxis, scale
	REAL		y (*), ylbl (*), rmargn (*)
C*
	CHARACTER	parm*4
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	parm = ' '
C
C*      Get the values from PTYPE.
C
	CALL IN_PTYP  ( ptype, iyaxis, ratio, rmargn, ier )
	IF  ( ier .ne. 0 )  THEN
            iret = - 7
            RETURN
        END IF
C
C*      Check validity of iyaxis
C
	IF  ( iyaxis .gt. 2 )  THEN
            iret = - 7
            RETURN
        END IF
C
C*	Get the scaling factor to use.
C
	CALL IN_SCAL  ( scale, iscale, iscalv, iret ) 
	CALL GR_SSCL  ( iscale, 1, npts, 1, 1, 1, npts, y,
     +			rmin, rmax, iret )
	IF  ( rmin .eq. rmax )  THEN
	    rmin =  1.E20
	    rmax = -1.E20
	    DO  kk = 1, npts
		IF  ( .not. ERMISS ( y (kk) ) )  THEN
		    rmin = AMIN1 ( rmin, y (kk) )
		    rmax = AMAX1 ( rmax, y (kk) )
		END IF
	    END DO
	    IF ( rmin .eq. 1.e20 .or. rmax .eq. -1.e20 ) THEN
	       iret = -9
	       CALL ER_WMSG ( 'GDTSER', iret, 'this', ier )
	       RETURN
	    END IF
	END IF
C
C*	Process the user input for yaxis.
C
	ilfdef = 1
	igfdef = 0
	itfdef = 1
	ivc = 0
	CALL IN_AXIS ( yaxis, ivc, .false., parm, rmin, rmax,
     +                 ilfdef, igfdef, itfdef, ystrt, ystop, ylbl, 
     +                 nylbl, ilbfrq, iglfrq, itmfrq, iret )
C*
	RETURN
	END
