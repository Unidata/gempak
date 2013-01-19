	SUBROUTINE GDPTXAX  ( xaxis, scale, iyaxis, npts, x, iscale, 
     +			     xstrt, xstop, xlbl, nxlbl, rmindt, rmaxdt, 
     +			     ilbfrq, iglfrq, itmfrq, iret )
C************************************************************************
C* GDPTXAX								*
C*									*
C* This subroutine gets the parameters to use for the x axis in		*
C* GDPTPDF.								*
C*									*
C* GDPTXAX  ( XAXIS, SCALE, IYAXIS, NPTS, X, ISCALE, XSTRT,		*
C*           XSTOP, XLBL, NXLBL, RMINDT, RMAXDT, ILBFRQ, IGLFRQ,	*
C*           ITMFRQ, IRET )						*
C*									*
C* Input parameters:							*
C*	XAXIS		CHAR*		Xmin / xmax / xinc		*
C*	SCALE		CHAR*		Scale factor			*
C*	IYAXIS		INTEGER		Y axis type			*
C*	NPTS		INTEGER		Number of x points		*
C*									*
C* Input and output parameters:						*
C*	X  (NPTS)	REAL		Values of x points		*
C*									*
C* Output parameters:							*
C*	ISCALE		INTEGER		Scaling factor			*
C*	XSTRT		REAL		Left value of x			*
C*	XSTOP		REAL		Right value of x		*
C*	XLBL  (NXLBL)	REAL		X axis labels			*
C*	NXLBL		INTEGER		Number of x axis labels		*
C*	RMINDT		REAL		Minimum scaled value		*
C*	RMAXDT		REAL		Maximum scaled value		*
C*	ILBFRQ		INTEGER		Label frequency			*
C*	IGLFRQ      	INTEGER		Grid line frequency		*
C*	ITMFRQ 		INTEGER 	Tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*                                       -12 = invalid xaxis request	*
C**									*
C* Log:									*
C* M. Li/SAIC     	8/07	Modified from GDPXAX			* 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	xaxis, scale
	REAL		x (*), xlbl (*)
C*
	LOGICAL		skewt
	CHARACTER       parm*32
C*
C------------------------------------------------------------------------
	iret = 0
C*
	ivc  = 0
	parm = ' '
	skewt = .false.
C*
	IF ( npts .le. 0 ) THEN
C
C*	  Set failsafe values for RMIN, RMAX.
C
          rmin = 0
	  rmax = 1.
          rmindt = rmin
          rmaxdt = rmax
C*
	ELSE
C
C*	  Get the scaling factor to use.
C
	  CALL IN_SCAL  ( scale, iscale, iscalv, iret )
	  CALL GR_SSCL  ( iscale, 1, npts, 1, 1, 1, npts, x,
     +			  rmin, rmax, iret )
	  rmindt = rmin
	  rmaxdt = rmax
C
C*	  Get range of data for x axis.  Set reasonable values if the
C*	  data are few (.lt. 4 points) or constant (both flagged in
C*	  GR_SCAL by RMIN = RMAX = 0).
C
	  IF  ( rmin .eq. rmax )  THEN
	      rmin = x ( 1 )
	      rmax = x ( 1 )
	      DO  kk = 1, npts
	          rmin = AMIN1 ( rmin, x (kk) )
		  rmax = AMAX1 ( rmax, x (kk) )
	      END DO
	  END IF
C*
        END IF
C
C*	Get the values to use for the x-axis.
C
        ilfdef = 1
        igfdef = 0
        itfdef = 1
	CALL IN_AXIS  ( xaxis, ivc, skewt, parm, rmin, rmax,
     +                  ilfdef, igfdef, itfdef, xstrt, xstop, xlbl, 
     +                  nxlbl, ilbfrq, iglfrq, itmfrq, ier )
C*
	IF ( ier .ne. 0 ) THEN
	  iret = -12
	  CALL ER_WMSG ( 'GDPTPDF' ,iret, xaxis, ier )
	END IF
	RETURN
	END
