	SUBROUTINE DFILL ( iwndw, np, x, y, iret )
C************************************************************************
C* DFILL								*
C*									*
C* This subroutine draws a filled polygon on the current graphics	*
C* device.								*
C*									*
C* DFILL ( IWNDW, NP, X, Y, IRET )					*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of points		*
C*	X (NP)		REAL		X coordinates in device units	*
C*	Y (NP)		REAL		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Whistler/SSAI	10/91	Adapted from DLINE			*
C* M. Linda/GSC		 2/97	Delete NL & ISL; changed X & Y to reals	*
C* S. Jacobs/NCEP	 7/97	Added check for VG driver		*
C* S. Jacobs/NCEP	 2/98	Added smoothing				*
C* S. Jacobs/NCEP	 4/98	Added parametric curve smoothing	*
C* S. Jacobs/NCEP	 7/98	Added max points to the DPRMTC call	*
C* T. Lee/GSC		 7/98	Changed DPRMTC to CV_PRMT; Added CRVSCL	*
C* S. Jacobs/NCEP	 8/98	Changed call to CV_PRMT			*
C* S. Jacobs/NCEP	 5/99	Added call to CV_RDUC to reduce points	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GBUFFT.CMN'
C*
	REAL		x (*), y (*)
C*
	REAL		xcv (LLMXPT), ycv (LLMXPT)
	REAL		xo (LLMXPT), yo (LLMXPT)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If requested, reduce the number of points on the line.
C
	IF  ( trfilt .gt. 0.0 )  THEN
	    val = trfilt * bscalf
	    CALL CV_RDUC ( np, x, y, val, npt, xo, yo, ier )
	  ELSE
	    DO  i = 1, np
		xo(i) = x(i)
		yo(i) = y(i)
	    END DO
	    npt = np
	END IF
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HRFILL ( npt, xo, yo, iret )
	    RETURN
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Process points.
C
	IF  ( ( tdens  .gt. 0 ) .and.
     +	      ( msmtyp .eq. 1 ) .and.
     +	      ( npt    .gt. 2 ) )  THEN
C
C*	    Compute a smooth curve using splines.
C
	    CALL DSPLIN ( metype, npt, xo, yo, tdens,
     +			  nout, xcv, ycv, ier )
	    DO  i = 1, nout
		mgx ( i ) = NINT ( xcv ( i ) )
		mgy ( i ) = NINT ( ycv ( i ) )
	    END DO
	  ELSE IF  ( ( tdens  .gt. 0 ) .and.
     +		     ( msmtyp .eq. 2 ) .and.
     +		     ( npt    .gt. 2 ) )  THEN
C
C*	    Compute a smooth curve using a parametric curve.
C
	    CALL CV_PRMT ( npt, xo, yo, tdens, LLMXPT, crvscl,
     +			   0, 0, nout, xcv, ycv, ier )
	    DO  i = 1, nout
		mgx ( i ) = NINT ( xcv ( i ) )
		mgy ( i ) = NINT ( ycv ( i ) )
	    END DO
	  ELSE
C
C*	    Just use the original points.
C
	    DO  i = 1, npt
		mgx ( i ) = NINT ( xo ( i ) )
		mgy ( i ) = NINT ( yo ( i ) )
	    END DO
	    nout = npt
	END IF
C
	CALL IFILL ( nout, mgx, mgy, iret )
C*
	RETURN
	END
