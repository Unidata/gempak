	SUBROUTINE DSPLN ( iwndw, np, x, y, iret )
C************************************************************************
C* DSPLN								*
C*									*
C* This subroutine draws special lines on the current graphics device.	*
C*									*
C* DSPLN ( IWNDW, NP, X, Y, IRET )					*
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
C* D. Keiser/GSC	 3/97	Copied from DLINE			*
C* D.W.Plummer/NCEP	 7/97	Lines 4 and 6 get arrow processing	*
C* S. Jacobs/NCEP	 2/98	Added smoothing using splines		*
C* S. Jacobs/NCEP	 3/98	Changed call to ISPLN; Added comments	*
C* S. Jacobs/NCEP	 4/98	Added parametric curve smoothing	*
C* S. Jacobs/NCEP	 7/98	Added max points to the DPRMTC call	*
C* T. Lee/GSC		 7/98	Changed DPRMTC to CV_PRMT; Added CRVSCL	*
C* S. Jacobs/NCEP	 8/98	Changed call to CV_PRMT			*
C* S. Jacobs/NCEP	 5/99	Added call to CV_RDUC to reduce points	*
C* J. Wu/SAIC	 	10/01	add special line 24, 25 - kink lines	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
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
	    CALL HRSPLN ( npt, xo, yo, iret )
	    RETURN
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Process points if special line width is greater than 0.
C
	IF  ( mslwid .gt. 0 ) THEN
C
C*	    Compute a smooth curve using splines.
C
	    IF  ( ( tdens  .gt. 0 ) .and.
     +		  ( msmtyp .eq. 1 ) .and.
     +		  ( npt    .gt. 2 ) )  THEN
		CALL DSPLIN ( metype, npt, xo, yo, tdens,
     +			      nout, xcv, ycv, ier )
		DO  i = 1, nout
		    mgx ( i ) = NINT ( xcv ( i ) )
		    mgy ( i ) = NINT ( ycv ( i ) )
		END DO
	      ELSE IF  ( ( tdens  .gt. 0 ) .and.
     +		         ( msmtyp .eq. 2 ) .and.
     +		         ( npt    .gt. 2 ) )  THEN
C
C*	    	Compute a smooth curve using a parametric curve.
C
		CALL CV_PRMT ( npt, xo, yo, tdens, LLMXPT, crvscl,
     +			       0, 0, nout, xcv, ycv, ier )
		DO  i = 1, nout
		    mgx ( i ) = NINT ( xcv ( i ) )
		    mgy ( i ) = NINT ( ycv ( i ) )
		END DO
	      ELSE
C
C*		Just use the original points.
C
		DO  i = 1, npt
		    mgx ( i ) = NINT ( xo ( i ) )
		    mgy ( i ) = NINT ( yo ( i ) )
		END DO
		nout = npt
	    END IF
C
C*	    Save current line type and width, then set special line
C*	    width. Also save the current fill pattern, and the fill
C*	    to solid.
C
	    iltyp = mltyp
	    ilthw = mlthw
	    ilwid = mlwid
	    ilwhw = mlwhw
	    CALL DSLINE ( 1, 0, mslwid, 0, k1, k2, k3, k4, ier )
	    msvft = mfltyp
	    svfsz = tfilsz
	    CALL DSFILL ( 1.0, 1, fsize, itype, ier )
C
C*	    Draw the special line.
C
	    IF ( ( msltyp .eq. 24 ) .or. ( msltyp .eq. 25 ) ) THEN
		CALL IKINK ( nout, mgx, mgy, ier )
	      ELSE	      
	        CALL ISPLN ( nout, mgx, mgy, ier )
	    ENDIF
C
C*	    Restore original line type and width, and fill pattern.
C
	    CALL DSLINE ( iltyp, ilthw, ilwid, ilwhw, 
     +			  k1, k2, k3, k4, ier )
	    CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
	END IF
C*
	RETURN
	END
