	SUBROUTINE GSPLN ( sys, np, x, y, iret )
C************************************************************************
C* GSPLN								*
C*									*
C* This subroutine draws special lines in any coordinate system. Each	*
C* special line	is a series of straight segments connecting the given	*
C* array of points. The special lines are drawn using attributes 	*
C* defined by GSSPLN.							*
C*									*
C* GSPLN ( SYS, NP, X, Y, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*                                        'S' = screen coordinates      *
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	NP		INTEGER		Number of points		*
C*	X (NP)		REAL		X coordinates / latitudes	*
C*	Y (NP)		REAL		Y coordinates / longitudes	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/97	Copied from GLINE			*
C* C. Lin/EAI	 	 6/97	Modified for adding 'S' in sysup	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* S. Jacobs/NCEP	 4/98	Changed DSCOLR to DSCLR2		*
C* D.W.Plummer/NCEP      3/04   Changes for cylindrical processing      *
C* S. Gilbert/NCEP	 8/06	Changed to do work in U coordinates     *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'GTBUFF.CMN'
C*
	CHARACTER*(*)	sys
	REAL		x (*), y (*)
C
	CHARACTER	sysuc*1, sysucu*2, sys_U*2
	INTEGER         poly, roll
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check for at least two points.
C
	IF ( np .lt. 2 ) RETURN
C
C*	Check that device has been set.
C
	IF  ( ddev .eq. ' ' ) THEN
	    iret = NDVICE
	    RETURN
	END IF
C
C*	Find coordinate system for clipping.
C
	CALL ST_LCUC ( sys, sysuc, ier )
	isys = INDEX ( sysup, sysuc )
	IF  ( isys .eq. 0 ) THEN
	    iret = NOCORD
	    RETURN
	ELSE
	    iwndw = isys - 2
	    IF ( iwndw .gt. 3 ) iwndw = 3
	    IF ( iwndw .le. 0 ) iwndw = 1
	END IF
	sysucu = sysuc // CHNULL
	sys_U = 'U' // CHNULL
C
C*	Reset colors if necessary.
C
	IF  ( ( lcolr .ne. mcolr ) .or. ( lcolr2 .ne. mcolr2 ) )
     +	    CALL DSCLR2 ( lcolr, lcolr2, jcl, jcl2, ier )
C
C*	Loop for NP > buffer; 1 point is shared between passes.
C
	npass = ( ( np - 2 ) / ( LLMXPT - 1 ) ) + 1
	ib = 1
	DO  m = 1, npass
	    ie = ib + LLMXPT - 1
	    IF ( ie .gt. np ) ie = np
	    ip = ie - ib + 1
C
	    IF  ( ddev .eq. 'VG' )  THEN
C
C*		If the driver is VG, transform to M coord.
C
		CALL GTRANS ( sysuc, 'M', ip, x(ib), y(ib),
     +			      gx, gy, iret )
		IF ( iret .ne. 0 ) RETURN
		CALL DSPLN ( iwndw, ip, gx, gy, iret )
	      ELSE
		IF ( ( isys .lt. 6 ) .or. ( igmode .ne. 1 ) ) THEN
C
C*		    Process the D, N, V, or P coordinates or
C*		    non-map mode.
C
		    CALL GTRANS ( sysuc, 'D', ip, x(ib), y(ib),
     +				  gx, gy, iret )
		    IF ( iret .ne. 0 ) RETURN
C
C*		    Check for missing points; send points to driver.
C
		    im = 0
		    DO  i = 1, ip
			IF  ( ( .not. ERMISS ( gx ( i ) ) ) .and.
     +			      ( .not. ERMISS ( gy ( i ) ) ) ) THEN
			    im = im + 1
			    gx ( im ) = gx ( i )
			    gy ( im ) = gy ( i )
			  ELSE
			    CALL DSPLN ( iwndw, im, gx, gy, iret )
			    IF ( iret .ne. 0 ) RETURN
			    im = 0
			END IF
		    END DO
C
		    CALL DSPLN ( iwndw, im, gx, gy, iret )
		    IF ( iret .ne. 0 ) RETURN
C
		  ELSE
C
C*		      Otherwise, process the M or G coordinates and
C*		      map mode.
C
		      poly = 0
		      CALL CGR_RANGE ( sysucu, np, x, y, poly,
     & 		                       sys_U, roll, nout, gx, gy,
     & 				xll, yll, xur, yur, iret )
		      IF ( iret .ne. 0 ) RETURN
C
C*                  Transform and send line to driver.
C
                    DO  ir = 0, roll
C
		    CALL GTRANS ( 'U', 'D', nout, gx, gy, fx, fy, iret )
		    IF ( iret .ne. 0 ) RETURN
C
		    im = 0
		    DO  ii = 1, nout
		      IF  ( ( .not. ERMISS ( fx ( ii ) ) ) .and.
     &			    ( .not. ERMISS ( fy ( ii ) ) ) )  THEN
		        im = im + 1
		        fx ( im ) = fx ( ii )
			fy ( im ) = fy ( ii )
		      END IF
		    END DO
C
		    CALL DSPLN ( iwndw, im, fx, fy, iret)
		    IF ( iret .ne. 0 ) RETURN
C
		    IF ( ir .ne. roll )  THEN
			DO  ii = 1, nout
			  gx ( ii ) = gx ( ii ) - TWOPI
			END DO
		    END IF
C
		    END DO
C
		END IF
C
	    END IF
C
C*	    First point of next pass equals last point of this pass.
C
	    ib = ie
	END DO
C*
	RETURN
	END
