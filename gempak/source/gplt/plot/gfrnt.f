	SUBROUTINE GFRNT ( sys, np, x, y, iret )
C************************************************************************
C* GFRNT								*
C*									*
C* This subroutine draws fronts in any coordinate system.  Each front	*
C* is a series of straight segments connecting the given array of	*
C* points.  The fronts are drawn using attributes defined by GSFRNT.	*
C*									*
C* GFRNT ( SYS, NP, X, Y, IRET )					*
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
C* E. Wehner/EAi	10/96	Based on GLINE				*
C* M. Linda/GSC		 1/97	Changed to pass X & Y as reals on down	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* C. Lin/EAI	 	 6/97	Modified for adding 'S' in sysup	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* S. Jacobs/NCEP	 4/98	Changed DSCOLR to DSCLR2		*
C* S. Gilbert/NCEP	 8/06	Changed to do work in "U" coordinates   *
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
	CHARACTER	sysuc*1
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
		CALL DFRNT ( iwndw, ip, gx, gy, iret )
	      ELSE
		IF  ( ( isys .lt. 6 ) .or. ( igmode .ne. 1 ) )  THEN
C
C*		    Process the D, N, V, or P coordinates or
C*		    non-map mode.
C
		    CALL GTRANS ( sysuc, 'D', ip, x(ib), y(ib),
     +				  gx, gy, iret )
		    IF  ( iret .ne. 0 )  RETURN
C
C*                  Check for missing points; send points to driver.
C
		    im = 0
		    DO  i = 1, ip
			IF  ( ( .not. ERMISS ( gx ( i ) ) ) .and.
     +			      ( .not. ERMISS ( gy ( i ) ) ) ) THEN
			    im = im + 1
			    gx ( im ) = gx ( i )
			    gy ( im ) = gy ( i )
			ELSE
			    CALL DFRNT ( iwndw, im, gx, gy, iret )
			    IF ( iret .ne. 0 ) RETURN
			    im = 0
			END IF
		    END DO
C
		    CALL DFRNT ( iwndw, im, gx, gy, iret )
		    IF ( iret .ne. 0 ) RETURN
		ELSE
C
C*		    Otherwise, process the M or G coordinates and
C*		    map mode.
C
		    CALL GTRANS ( sysuc, 'U', ip, x(ib), y(ib),
     +				  gx, gy, iret )
		    IF ( iret .ne. 0 ) RETURN
C
C*		    Check all points for points going the wrong way.
C
		    jb = 1
		    DO i = 1, ip - 1
C
			xdis = ABS ( gx ( i ) - gx ( i + 1 ) )
			ydis = ABS ( gy ( i ) - gy ( i + 1 ) )
C
			IF  ( ( xdis .gt. ueps ) .or.
     +			      ( ydis .gt. veps ) )  THEN
C
C*			    Process points up to the breakpoint.
C
			    IF ( ( .not. ERMISS ( gx ( i ) ) ) .and.
     +			         ( .not. ERMISS ( gy ( i ) ) ) ) THEN
C
C*			    	Save pair of points around the
C*				breakpoint.
C
				gxsav0 = gx ( i )
				gysav0 = gy ( i )
				gxsav1 = gx ( i + 1 )
				gysav1 = gy ( i + 1 )
C
C*			    	Project point so line goes the right way.
C
				IF ( ( .not. ERMISS (gx(i+1)) ) .and.
     +				     ( .not. ERMISS (gy(i+1)) ) ) THEN
C
				    xspn = ABS ( xbndrl - xbndll )
				    yspn = ABS ( ybndbl - ybndtl )
				    xdel = ABS ( xspn - xdis )
				    ydel = ABS ( yspn - ydis )
C
				    xl = ABS ( gx ( i ) - xbndll )
				    xr = ABS ( gx ( i ) - xbndrl )
				    yt = ABS ( gy ( i ) - ybndtl )
				    yb = ABS ( gy ( i ) - ybndbl )
C
				    IF ( xdis .gt. ueps ) THEN
					IF ( xl .lt. xr ) THEN
					    gx (i+1) = gx (i) - xdel
					ELSE
					    gx (i+1) = gx (i) + xdel
					END IF
				    END IF
C
				    IF ( ydis .gt. veps ) THEN
					IF ( yt .lt. yb ) THEN
					    gy (i+1) = gy (i) + ydel
					ELSE
					    gy (i+1) = gy (i) - ydel
					END IF
				    END IF
C
				    jp = i - jb + 2
				ELSE
				    jp = i - jb + 1
				END IF
C
				CALL GTRANS ( 'U', 'D', jp,
     +					      gx(jb), gy(jb),
     +					      gx(jb), gy(jb), iret )
				IF ( iret .ne. 0 ) RETURN
C
C*			    	Send points before the breakpoint
C*				to driver.
C
				CALL DFRNT ( iwndw, jp, gx(jb), gy(jb),
     +					     iret )
				IF ( iret .ne. 0 ) RETURN
C
C*			    	Restore the pair of points around
C*				breakpoint.
C
				gx ( i )     = gxsav0
				gy ( i )     = gysav0
				gx ( i + 1 ) = gxsav1
				gy ( i + 1 ) = gysav1
C
C*			    	Process the other side of the breakpoint.
C
				IF ( ( .not. ERMISS (gx(i+1)) ) .and.
     +				     ( .not. ERMISS (gy(i+1)) ) ) THEN
C
				    xl = ABS ( gx (i+1) - xbndll )
				    xr = ABS ( gx (i+1) - xbndrl )
				    yt = ABS ( gy (i+1) - ybndtl )
				    yb = ABS ( gy (i+1) - ybndbl )
C
				    IF ( xdis .gt. ueps ) THEN
					IF ( xl .lt. xr ) THEN
					    gx (i) = gx (i+1) - xdel
					ELSE
					    gx (i) = gx (i+1) + xdel
					END IF
				    END IF
C
				    IF ( ydis .gt. veps ) THEN
					IF ( yt .lt. yb ) THEN
					    gy (i) = gy (i+1) + ydel
					ELSE
					    gy (i) = gy (i+1) - ydel
					END IF
				    END IF
C
				    jb = i
				ELSE
				    jb = i + 1
				END IF
			    ELSE
				jb = i + 1
			    END IF
			END IF
		    END DO
C
C*		    Transform and send line to driver.
C
		    jp = ip - jb + 1
		    CALL GTRANS ( 'U', 'D', jp, gx(jb), gy(jb),
     +				  gx(jb), gy(jb), iret )
		    IF ( iret .ne. 0 ) RETURN
C
		    CALL DFRNT ( iwndw, jp, gx(jb), gy(jb), iret )
		    IF ( iret .ne. 0 ) RETURN
C
		END IF
	    END IF
C
C*	    First point of next pass equals last point of this pass.
C
	    ib = ie
	END DO
C*
	RETURN
	END
