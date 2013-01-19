	SUBROUTINE GFILL ( sys, np, x, y, iret )
C************************************************************************
C* GFILL								*
C*									*
C* This subroutine draws a filled polygon in any coordinate system.	*
C* NP is the number of points that define the polygon.  The polygon	*
C* is drawn using color defined by GSCOLR.				*
C*									*
C* GFILL ( SYS, NP, X, Y, IRET )					*
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
C* J. Whistler/SSAI	10/91	Adapted from GLINE			*
C* M. desJardins/NMC	11/91	Check for 55 pts; check for missing dta	*
C* G. Krueger/EAI	 5/95	npt changed to np on buffer limit check	*
C* G. Krueger/EAI	 5/95	Added GFDUP call			*
C* M. Linda/GSC		 2/97	Removed /FILLBF/, passing reals on down	*
C* C. Lin/EAI		 6/97	Modified for adding 'S' in sysup	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* S. Jacobs/NCEP	 7/97	Added check for VG driver		*
C* D.W.Plummer/NCEP	 1/04	Changes for cylindrical projections	*
C* S.Gilbert/NCEP	 8/06	Changed to do work in "U" coordinates   *
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
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
	INTEGER		poly, roll
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check for at least three points.
C
	IF ( np .lt. 3 ) RETURN
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
C*	Reset color if necessary.
C
	IF ( lcolr .ne. mcolr ) CALL DSCOLR ( lcolr, jcl, ier )
C
C*	Check NP > buffer; all points must go to driver as single buffer.
C
	IF  ( np .gt. LLMXPT ) THEN
	    iret = NNPNTS
	    RETURN
	END IF
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL GTRANS ( sysuc, 'M', np, x, y, gx, gy, iret )
	    IF  ( iret .ne. 0 )  RETURN
	  ELSE
	    IF  ( ( isys .lt. 6 ) .or. ( igmode .ne. 1 ) ) THEN
C
C*	    	Process the D, N, V, or P coordinates or non-map mode.
C
		CALL GTRANS ( sysuc, 'D', np, x, y, gx, gy, iret )
C
	      ELSE
C
C*	    	Process the M or G coordinates and map mode.
C
		poly = 1
		CALL CGR_RANGE ( sysucu, np, x, y, poly,
     &			sys_U, roll, nout, gx, gy,
     &			xll, yll, xur, yur, iret )
		IF ( iret .ne. 0 ) RETURN
C
C*		Abandon polygon with any missing points.
C
		DO  i = 1, nout
	    	  IF  ( ERMISS ( gx ( i ) ) .or. 
     &			ERMISS ( gy ( i ) ) ) RETURN
		END DO
C
		IF ( roll .ne. 0 .or. nout .ne. np )  THEN
C
		  DO  ir = 0, roll
C
		    CALL GTRANS ( 'U', 'D', nout, gx, gy, fx, fy, iret )
		    IF ( iret .ne. 0 ) RETURN
C
		    CALL DFILL ( iwndw, nout, fx, fy, iret)
		    IF ( iret .ne. 0 ) RETURN
C
		    DO  ii = 1, nout
		      gx ( ii ) = gx ( ii ) - TWOPI
		    END DO
C
		  END DO
C
		  RETURN
C
		ELSE
C
		  CALL GTRANS ( 'U', 'D', np, gx, gy, gx, gy, iret )
C
		END IF
C
	    END IF
	END IF
C
	IF ( iret .ne. 0 ) RETURN
C
C*	Abandon polygon with any missing points.
C
	DO  i = 1, np
	    IF  ( ERMISS ( gx ( i ) ) .or. ERMISS ( gy ( i ) ) ) RETURN
	END DO
C
C*	Send polygon to the driver.
C
	CALL DFILL ( iwndw, np, gx, gy, iret)
C*
	RETURN
	END
