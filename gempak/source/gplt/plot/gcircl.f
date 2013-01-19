	SUBROUTINE GCIRCL  ( sys, xpt, ypt, xcrm, ycrm, np, iret )
C************************************************************************
C* GCIRCL								*
C*									*
C* This subroutine draws a circle centered at a point which may	be      *
C* defined in any coordinate system.  The radius of the circle is       *
C* defined by the circumference point xcrm and ycrm. where the circle   *
C* passes through this point.  NP is the number of points to be used    *
C* in drawing the circle.  If NP is zero, 10 points will be used.	*
C*									*
C* GCIRCL  ( SYS, XPT, YPT, XCRM, YCRM, NP, IRET )				*
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
C*	XPT		REAL		X coordinate / latitude		*
C*	YPT		REAL		Y coordinate / longitude	*
C*	XCRM		REAL		X radius coordinate / latitude	*
C*	YCRM		REAL		Y radius coordinate / latitude	*
C* 	NP		INTEGER		Number of points on circle	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/86						*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* S. Jacobs/NCEP	11/96	Added color reset			*
C* S. Jacobs/NCEP	 2/97	Added DEVACT.CMN			*
C* C. Lin/EAI	 	 6/97	Modified for adding 'S' in sysup	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* A. Hardy/GSC         11/98   Changed GCIRCL call, deleted norm coord *
C* G. Krueger/EAI	 6/99	Allow circle wrap on edge of world map	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*)	sys
C*
	CHARACTER	sysuc*1, sysout*1
        REAL		xi(2), yi(2), xd(2), yd(2), xlin(2), ylin(2),
     +			xsave(2), ysave(2)
C------------------------------------------------------------------------
C*	Check that a device has been set.
C
	IF  ( ddev .ne. ' ' )  THEN
	    iret = NORMAL
	  ELSE
	    iret = NDVICE
	    RETURN
	END IF
C
C*	Find coordinate system for clipping.
C
	CALL ST_LCUC  ( sys, sysuc, ier)
	isys = INDEX  ( sysup, sysuc )
	IF  ( isys .eq. 0 )  THEN
	    iret = NOCORD
	    RETURN
	  ELSE
	    iwndw = isys - 2
	    IF ( iwndw .gt. 3) iwndw = 3
	    IF ( iwndw .le. 0) iwndw = 1
	END IF
C
C*	Reset color if necessary.
C
	IF ( lcolr .ne. mcolr ) CALL DSCOLR ( lcolr, jcm, ier )
C
C*	Prepare to transform center coordinates to device coordinates.
C
	iret = 0
	xi(1) = xpt
	yi(1) = ypt
	xi(2) = xcrm
	yi(2) = ycrm
C
C*	Checking for VG driver.
C
        IF ( ddev .eq. 'VG' ) THEN
            sysout = 'M'
          ELSE
            sysout = 'D'
        END IF 
C
	IF ( ddev .eq. 'VG' .or.
     +	     ( isys .lt. 6 ) .or. ( igmode .ne. 1 ) ) THEN
	    CALL GTRANS  ( sys, sysout, 2, xi, yi, xd, yd, ier )
	  ELSE
	    CALL GTRANS  ( sys, 'L', 2, xi, yi, xlin, ylin, ier )
C
	    xdis = ABS ( xlin ( 1 ) - xlin ( 2 ) )
	    ydis = ABS ( ylin ( 1 ) - ylin ( 2 ) )
C
	    IF  ( ( xdis .gt. ueps ) .or. ( ydis .gt. veps ) )  THEN
		xsave (1) = xlin (1)
		xsave (2) = xlin (2)
		ysave (1) = ylin (1)
		ysave (2) = ylin (2)
C
		xspn = ABS ( xbndrl - xbndll )
		yspn = ABS ( ybndbl - ybndtl )
		xdel = ABS ( xspn - xdis )
		ydel = ABS ( yspn - ydis )
C
		xl = ABS ( xlin ( 1 ) - xbndll )
		xr = ABS ( xlin ( 1 ) - xbndrl )
		yt = ABS ( ylin ( 1 ) - ybndtl )
		yb = ABS ( ylin ( 1 ) - ybndbl )
C
		IF ( xdis .gt. ueps ) THEN
		    IF ( xl .lt. xr ) THEN
			xlin (2) = xlin (1) - xdel
		    ELSE
			xlin (2) = xlin (1) + xdel
		    END IF
		END IF
C
		IF ( ydis .gt. veps ) THEN
		    IF ( yt .lt. yb ) THEN
			ylin (2) = ylin (1) + ydel
		    ELSE
			ylin (2) = ylin (1) - ydel
		    END IF
		END IF
		CALL GTRANS  ( 'L', sysout, 2, xlin, ylin, xd, yd, ier )
		CALL DCIRCL  ( iwndw, xd(1), yd(1), xd(2), yd(2), np, ier )
C
		xlin (1) = xsave (1)
		xlin (2) = xsave (2)
		ylin (1) = ysave (1)
		ylin (2) = ysave (2)
C
		xl = ABS ( xlin (2) - xbndll )
		xr = ABS ( xlin (2) - xbndrl )
		yt = ABS ( ylin (2) - ybndtl )
		yb = ABS ( ylin (2) - ybndbl )
C
		IF ( xdis .gt. ueps ) THEN
		    IF ( xl .lt. xr ) THEN
			xlin (1) = xlin (2) - xdel
		    ELSE
			xlin (1) = xlin (2) + xdel
		    END IF
		END IF
C
		IF ( ydis .gt. veps ) THEN
		    IF ( yt .lt. yb ) THEN
			ylin (1) = ylin (2) + ydel
		    ELSE
			ylin (1) = ylin (2) - ydel
		    END IF
		END IF
	    END IF
	    CALL GTRANS  ( 'L', sysout, 2, xlin, ylin, xd, yd, ier )
	END IF
C
C*	Plot the circle.
C
	CALL DCIRCL  ( iwndw, xd(1), yd(1), xd(2), yd(2), np, ier )
C*
	RETURN
	END
