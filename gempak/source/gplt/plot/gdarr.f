	SUBROUTINE GDARR ( sys, np, x, y, dir, iret )
C************************************************************************
C* GDARR								*
C*									*
C* This subroutine plots directional arrows in any coordinate system.	*
C* The orientation is relative to local north.  If the arrows		*
C* are not plotted on a map projection, local north is assumed to be	*
C* vertical.  By convention, each arrow points in the direction from	*
C* which the wind is blowing.  The directional arrows are drawn using	*
C* attributes defined by GSDARR.					*
C*									*
C* GDARR ( SYS, NP, X, Y, DIR, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*					  'S' = screen coordinates	*
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	NP		INTEGER		Number of wind arrows		*
C*	X     (NP)	REAL		X coordinates / latitudes	*
C*	Y     (NP)	REAL		Y coordinates / longitudes	*
C*	DIR   (NP)	REAL		Wind directions			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C* I. Durham/GSC	10/98	Changed GAZDRM to GP_AZDR		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'GTBUFF.CMN'
C*
	CHARACTER*(*)	sys
	REAL		x (*), y (*), dir (*)
C
	CHARACTER	sysuc*1, syssav*1, sysout*1
C
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check for zero points.
C
	IF ( np .le. 0 ) RETURN
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
C*	Reset color if necessary.
C
	IF ( lcolr .ne. mcolr ) CALL DSCOLR ( lcolr, jcm, ier )
C
C*      Set the output coordinates based on the device driver.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    sysout = 'M'
	  ELSE
	    sysout = 'D'
	END IF
C
C*	Loop when points will not fit in one buffer.
C
	npass = ( ( np - 1 ) / LLMXPT ) + 1
	ib = 1
	syssav = sysuc
	DO  m = 1, npass
	    ie = ib + LLMXPT - 1
	    IF ( ie .gt. np ) ie = np
	    num = ie - ib + 1
C
C*	    If input is in G coordinates, transform to M coordinates.
C
	    sysuc = syssav
	    IF  ( sysuc .eq. 'G' ) THEN
		CALL GTRANS ('G','M', num, x (ib), y (ib), gx, gy, ier )
		sysuc = 'M'
	    ELSE
		DO  i = 1, num
		    ik = ib + i - 1
		    gx (i) = x (ik)
		    gy (i) = y (ik)
		END DO
	    END IF
C
C*	    Transform direction for map or grid coordinates.
C
	    DO  i = 1, num
		gdir (i) = dir ( ib + i - 1 )
		IF  ( ( sysuc .eq. 'M' ) .and.
     +		      ( ddev .ne. 'VG' ) ) THEN
		    CALL GP_AZDR ( gdir(i), gx(i), gy(i), gdir(i), ier )
		END IF
	    END DO
C
C*	    Transform to device coordinates.
C
	    CALL GTRANS ( sysuc, sysout, num, gx, gy, gx, gy, ier )
C
C*	    Send points to device driver.
C
	    CALL DDARR ( iwndw, num, gx, gy, gdir, iret )
C
	    ib = ie + 1
	END DO
C*
	RETURN
	END
