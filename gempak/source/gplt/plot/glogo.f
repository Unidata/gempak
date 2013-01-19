	SUBROUTINE GLOGO ( sys, x, y, size, iclmod, ilogo, iret )
C************************************************************************
C* GLOGO								*
C*									*
C* This subroutine draws a specified emblem in any coordinate system    *
C* such as the NOAA seagull emblem.					*
C*									*
C* GLOGO ( SYS, X, Y, SIZE, ICLMOD, ILOGO, IRET )			*
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
C*	X		REAL		X coordinate or latitude	*
C*	Y		REAL		Y coordinate or longitude	*
C*	SIZE		REAL		Emblem size			*
C*	ICLMOD		INTEGER		Emblem color mode		*
C*					  '1' = monochrome		*
C*					  '2' = color			*
C*	ILOGO		INTEGER		Emblem ID			*
C*					  '1' = NOAA			*
C*					  '2' = NWS			*
C*					  '3' = NOAA w/o text		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 7/97	Original				*
C* A. Hardy/GSC          9/98   Added check for RBK driver              *
C* A. Hardy/GSC          5/00	Added emblem color mode			*
C* J. Wu/GSC		 3/01   Added emblem ID 			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	CHARACTER*(*)	sys
C
	CHARACTER	sysuc*1, sysout*1
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check that device has been set.
C
	IF  ( ddev .eq. ' ' ) THEN
	    iret = NDVICE
	    RETURN
	END IF
C
	IF ( ( ddev .eq. 'UTF' ) .or. ( ddev .eq. 'RBK' ) ) RETURN
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
C*	Translate center location to device coordinates.
C
	IF  ( ddev .eq. 'VG' ) THEN
	    sysout = 'M'
	  ELSE
	    sysout = 'D'
	END IF
C
	CALL GTRANS ( sysuc, sysout, 1, x, y, xd, yd, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Check for bad center location (i.e., missing).
C
	IF ( ( ERMISS ( xd ) ) .or. ( ERMISS ( yd ) ) ) RETURN
C
C*	Draw the emblem.
C
	CALL DLOGO ( iwndw, xd, yd, size, iclmod, ilogo, iret )
C*
	RETURN
	END
