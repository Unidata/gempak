	SUBROUTINE DHASH ( iwndw, np, x, y, dir, iret )
C************************************************************************
C* DHASH								*
C*									*
C* This subroutine draws hash marks at points defined in any            *
C* coordinate system. The orientation is orthogonal relative to the     *
C* front line.  The hash marks will be drawn using attributes defined   *
C* by GSHASH.								*
C*									*
C* DHASH ( IWNDW, NP, X, Y, DIR, IRET )					*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of hash marks		*
C*	X   (NP)	REAL		X coordinates in device units	*
C*	Y   (NP)	REAL		Y coordinates in device units	*
C*	DIR (NP)	REAL		Hash mark directions		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C* I. Durham/GSC	 4/98	Made changes to allow VGF driver ops	*
C* S. Jacobs/NCEP	 4/98	Renamed line width var for resetting	*
C* A. Hardy/GSC		10/00   switched mhwid w/ mlwidh DSLINE&scaling *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	REAL		x (*), y (*), dir (*)
	REAL		rx (4), ry (4)
C*
	INTEGER		ix (2), iy (2), jx (2), jy (2)
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
C*      If the driver is VG, send the points directly to the device
C*      for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HHASH ( np, x, y, dir, iret )
	    RETURN
	END IF
C
C*	Set clipping window
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Save line attributes
C
	jltyp = mltyp
	jlwid = mlwid
C
	CALL DSLINE ( 1, 0, mhwid, 0, ilt, ilht, ilw, ilhw, ier )
C
C*	Loop through hash marks
C
	DO i = 1, np
C
C*	   Set scaling factors
C
	   tshszs = thshsz * bscals
	   mhwids = mlwidh * bscali
C
C*	   Compute major angles
C
	   rbeta = ATAN ( mhwids / tshszs )
C
	   rhypt = ( ( mhwids / 2.0 ) / ( SIN ( rbeta ) ) )
C
	   rang1 = ( dir ( i ) * dtr ) + rbeta
	   rang2 = ( dir ( i ) * dtr ) - rbeta
C
C*	   Compute coordinates of mark endpoints
C
	   rx ( 1 ) = x ( i ) - ( ( COS ( rang1 ) ) * rhypt ) * ispanx
	   ry ( 1 ) = y ( i ) + ( ( SIN ( rang1 ) ) * rhypt ) * ispany
C
	   rx ( 2 ) = x ( i ) + ( ( COS ( rang1 ) ) * rhypt ) * ispanx
	   ry ( 2 ) = y ( i ) - ( ( SIN ( rang1 ) ) * rhypt ) * ispany
C
	   rx ( 3 ) = x ( i ) - ( ( COS ( rang2 ) ) * rhypt ) * ispanx
	   ry ( 3 ) = y ( i ) + ( ( SIN ( rang2 ) ) * rhypt ) * ispany
C
	   rx ( 4 ) = x ( i ) + ( ( COS ( rang2 ) ) * rhypt ) * ispanx
	   ry ( 4 ) = y ( i ) - ( ( SIN ( rang2 ) ) * rhypt ) * ispany
C
C*	   Convert real-valued points to integers by rounding
C
	   ix ( 1 ) = NINT ( rx ( 1 ) )
	   iy ( 1 ) = NINT ( ry ( 1 ) )
C
	   ix ( 2 ) = NINT ( rx ( 4 ) )
	   iy ( 2 ) = NINT ( ry ( 4 ) )
C
	   jx ( 1 ) = NINT ( rx ( 3 ) )
	   jy ( 1 ) = NINT ( ry ( 3 ) )
C
	   jx ( 2 ) = NINT ( rx ( 2 ) )
	   jy ( 2 ) = NINT ( ry ( 2 ) )
C
C*	   Draw hash marks using endpoints
C
	   CALL ILINE ( 2, ix, iy, ier )
	   CALL ILINE ( 2, jx, jy, ier )
C
	END DO
C
	CALL DSLINE ( jltyp, 0, jlwid, 0, ilt, ilht, ilw, ilhw, ier )
C
	RETURN
	END
