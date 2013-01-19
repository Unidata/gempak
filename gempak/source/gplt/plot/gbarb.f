	SUBROUTINE GBARB ( sys, np, x, y, spd, dir, iret )
C************************************************************************
C* GBARB								*
C*									*
C* This subroutine plots wind barbs in any coordinate system.  The	*
C* orientation of each barb is relative to local north.  If the barbs	*
C* are not plotted on a map projection, local north is assumed to be	*
C* vertical.  By convention, each barb points in the direction from	*
C* which the wind is blowing.  The wind barbs are drawn using		*
C* attributes defined by GSBARB.					*
C*									*
C* GBARB ( SYS, NP, X, Y, SPD, DIR, IRET )				*
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
C*	NP		INTEGER		Number of wind barbs		*
C*	X     (NP)	REAL		X coordinates / latitudes	*
C*	Y     (NP)	REAL		Y coordinates / longitudes	*
C*	SPD   (NP)	REAL		Wind speeds			*
C*	DIR   (NP)	REAL		Wind directions			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* I. Graffman/RDS	 2/88	Fixed buffering				*
C* M. desJardins/GSFC	 5/89	Added SYSSAV to save SYSUC for buffers	*
C* S. Schotz/GSC	 1/90	Added barb width and type		*
C* K. Brill/NMC		11/91	Moved SPD and DIR into GSPD and GDIR	*
C* M. desJardins/NMC	 3/92	Move GSPD and GDIR to common		*
C* S. Jacobs/NMC	 8/94	Added flag for S. Hemisphere data	*
C* P. Bruehl/Unidata	 1/95	Fixed S.hemi flagging for missing data	*
C* M. Linda/GSC		 6/96	Bypass buffer /BARBBF/ when NP > 1	*
C* M. Linda/GSC		12/96	Removed /BARBBF/, passing reals on down	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* C. Lin/EAI	 	 6/97	Modified for adding 'S' in sysup	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* I. Durham/GSC	10/98	Changed call to GAZDRM to GP_AZDR	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'GTBUFF.CMN'
C*
	CHARACTER*(*)	sys
	REAL		x (*), y (*), spd (*), dir (*)
C
	CHARACTER	sysuc*1, syssav*1, sysout*1
C
	INCLUDE		'ERMISS.FNC'
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
		im = ib + i - 1
		gspd (i) = spd ( im )
		gdir (i) = dir ( im )
C
		IF  ( ( sysuc .eq. 'M' ) .and.
     +		      ( ddev .ne. 'VG' ) ) THEN
		    CALL GP_AZDR ( gdir(i), gx(i), gy(i), gdir(i), ier )
C
C*		    Flag S. hemisphere data with negative wind speed.
C
		    IF  ( ( gx (i) .lt. 0 ) .and. 
     +			  ( .not. ERMISS ( gspd (i) ) ) )
     +			gspd (i) = - gspd (i)
		END IF
	    END DO
C
C*	    Transform to device coordinates.
C
	    CALL GTRANS ( sysuc, sysout, num, gx, gy, gx, gy, ier )
C
C*	    Send points to device driver.
C
	    CALL DBARB ( iwndw, num, gx, gy, gspd, gdir, iret )
C
	    ib = ie + 1
	END DO
C*
	RETURN
	END
