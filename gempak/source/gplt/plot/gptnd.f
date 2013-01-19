	SUBROUTINE GPTND ( sys, np, ptcod, x, y, ixoff, iyoff, iret )
C************************************************************************
C* GPTND								*
C*									*
C* This subroutine plots pressure tendency symbols in any coordinate	*
C* system.  The symbols are drawn using attributes defined by GSPTND.	*
C*									*
C* GPTND ( SYS, NP, PTCOD, X, Y, IXOFF, IYOFF, IRET )			*
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
C*	NP		INTEGER		Number of symbols		*
C*	PTCOD (NP)	REAL		Pressure tendency symbol codes	*
C*	X     (NP)	REAL		X coordinates / latitudes	*
C*	Y     (NP)	REAL		Y coordinates / longitudes	*
C*	IXOFF (NP)	INTEGER		X offsets in half characters	*
C*	IYOFF (NP)	INTEGER		Y offsets in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90	GEMPLT Version 5.0			*
C* M. desJardins/NMC	 3/92	Fix bug in buffer flush			*
C* M. Linda/GSC		 6/96	Bypass buffer /PTNDBF/ when NP > 1	*
C* M. Linda/GSC		12/96	Removed /PTNDBF/, passing reals on down	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* C. Lin/EAI	 	 6/97	Modified for adding 'S' in sysup	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'GTBUFF.CMN'
C*
	CHARACTER*(*)	sys
	REAL		ptcod (*), x (*), y (*)
	INTEGER		ixoff (*), iyoff (*)
C
	CHARACTER	sysuc*1, sysout*1
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
C*	Set the output coordinates based on the device driver.
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
	DO  m = 1, npass
	    ie = ib + LLMXPT - 1
	    IF ( ie .gt. np ) ie = np
	    num = ie - ib + 1
C
C*	    Transform to device coordinates.
C
	    CALL GTRANS ( sysuc, sysout, num, x (ib), y (ib),
     +			  gx, gy, ier )
C
C*	    Send symbols to device driver.
C
	    CALL DPTND ( iwndw, num, ptcod (ib), gx, gy, ixoff (ib),
     +			 iyoff (ib), iret )
	    ib = ie + 1
	END DO
C*
	RETURN
	END
