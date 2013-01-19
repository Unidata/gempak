	SUBROUTINE GTEXT ( sys, x, y, cchar, rotat, ixoff, iyoff, iret )
C************************************************************************
C* GTEXT								*
C*									*
C* This subroutine plots a text string in any coordinate system.  Any	*
C* text overflowing into the margins is clipped.  The reference point	*
C* (X,Y) determines the center of the first character.  The text	*
C* string may be rotated from horizontal at the reference point and	*
C* offset along the rotated X and Y coordinates.  Positive X offsets	*
C* are toward the right;  positive Y offsets are toward the top.  The	*
C* text is drawn using attributes defined by GSTEXT.			*
C*                                                                      *
C* The text string may contain carriage returns and/or line feed        *
C* characters.  A carriage return/line feed will terminate one line of  *
C* text and begin a new line.  Each new line will be offset normal to   *
C* the rotation from the previous line and oriented along the direction *
C* of rotation.  All resulting lines of text will be placed so that     *
C* they are centered normal to the rotation.                            *
C*									*
C* GTEXT ( SYS, X, Y, CCHAR, ROTAT, IXOFF, IYOFF, IRET )		*
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
C*	X		REAL		X coordinate / latitude		*
C*	Y		REAL		Y coordinate / longitude	*
C*	CCHAR		CHAR*		Text string to plot		*
C*	ROTAT		REAL		Rotation angle in degrees	*
C*	IXOFF		INTEGER		X offset in half characters	*
C*	IYOFF		INTEGER		Y offset in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 1/88	Fixed missing data problem		*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	10/88	Corrected error in trans D coords	*
C* S. Schotz/GSC	 1/90	Added text width			*
C* M. desJardins/NMC	 3/92	len --> lent				*
C* M. Linda/GSC		 1/97	Removed buffer /TEXTBF/, X & Y to reals	*
C* S. Jacobs/NCEP	 2/97	Added DEVACT.CMN			*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* C. Lin/EAI	 	 6/97	Modified for adding 'S' in sysup	*
C* S. Jacobs/NCEP	 7/97	Updated documentation			*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* S. Jacobs/NCEP	 9/97	Added check for N-relative rotation	*
C* S. Jacobs/NCEP	 7/98	Redefined N-relative angles		*
C* S. Jacobs/NCEP	 7/98	Added Grid coord to N-rel check		*
C* I. Durham/GSC	10/98	Changed GAZDRM to GP_AZDR		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	CHARACTER*(*)	cchar, sys
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
C*	Translate to device coordinates.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    sysout = 'M'
	  ELSE
	    sysout = 'D'
	END IF
C
C*	Check for North-relative rotation.
C
	IF  ( ( mrrotn .eq. 2 )  .and.
     +	      ( ( sysuc .eq. 'M' ) .or. ( sysuc .eq. 'G' ) ) .and.
     +	      ( ddev .ne. 'VG' ) )  THEN
C
C*	    First convert from Grid to Map coordinates, if needed.
C
	    IF  ( sysuc .eq. 'G' )  THEN
		CALL GTRANS ( sysuc, 'M', 1, x, y, xout, yout, ier )
		sysuc = 'M'
	      ELSE
		xout = x
		yout = y
	    END IF
C
C*	    Apply the North relative axis rotation.
C
	    CALL GP_AZDR ( rotat, xout, yout, rangl, ier )
C
C*	    Compute the angle as if it were for Screen relative text.
C
	    rotn = rotat + ( rotat - rangl )
	  ELSE
	    rotn = rotat
	    xout = x
	    yout = y
	END IF
C
C*	Transform to the output coordinate system.
C
	CALL GTRANS ( sysuc, sysout, 1, xout, yout, xp, yp, ier )
C
C*	Check for missing location.
C
	IF ( ( ERMISS ( xp ) ) .or. ( ERMISS ( yp ) ) ) RETURN
C
C*	Reset color if necessary.
C
	IF ( lcolr .ne. mcolr ) CALL DSCOLR ( lcolr, jcm, ier )
C
C*	Send the text string to the device driver.
C
	CALL ST_LSTR ( cchar, lenc, ier )
	CALL DTEXT   ( iwndw, xp, yp, cchar, lenc, rotn,
     +		       ixoff, iyoff, iret )
C*
	RETURN
	END
