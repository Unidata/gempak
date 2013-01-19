	SUBROUTINE GTXSY ( sys, itype, isym, ijust, ixoff, iyoff,
     +			   rotn, x, y, cchar, iret )
C************************************************************************
C* GTXSY								*
C*									*
C* This subroutine plots a special text and symbol to any coordinate    *
C* system.  The special text is centered on the given reference point   *
C* (X,Y).  The text is drawn using attributes defined by GSTEXT, and    *
C* the surrounding box is drawn with attributes defined by GSLINE.      *
C* Depending upon special text type, the box may be filled.             *
C*                                                                      *
C* GTXSY ( SYS, ITYPE, ISYM, IJUST, IXOFF, IYOFF, ROTN, X, Y,           *
C*	   CCHAR, IRET )						*
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
C*	ITYPE		INTEGER		Special Text Type		*
C*					    1 = right justified		*
C*                                          2 = high pressure box       *
C*                                          3 = pressure box            *
C*                                                  bounded, unfilled   *
C*                                          4 = pressure box            *
C*                                                  bounded, filled     *
C*                                          5 = pressure box,           *
C*                                                  unbounded, filled   *
C*                                          6 = freezing level symbol   *
C*					    7 = turbulence symbol	*
C*					    8 = cloud level		*
C*					    9 = high level turbulence	*
C*					   10 = underline		*
C*					   11 = underline, fill box	*
C*					   12 = midlevel icing		*
C*					   13 = overline		*
C*					   14 = overline, fill box	*
C*					   15 = "Big Box" for mid-level	*
C*	ISYM		INTEGER		Symbol type if itype of 7	*
C*					    one or two digits in range  *
C*					     0-8. 			*
C*	IJUST		INTEGER		Justification (-1, 0, 1) 	*
C*	IXOFF		INTEGER		X Offset			*
C*	IYOFF		INTEGER		Y Offset			*
C*	ROTN		REAL		Rotation			*
C*	X		REAL		X coordinate / latitude		*
C*	Y		REAL		Y coordinate / longitude	*
C*	CCHAR		CHAR*		Text string to plot		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	 4/97	Initial coding -- copied from GTEXTC    *
C* E. Safford/GSC        5/97   Changed sym to isym                     *
C* C. Lin/EAI	 	 6/97	Modified for adding 'S' to sysup    	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* E. Safford/GSC        7/97   Returned isym as parameter              *
C* E. Safford/GSC        7/97   Added ijust, irotn, ioff		*
C* S. Jacobs/NCEP	 7/98	Added check for N-relative rotation	*
C* S. Jacobs/NCEP	 7/98	Redefined N-relative angles		*
C* S. Jacobs/NCEP	 7/98	Added Grid coord to N-rel check		*
C* S. Jacobs/NCEP	 9/98	Fixed rotation variable name typo	*
C* I. Durham/GSC	10/98	Changed GAZDRM to GP_AZDR		*
C* M. Li/SAIC		11/01	Added type 12 for midlevel icing	*
C* T. Lee/SAIC		 8/02	Added text to prolog			*
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
	    CALL GP_AZDR ( rotn, xout, yout, rangl, ier )
C
C*	    Compute the angle as if it were for Screen relative text.
C
	    rotat = rotn + ( rotn - rangl )
	  ELSE
	    rotat = rotn
	    xout  = x
	    yout  = y
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
	CALL DTXSY  ( iwndw, itype, isym, ijust, ixoff, iyoff, rotat,
     +		      xp, yp, cchar, lenc, iret )
C*
	RETURN
	END
