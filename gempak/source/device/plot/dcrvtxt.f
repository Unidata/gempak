	SUBROUTINE DCRVTXT ( iwndw, x, y, rad, cangle, nlett, idir,
     +			     string, iret )
C************************************************************************
C* DCRVTXT								*
C*									*
C* This subroutine plots text in an arc.  The center point (X, Y)	*
C* specifies the center of a circle of radius RAD.  Angle CANGLE	*
C* corresponds with the center of the text on the circle.  Parameter	*
C* NLETT controls spacing between letters of the string.		*
C*									*
C* DCRVTXT ( IWNDW, X, Y, RAD, CANGLE, NLETT, IDIR, STRING, IRET )	*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	RAD		REAL		Radius in device units		*
C*	CANGLE		REAL		Angle for string's center	*
C*	NLETT		INTEGER		Number of letters per 360 deg.	*
C*	IDIR		INTEGER		Text orientation		*
C*					  0 = right-side-up		*
C*					  1 = up-side-down		*
C*	STRING		CHAR*		String of text to plot		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 7/97	Original				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
C*
	CHARACTER*(*)	string
C
	CHARACTER	c1*1
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Convert the angle to a range between -180 and +180 degrees.
C
	cang = cangle
	CALL PRNLON ( 1, cang, irt )
C
C*	Calculate separation, orentation, and direction of characters.
C
	ainc = 360. / FLOAT ( nlett )
	aflp = 90.0
	IF  ( ( ( cang .ge. 0. ) .and. ( idir .ne. 0 ) ) .or.
     +	      ( ( cang .lt. 0. ) .and. ( idir .eq. 0 ) ) ) THEN
	    ainc = -ainc
	    aflp = -aflp
	END IF
C
C*	Determine the angle where the string begins.
C
	CALL ST_LSTR ( string, nt, iret )
	a0 = cang + ainc * FLOAT ( nt - 1 ) / 2.
C
C*	Plot the string, one character at a time.
C
	rx = rad * FLOAT ( ispanx )
	ry = rad * FLOAT ( ispany )
C
	DO  i = 1, nt
	    c1 = string ( i : i )
	    ag = a0 - ainc * FLOAT ( i - 1 )
	    ar = ag * DTR
	    xr = x + rx * COS ( ar )
	    yr = y + ry * SIN ( ar )
	    CALL DTEXT ( iwndw, xr, yr, c1, 1, ag - aflp, 0, 0, iret )
	END DO
C*
	RETURN
	END
