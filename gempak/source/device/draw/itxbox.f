	SUBROUTINE ITXBOX ( x, y, ixoff, iyoff, maxlen, nlines,
     +			    rotat, iret )
C************************************************************************
C* ITXBOX								*
C*									*
C* This subroutine optionally plots a border and a background fill for	*
C* a text string with MAXLEN characters by NLINES number of lines. 	*
C*									*
C* ITXBOX ( X, Y, IXOFF, IYOFF, MAXLEN, NLINES, ROTAT, IRET )		*
C*									*
C* Input parameters:							*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	IXOFF		INTEGER		X Offset			*
C*	IYOFF		INTEGER		Y Offset			*
C*	MAXLEN		INTEGER		Max length of a line		*
C*	NLINES		INTEGER		Number of lines of text		*
C*	ROTAT		REAL   		Rotation			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 1/98						*
C* S. Jacobs/NCEP	 2/98	Fixed background fill for underline	*
C* S. Jacobs/NCEP	 4/98	Changed fill to always use solid	*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* T. Lee/SAIC		 8/02	Added overline				*
C* R. Jones/NCEP	 8/06	Added reverse video			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	REAL		xoff(6), yoff(6), xo(6), yo(6)
	INTEGER		jxc(6), jyc(6)
C------------------------------------------------------------------------
	iret   = NORMAL
C
C*	Get the type of bounding box and background fill to plot.
C
	ibr = mbrdr / 100
	ifl = MOD ( mbrdr, 100 ) / 10
	itp = MOD ( mbrdr, 10 )
C
C*	Compute the offsets for the string based on text character
C*	spacing.
C
	IF  ( MOD ( ixoff, 2 ) .ne. 0 ) THEN
	    ixo = ( ixoff - 1 ) / 2 * 7 + 4
	  ELSE
	    ixo = ixoff / 2 * 7
	END IF
	IF  ( MOD ( iyoff, 2 ) .ne. 0 ) THEN
	    iyo = ( iyoff - 1 ) / 2 * 9 + 5
	  ELSE
	    iyo = iyoff / 2 * 9
	END IF
C
C*	Compute offsets for the corners of the bounding box based on
C*	the text character spacing. Add 1/4 of a character around the
C*	text for the box. The point of the Low and High boxes is
C*	1 character away from the text.
C
	extra = 0.25
	point = 1.0
C
C*	Get the corner offsets in terms of number of characters.
C
	IF  ( mjust .eq. 1 )  THEN
	    aleft  = -0.5
	    aright = maxlen - 0.5
	  ELSE IF  ( mjust .eq. 3 )  THEN
	    aleft  = -(maxlen - 0.5)
	    aright = 0.5
	  ELSE
	    aleft  = -(maxlen/2.0)
	    aright =   maxlen/2.0
	END IF
	abot = -(nlines/2.0)
	atop =   nlines/2.0
C
	IF  ( itp .eq. 2 )  THEN
C
C*	    Low pressure box.
C
	    xoff(1) = aleft  - extra
	    yoff(1) = abot   + extra
	    xoff(2) = aleft  - extra
	    yoff(2) = atop   + extra
	    xoff(3) = aright + extra
	    yoff(3) = atop   + extra
	    xoff(4) = aright + extra
	    yoff(4) = abot   + extra
	    xoff(5) = ( aleft + aright ) / 2.0
	    yoff(5) = yoff(1) - point
	    xoff(6) = xoff(1)
	    yoff(6) = yoff(1)
	    np = 6
	  ELSE IF  ( itp .eq. 3 )  THEN
C
C*	    High pressure box.
C
	    xoff(1) = aleft  - extra
	    yoff(1) = atop   - extra
	    xoff(2) = aleft  - extra
	    yoff(2) = abot   - extra
	    xoff(3) = aright + extra
	    yoff(3) = abot   - extra
	    xoff(4) = aright + extra
	    yoff(4) = atop   - extra
	    xoff(5) = ( aleft + aright ) / 2.0
	    yoff(5) = yoff(1) + point
	    xoff(6) = xoff(1)
	    yoff(6) = yoff(1)
	    np = 6
	  ELSE
C
C*	    Regular box.
C
	    xoff(1) = aleft  - extra
	    yoff(1) = abot   - extra
	    xoff(2) = aright + extra
	    yoff(2) = abot   - extra
	    xoff(3) = aright + extra
	    yoff(3) = atop   + extra
	    xoff(4) = aleft  - extra
	    yoff(4) = atop   + extra
	    xoff(5) = xoff(1)
	    yoff(5) = yoff(1)
	    np = 5
	END IF
C
C*	Compute the corner offsets in device coordinates.
C
	DO  i = 1, np
	    xo(i) = xoff(i) * 7
	    yo(i) = yoff(i) * 9
	END DO
C
C*	Compute the bounding box corner locations by applying the
C*	offsets and rotation.
C
	cosrot = COS ( rotat * DTR )
	sinrot = SIN ( rotat * DTR )
	size = txsize * bscalc
	DO  i = 1, np
	    xd     = ( ixo + xo(i) ) * size
	    yd     = ( iyo + yo(i) ) * size
C
	    xprimd = xd * cosrot - yd * sinrot
	    yprimd = xd * sinrot + yd * cosrot
C
	    jxc(i) = x + ispanx * NINT ( xprimd )
	    jyc(i) = y + ispany * NINT ( yprimd )
	END DO
C
C*	Fill in the bounding box with the background color if ifl=2
C*      or switch the text and background colors (reverse video) if ifl=3.
C
	IF  ( ifl .eq. 2 )  THEN
	    mmcolr = mcolr
	    mmfltp = mfltyp
	    CALL DSCOLR ( 101, imclr, ier )
	    CALL DSFILL ( 0, 1, fsiz, jtyp, ier )
	    CALL IFILL  ( np, jxc, jyc, ier )
	    CALL DSCOLR ( mmcolr, imclr, ier )
	    CALL DSFILL ( 0, mmfltp, fsiz, jtyp, ier )
        ELSE IF ( ifl .eq. 3 ) THEN
            mmcolr = mcolr
            mmfltp = mfltyp
            CALL DSCOLR ( mmcolr, imclr, ier )
            CALL DSFILL ( 0, 1, fsiz, jtyp, ier )
            CALL IFILL  ( np, jxc, jyc, ier )
            CALL DSCOLR ( 101, imclr, ier )
            CALL DSFILL ( 0, mmfltp, fsiz, jtyp, ier )
	END IF
C
C*	Draw the bounding box.
C
	ix = 1
	IF  ( ibr .eq. 2 )  THEN
	    IF  ( itp .eq. 5 .or. itp .eq. 6 )  THEN
C
C*		For underline or over line, only draw the first two points.
C
		npl = 2
C
		IF ( itp .eq. 6 ) ix = 3
	      ELSE
C
C*		For all other types, draw all points.
C
		npl = np
	    END IF
	    CALL ILINE  ( npl, jxc (ix), jyc (ix), ier )
	END IF
C*
	RETURN
	END
