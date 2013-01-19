	SUBROUTINE GG_RVEC  ( refvec, defstr, iret )
C************************************************************************
C* GG_RVEC								*
C*									*
C* This subroutine decodes the reference arrow parameter in the form:	*
C*	Reference mag ; x- ; y- ; size / font / width / HW ; string  	*
C*									*
C* GG_RVEC  ( REFVEC, DEFSTR, IRET ) 					*
C*									*
C* Input parameters:							*
C*	REFVEC		CHAR*		Reference arrow input		*
C*	DEFSTR		CHAR*		Default label string		*
C*									*
C* Output parameters:							*
C*      IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* L. Sager/NMC	 	 7/93						*
C* S. Jacobs/EAI	10/93	Cleaned up; added rtext return		*
C* S. Jacobs/NMC	 8/94	Created from IN_RVEC			*
C* S. Jacobs/NCEP	 9/97	Changed call to GQTEXT and GSTEXT	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	refvec, defstr
C*
	CHARACTER	rrr*72, rrr2*72, rtext*72, string*72, arolab*24
	REAL		rarr (3)
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse off the the user input string, if present.
C
	CALL ST_NOCC ( refvec, ';', 4, ipos, ier )
	IF  ( ( ipos .ne. 0 ) .and. ( ier .eq. 0 ) )  THEN
	    rrr2   = refvec ( :ipos-1 )
	    string = refvec ( ipos+1: )
	ELSE
	    rrr2   = refvec
	    string = ' '
	END IF
C
C*	Parse off the the text attributes, if present.
C
	CALL ST_NOCC ( refvec, ';', 3, ipos, ier )
	IF  ( ( ipos .ne. 0 ) .and. ( ier .eq. 0 ) )  THEN
	    rrr   = rrr2 ( :ipos-1 )
	    rtext = rrr2 ( ipos+1: )
	ELSE
	    rrr   = rrr2
	    rtext = ' '
	END IF
C
C*	Break the remaining input into elements.
C
	CALL ST_RLST  ( rrr, ';', RMISSD, 3, rarr, num, ier )
C
C* 	Set the magnitude and location.
C
	IF  ( rarr (1) .eq. RMISSD )  THEN
	    rmag = 0.
	ELSE
	    rmag = rarr (1)
	END IF
C*
	IF  ( rarr (2) .eq. RMISSD )  THEN
	    rx = .05
	ELSE
	    rx = rarr (2)
	END IF
C*
	IF  ( rarr (3) .eq. RMISSD )  THEN
	    ry = .05
	ELSE
	    ry = rarr (3)
	END IF
C
C*	Plot reference arrow, if the magnitude is greater than zero.
C
	IF  ( rmag .le. 0. )  RETURN
C
C*	Set the arrow label.
C
	CALL ST_RLCH ( rmag, 0, arolab, ier )
	CALL ST_LSTR ( arolab, ilen, ier )
	IF  ( string .ne. ' ' )  THEN
	    arolab = arolab (:ilen) // ' ' // string
	ELSE
	    arolab = arolab (:ilen) // ' ' // defstr
	END IF
C
C*	Save the current text settings. Set the text attributes for
C*	the label.
C
	CALL GQTEXT ( mtxfn, mtxhw, szmtxt, mtxwid,
     +		      mbrdr, mrrotn, mjust, ier )
	CALL IN_TEXT ( rtext, ier )
C
C*	Set the text location and plot the text.
C
	CALL GQSYSZ ( rwm, rhm, rwc, rhc, rxlb, rylb, ier )
	CALL GQBND ( 'N', xl, yb, xr, yt, ier )
	xrp = xl + rx * ( xr - xl )
	yrp = yb + ry * ( yt - yb )
	x01 = xrp
	yyy = yrp		         
	CALL GTEXT ( 'N', x01, yyy, arolab, 0., 0, 0, ier )
C
C*	Reset the saved text attributes.
C
	CALL GSTEXT ( mtxfn, mtxhw, szmtxt, mtxwid,
     +		      mbrdr, mrrotn, mjust, ier )
C
C*	Set the reference arrow magnitude, direction and plotting
C*	location and draw the arrow.
C
	spd = rmag
	dir = 270.0
	CALL ST_LSTR ( arolab, lenaro, ier )
	offset = FLOAT ( lenaro + 1 )
	x01 = x01 + offset * rwc
	CALL GARRW ( 'N', 1, x01, yyy, spd, dir, iret )
C*
	RETURN
	END
