	SUBROUTINE GG_CBAR ( clrtxt, nflvl, flvl, ifcolr, iret )
C************************************************************************
C* GG_CBAR								*
C*									*
C* This routine will draw a color bar.					*
C*									*
C* GG_CBAR  ( CLRTXT, NFLVL, FLVL, IFCOLR, IRET )			*
C*									*
C* Input parameters:							*
C*	CLRTXT		CHAR*		Color bar, text string input    * 
C*	NFLVL		INTEGER		Number of fill levels		*
C*	FLVL(*)		REAL		Fill levels			*
C*	IFCOLR(*)	INTEGER		Color of fill levels		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 5/92						*
C* P. Bruehl/Unidata	 4/93	Corrected syntax at line 268		*
C* G. Krueger/EAI	 8/93	Modified parameter list for view coord	*
C* S. Jacobs/EAI	 9/93	Clean up; Added call to IN_CBAR		*
C* S. Jacobs/EAI	10/93	Added label decimal place computation	*
C* S. Jacobs/EAI	10/93	Removed check for first and last value	*
C* S. Jacobs/NMC         4/94   Removed unused variables                *
C* D.W.Plummer/NCEP      1/97   Removed call to GFLUSH after GFILL	*
C* A. Hardy/GSC          4/98   Added calls to GQFILL and  GSFILL       *
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* T. Piper/GSC		12/99	Set specific text attributes, save users*
C* S. Jacobs/NCEP	 2/00	Removed commented code; fixed bar size	*
C* T. Piper/GSC		 9/00	Corrected logic for resetting attributes*
C* A. Hardy/GSC		 1/01   Can change color bar label attributes   *
C************************************************************************
	CHARACTER*(*)	clrtxt
	INTEGER		ifcolr (*)
	REAL		flvl (*)
C*
	REAL		size (2), pos (2), xbox (5), ybox (5)
	CHARACTER	orient*1, label*10, clrbar(2)*80
	LOGICAL		cbrflg
C------------------------------------------------------------------------
	iret = 0
C
C*      Split clrtxt string to color bar and text attribute strings.
C
	CALL ST_CLST (clrtxt, '|', ' ', 2, clrbar, num, ier)
C
C*	Check that there is at least one fill level value.
C
	IF  ( nflvl .le. 0 )  RETURN
	nflvl1 = nflvl + 1
C
C*	Parse the color bar input.
C
	CALL IN_CBAR ( clrbar(1), icbar, size, ilblfq, orient, cbrflg,
     +		       ixjust, iyjust, pos, ier )
	IF  ( .not. cbrflg )  RETURN
C
C*      Save the current line attributes.
C
	CALL GQLINE ( jltyp, jlthw, jwidth, jwhw, ier )
	CALL GSLINE ( 1, 0, 1, 0, ier )
C
C*	Save User's Text Attributes
C
	CALL GQTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr, 
     +		      irrotn, ijust, iret )
C
C*      Parse the text_info input. Set missing values to defaults.
C
        CALL IN_TXTN ( clrbar(2), ifont, iswhw, siztxt, 
     +                 itxwd, ibordr, irotat, ijstif, iret )
        if ( ifont  .le. 0 ) ifont = 21
        if ( siztxt .le. 0 ) siztxt = 1.
        if ( iswhw  .le. 0 ) iswhw = 2
        if ( itxwd  .le. 0 ) itxwd = 1
C
        ibordr = 111
        irotat = 1
        ijstif = 1
C
        CALL GSTEXT ( ifont, iswhw, siztxt, itxwd, 
     +                ibordr, irotat, ijstif, ier )
C
C*	Get plot bounds in View coordinates.
C
	CALL GQBND ( 'V', xl, yb, xr, yt, ier )
C
	CALL ST_LCUC ( orient, orient, ier )
C
C*	Determine the corners of the colorbar.
C
	IF ( orient .ne. 'H' ) THEN
	    ihoriz = 2
	    ivert = 1
	ELSE
	    ihoriz = 1
	    ivert = 2
	ENDIF
	IF ( ixjust .eq. 3 ) THEN
	    xll = pos (1) - size (ihoriz)
	    xur = pos (1)
	ELSE IF ( ixjust .eq. 2 ) THEN
	    xll = pos (1) - size (ihoriz) / 2
	    xur = pos (1) + size (ihoriz) / 2
	ELSE
	    xll = pos (1)
	    xur = pos (1) + size (ihoriz)
	ENDIF
	IF ( iyjust .eq. 3 ) THEN
	    yll = pos (2) - size (ivert)
	    yur = pos (2)
	ELSE IF ( iyjust .eq. 2 ) THEN
	    yll = pos (2) - size (ivert) / 2
	    yur = pos (2) + size (ivert) / 2
	ELSE
	    yll = pos (2)
	    yur = pos (2) + size (ivert)
	ENDIF
	xll = xll * ( xr - xl ) + xl
	xur = xur * ( xr - xl ) + xl
	yll = yll * ( yt - yb ) + yb
	yur = yur * ( yt - yb ) + yb
C
C*	Compute the number of decimal places to display.
C
	CALL GR_NDCP ( nflvl1, flvl, ndecp, ier )
C
C*	Set the fill attributes.
C
	CALL GQFILL ( szfil, ifltyp, ier )
	CALL GSFILL ( 1.0, 1, ier )
C
C*	Draw the color bar
C
	knt1 = 0
	DO  i = 1, nflvl1
	    knt1 = knt1 + 1
	    IF  ( orient .ne. 'H' )  THEN
C
C*		Set up for the vertically oriented color bar
C
		diff = ( yur - yll ) / nflvl1
		xbox(1) = xll 
		ybox(1) = yll + (knt1-1) * diff
		xbox(2) = xur
		ybox(2) = yll + (knt1-1) * diff
		xbox(3) = xur 
		ybox(3) = yll + knt1 * diff
		xbox(4) = xll 
		ybox(4) = yll + knt1 * diff
	      ELSE
C
C*		Set up for the horizontally oriented color bar
C
		diff = ( xur - xll ) / nflvl1
		xbox(1) = xll + (knt1-1) * diff 
		ybox(1) = yll 
		xbox(2) = xll + knt1 * diff
		ybox(2) = yll
		xbox(3) = xll + knt1 * diff 
		ybox(3) = yur 
		xbox(4) = xll + (knt1-1) * diff
		ybox(4) = yur
	    END IF
C
C*	    Set Attributes.
C
	    IF  ( ifcolr(i) .ne. 0 )  THEN
		CALL GSCOLR ( ifcolr(i), ier )
		CALL GFILL ( 'N', 4, xbox, ybox, ier )
	    END IF
C
C*	    Draw box around fill if requested.
C
	    IF ( icbar .ge. 0 ) THEN
		CALL GSCOLR ( icbar, ier )
		xbox(5) = xbox(1)
		ybox(5) = ybox(1)
		CALL GLINE ( 'N', 5, xbox, ybox, ier )
	    END IF
C
C*	    Plot label.
C
	    IF ( ilblfq .ne. 0 ) THEN
		IF ( ( MOD ((i+(ABS(ilblfq)-1)), ilblfq ) .eq. 0 ) .and. 
     +		     ( i .ne. nflvl1 ) ) THEN
C
C*		    Encode value into a character, checking to see
C*		    how many decimal places to use.
C
		    IF  ( ndecp .eq. 0 )  THEN
			iform = 1
		    ELSE IF  ( ndecp .gt. 0 )  THEN
			iform = 2
		    ELSE
			iform = 3
		    END IF
		    CALL GR_LABL ( flvl(i), iform, ndecp, label,
     +				   ixoff, ier )
C*
		    CALL GSCOLR ( icbar, ier )
		    IF  ( orient .ne. 'H' )  THEN
			iyoff = 0
			IF ( ilblfq .ge. 0 ) THEN
			    ixoff = 1
			    xlabl = xbox(3)
			    ylabl = ybox(3)
			ELSE
			    ixoff = (-2) * ixoff
			    xlabl = xbox(4)
			    ylabl = ybox(4)
			END IF
		    ELSE
			ixoff = ixoff * (-1)
			IF ( ilblfq .lt. 0 ) THEN
			    iyoff = -2
			    xlabl = xbox(2)
			    ylabl = ybox(2)
			ELSE
			    iyoff = 2
			    xlabl = xbox(3)
			    ylabl = ybox(3)
			END IF
		    END IF
		    CALL GTEXT ( 'N', xlabl, ylabl, label, 0.,
     +				 ixoff, iyoff, ier )
		END IF
	    END IF
	END DO
C
C*	Reset User's Fill, Line, & Text Attributes
C
	CALL GSFILL ( szfil, ifltyp, ier )
	CALL GSLINE ( jltyp, jlthw, jwidth, jwhw, ier )
	CALL GSTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr, 
     +		      irrotn, ijust, iret )
C*
	RETURN
	END
