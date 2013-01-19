	SUBROUTINE IM_CBAR ( clrbar, iret )
C************************************************************************
C* IM_CBAR2								* 
C*	- RADMAP wants labels at start of box - not centered		*
C*									*
C* This routine will draw a color bar for imagery.			*
C*									*
C* IM_CBAR  ( CLRBAR, IRET )						*
C*									*
C* Input parameters:							*
C*	CLRBAR		CHAR*		Color bar input			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95	Copied from GG_CBAR()			*
C* J. Cowie/COMET	 7/96	Updated computation and drawing		*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* A. Hardy/GSC          4/98   Added calls to GQFILL and GSFILL        *
C* S. Jacobs/NCEP	 2/99	Moved calls to GQLINE and GSLINE	*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* S. Jacobs/NCEP	 5/99	Moved the labels to center of color box	*
C* T. Piper/GSC		 9/00	Fixed horizontal bar labeling bug	*
C* T. Piper/GSC		 7/01	Fixed typo of variable sizfil		*
C* M. Li/SAIC		11/03 	Changed calling sequence		*
C************************************************************************
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	clrbar
	INTEGER		idcols (256)
C*
	CHARACTER	orient*1, label*10
	REAL		size (2), pos (2), xbox (5), ybox (5)
	LOGICAL		cbrflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse the color bar input.
C
	CALL IN_CBAR ( clrbar, icbar, size, ilblfq, orient, cbrflg,
     +		       ixjust, iyjust, pos, ier )
	IF  ( .not. cbrflg )  RETURN
C
C*	Save the current line attributes.
C
	CALL GQLINE ( jltyp, jlthw, jwidth, jwhw, ier )
	CALL GSLINE ( 1, 0, 1, 0, ier )
C
C*	Get plot bounds in View coordinates.
C
	CALL GQBND ( 'V', xl, yb, xr, yt, ier )
C
	CALL ST_LCUC ( orient, orient, ier )
C
C*	Determine the corners of the color bar.
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
	CALL GQFILL ( sizfil, ifltyp, ier )
	CALL GSFILL ( 1.0, 1, ier )
C
C*	Create data level numbers for the color bar
C
	CALL GQCLRS ( imbank, ncolr, ier )
        ratio = FLOAT (ncolr - 1) / (imndlv - 1)
        DO ic = 1, imndlv
            idcols (ic) = (ic - 1) * ratio + .5
        END DO
C
C*	Fill the color boxes
C
	knt = 0
	nflvl = imndlv
	DO  i = 1, nflvl
C
	    knt = knt + 1
	    IF  ( orient .ne. 'H' )  THEN
C
C*		Set up for the vertically oriented color bar
C
		diff = ( yur - yll ) / nflvl
		xbox(1) = xll 
		ybox(1) = yll + (knt-1) * diff
		xbox(2) = xur
		ybox(2) = yll + (knt-1) * diff
		xbox(3) = xur 
		ybox(3) = yll + knt * diff
		xbox(4) = xll 
		ybox(4) = yll + knt * diff
	    ELSE
C
C*		Set up for the horizontally oriented color bar
C
		diff = ( xur - xll ) / nflvl
		xbox(1) = xll + (knt-1) * diff 
		ybox(1) = yll 
		xbox(4) = xll + knt * diff
		ybox(4) = yll
		xbox(3) = xll + knt * diff 
		ybox(3) = yur 
		xbox(2) = xll + (knt-1) * diff
		ybox(2) = yur
	    END IF
C
C*	    Set bar color, fill it, set line color, draw it
C
	    CALL GSCOLB ( imbank, idcols(i), ier )
	    CALL GFILL ( 'N', 4, xbox, ybox, ier )
	    CALL GSCOLB ( 0, icbar, ier )
	    CALL GLINE ( 'N', 2, xbox, ybox, ier )
C
C*	    Plot label.
C
C*	    Label Frequency Not Implemented - Label All
	    IF ( ilblfq .ne. 0 .and. 
     +	         cmblev (i) .ne. ' ' ) THEN
		label = cmblev (i)
C*	VERTICAL
		IF  ( orient .ne. 'H' )  THEN
		    iyoff = 0
C*	RIGHT
		    IF ( ilblfq .ge. 0 ) THEN
			ixoff = 1
		        xlabl = xbox(2)
		        ylabl = ybox(2)
C*	LEFT
		    ELSE
			CALL ST_LSTR ( label, ixoff, ier)
			ixoff = (-2) * ixoff
		        xlabl = xbox(1)
		        ylabl = ybox(1) 
		    END IF
C*	HORIZONTAL
		ELSE
		    CALL ST_LSTR ( label, ixoff, ier)
		    ixoff = ixoff * (-1)
C*	BOTTOM
		    IF ( ilblfq .lt. 0 ) THEN
		        iyoff = -2
		        xlabl = xbox(1)
		        ylabl = ybox(1)
C*	TOP
		    ELSE
		        iyoff = 1
		        xlabl = xbox(2)
		        ylabl = ybox(2)
		    END IF
		END IF
		CALL GTEXT ( 'N', xlabl, ylabl, label, 0.,
     +				     ixoff, iyoff, ier )
	    END IF
	END DO
C
C*	Draw box around the whole bar
C
	xbox(1) = xll
	ybox(1) = yll
	xbox(2) = xur
	ybox(2) = yll
	xbox(3) = xur
	ybox(3) = yur
	xbox(4) = xll
	ybox(4) = yur
	xbox(5) = xll
	ybox(5) = yll
	CALL GLINE ( 'N', 5, xbox, ybox, ier )
C
C*	Reset the line and fill attributes
C
	CALL GSFILL ( sizfil, ifltyp, ier )
	CALL GSLINE ( jltyp, jlthw, jwidth, jwhw, ier )
C
C*	Plot units label
C
C*	VERTICAL
	IF ( orient .ne. 'H' ) THEN
C*	    RIGHT	
	    IF ( ilblfq .ge. 0 ) THEN
	        CALL GTEXT ( 'N', xur, yur, cmbunt, 0., -1, 2, ier )
C*	    LEFT	
	    ELSE
	        CALL GTEXT ( 'N', xll, yur, cmbunt, 0., -4, 2, ier )
	    END IF
C*	HORIZONTAL
	ELSE
C*	    TOP
	    IF ( ilblfq .ge. 0 ) THEN
		CALL GTEXT ( 'N', xur, yur, cmbunt, 0., 1, 1, ier )
C*	    BOTTOM
	    ELSE
		CALL GTEXT ( 'N', xur, yll, cmbunt, 0., 1, -2, ier )
	    END IF
	END IF
C*
	RETURN
	END
