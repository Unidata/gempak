	SUBROUTINE IM_CBAR ( clrbar, iret )
C************************************************************************
C* IM_CBAR								*
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
C* A. Hardy/GSC		 4/98	Added calls to GQFILL and GSFILL	*
C* S. Jacobs/NCEP	 2/99	Moved calls to GQLINE and GSLINE	*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* S. Jacobs/NCEP	 5/99	Moved the labels to center of color box	*
C* T. Piper/GSC		 9/00	Fixed horizontal bar labeling bug	*
C* T. Piper/GSC		 7/01	Fixed typo of variable sizfil		*
C* M. Li/SAIC		11/03	Changed calling sequence		*
C* T. Piper/SAIC	07/06	Put () around -2 to eliminate warning	*
C* T. Piper/SAIC	07/06	Changed GTEXT for units label; top,	*
C*				horizontal case; 2nd to last arg 1 to -2*
C* S. Chiswell/Unidata	11/06	Added parsing of text attributes	*
C* M. James/Unidata     06/10   Moved label frequency logic from IMNIDH *
C* M. James/Unidata     11/13   Hydrometeor classification labeling and *
C*                              national composite GINI support added.  *
C* M. James/Unidata     04/14   High-res NEXRCOMP reworked              *
C************************************************************************
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	clrbar, dhc(16)*4
	INTEGER		idcols (256), clevst, clevsp, dhci, idx
C*
	CHARACTER	orient*1, label*10, clrtxt(2)*128
	REAL		size (2), pos (2), xbox (5), ybox (5)
	LOGICAL		cbrflg, hrulim
        DATA            dhc  / 'ND', 'BI', 'GC', 'IC',
     +                         'DS', 'WS', 'RA', 'HR',
     +                         'BD', 'GR', 'HA', '',
     +                         '', '', 'UK', 'RF' /
C------------------------------------------------------------------------
	iret = 0
C
C*      Split clrtxt string to color bar and text attribute strings.
C
	CALL ST_CLST ( clrbar, '|', ' ', 2, clrtxt, num, ier)
C
C*	Parse the color bar input.
C
	CALL IN_CBAR ( clrtxt(1), icbar, size, ilblfq, orient, cbrflg,
     +		       ixjust, iyjust, pos, ier )
	IF  ( .not. cbrflg )  RETURN
C
C*	Save the current line attributes.
C
	CALL GQLINE ( jltyp, jlthw, jwidth, jwhw, ier )
	CALL GSLINE ( 1, 0, 1, 0, ier )
C
C*      Save Users Text Attributes
C
	CALL GQTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +		      irrotn, ijust, ier )
C
C*      Parse the text_info input and set text attributes. 
C
	CALL IN_TXTN ( clrtxt(2), ifont, iswhw, siztxt,
     +		       itxwd, ibordr, irotat, ijstif, ier )
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
        IF ( imtype .eq. 135) THEN
           imndlv = 199 
        END IF
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
        clevst = 1
        clevsp = nflvl

        SELECT CASE (imtype)
C HHC 
          CASE ( 2**(24) )
            DO idx = 1, imndlv
                cmblev ( idx ) = ''
            END DO
            dhci = 1
            DO idx = 1, imndlv,10 
                cmblev ( idx ) = dhc ( dhci )
                dhci = dhci + 1
            END DO
            cmblev ( imndlv ) = 'RF'
C EET composite GINI 2**25
C
C - this is only for labelling, not data values.  data values range from 
C   2-71 (69 increments) for 0 <= EET < 70k ft
C   0 = Missing data
C   1 = Bad data / flagged
C
          CASE ( 2**(25) )
            DO idx = 1,nflvl
                cmblev ( idx ) = ''
            END DO
            DO idx = 3,73,10
                val = idx - 3 
                CALL ST_INCH ( int(val), cmblev (idx), ier )
            END DO
            DO idx = 131,200,10
                val = idx - 131
                CALL ST_INCH ( int(val), cmblev (idx), ier )
            END DO
            CALL ST_INCH ( int(70), cmblev (199), ier )
            cmblev ( 131 ) = 'TOP'
            cmblev ( 1 ) = ' '
        END SELECT

	DO  i = clevst, clevsp 
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
	    IF ( imbank .ne. 1 )CALL GLINE ( 'N', 2, xbox, ybox, ier )
C
C*	    Plot label.
C
C*	    Label Frequency Not Implemented - Label All
C*	    Special handling for NEXRAD 256-level products
C
	    IF ( ilblfq .ne. 0 .and. cmblev (i) .ne. ' ' ) THEN
               IF ( imftyp .eq. 13 ) THEN
                  SELECT CASE ( imtype ) 
                     CASE (94,32)
                        IF ( ( i .eq. clevsp ) .or.
     +                    ( MOD ( i + 3 , 10 ) .eq. 0 ) ) THEN
                           label = cmblev (i)
                        ELSE
                           label = ' '
                        END IF
                     CASE (99)
                        IF ( MOD ( i-10 , 20 ) .eq. 0 ) THEN
                           label = cmblev (i)
                        ELSE
                           label = ' '
                        END IF
                     CASE (170,172,173,174,175) 
                        IF ( ( i .eq. 1 ) .or.
     +                    ( MOD ( i , 20 ) .eq. 0  ) ) THEN 
                           label = cmblev (i)
                        ELSE
                           label = ' '
                        END IF
                     CASE (159) 
                        IF ( MOD ( i , 16 ) .eq. 0   ) THEN 
                           label = cmblev (i)
                        ELSE
                           label = ' '
                        END IF
                     CASE (163) 
                        IF ( MOD ( i , 20 ) .eq. 3   .or.
     +                   (i .eq. 33) .or. (i .eq. 12)) THEN 
                           label = cmblev (i)
                        ELSE
                           label = ' '
                        END IF
                     CASE (161) 
                        IF ( ( i .eq. 1 ) .or.
     +                    ( MOD ( i , 30 ) .eq. 0  ) ) THEN 
                           label = cmblev (i)
                        ELSE
                           label = ' '
                        END IF
                     CASE (134)
                        IF ( ( i .eq. 1 ) .or. 
     +                    ( MOD ( i , 12 ) .eq. 0 ) ) THEN
                           label = cmblev (i)
                        ELSE
                           label = ''
                        END IF
                     CASE (135)
                        IF ( ( i .ge. 3 )  .and.
     +                    ( i .le. 73 ) .and.
     +                    ( MOD ( i , 10 ) .eq. 2) ) THEN
                           label = cmblev (i)
                        ELSE IF ( ( i .ge. 131 ) .and.
     +                    ( i .le. 200 ) .and.
     +                    ( MOD ( i , 10 ) .eq. 0 ) ) THEN
                           label = cmblev (i)
                        ELSE IF ( i .eq. 2 ) THEN
                           label = cmblev (i)
                        ELSE IF ( ( i .eq. 71 ) .or. 
     +                    ( i .eq. 199 ) .or.
     +                    ( i .eq. 130 )  ) THEN
                           label = cmblev (i)
                        ELSE
                           label = ''
                        END IF
                     CASE DEFAULT 
                        label = cmblev (i)
                  END SELECT 
               ELSE
                  label = cmblev (i)
               END IF
C*	VERTICAL
		IF  ( orient .ne. 'H' )  THEN
		    iyoff = 0
C*	RIGHT
		    IF ( ilblfq .ge. 0 ) THEN
		        ixoff = 1
		        xlabl = xbox(2)
		        ylabl = ( ybox(2) + ybox(3) ) / 2.0
C*	LEFT
		    ELSE
			CALL ST_LSTR ( label, ixoff, ier)
			ixoff = (-2) * ixoff
		        xlabl = xbox(1)
		        ylabl = ( ybox(1) + ybox(4) ) / 2.0
		    END IF
C*	HORIZONTAL
		ELSE
		    CALL ST_LSTR ( label, ixoff, ier)
		    ixoff = ixoff * (-1)
C*	BOTTOM
		    IF ( ilblfq .lt. 0 ) THEN
		        iyoff = -2
		        xlabl = ( xbox(1) + xbox(4) ) / 2.0
		        ylabl = ybox(1)
C*	TOP
		    ELSE
		        iyoff = 1
		        xlabl = ( xbox(2) + xbox(3) ) / 2.0
		        ylabl = ybox(2)
		    END IF
		END IF
		CALL GTEXT ( 'N', xlabl, ylabl, label, 0.,
     +				     ixoff, iyoff, ier )
C	    END IF
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
		CALL GTEXT ( 'N', xll, yll, cmbunt, 0., 1, -2, ier )
C*	    BOTTOM
	    ELSE
		CALL GTEXT ( 'N', xur, yll, cmbunt, 0., 1, -2, ier )
	    END IF
	END IF
C
C*	Reset User's Text Attributes
C
	CALL GSTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +                irrotn, ijust, ier )
C*
	RETURN
	END
