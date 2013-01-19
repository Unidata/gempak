	SUBROUTINE GG_SCAL ( mscale, iret )
C************************************************************************
C* GG_SCAL								*
C*									*
C* This routine will draw a scale legend on a map.			*
C*									*
C* GG_SCAL  ( MSCALE, IRET )						*
C*									*
C* Input parameters:							*
C*	MSCALE		CHAR*		scale legend, text string input * 
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Piper/SAIC	07/04	Created based upon gg_cbar		*
C* T. Piper/SAIC	08/04	Corrected boxsiz calculation		*
C* T. Piper/SAIC	10/04	New algorithm based upon Plot coord.	*
C* T. Piper/SAIC	10/04	Added logic for true at the equator	*
C* T. Piper/SAIC	12/04	Added support for background color	*
C* T. Piper/SAIC	01/05	Added title, mask, and optional legend	*
C* T. Piper/SAIC	02/05	Removed units plot from legend check	*
C************************************************************************
	INCLUDE	'GEMPRM.PRM'
C
	CHARACTER*(*)	mscale
C*
	REAL		pos(2), size(2), values(20), xbox(5), ybox(5)
	INTEGER		icolor(3), just(2)
	LOGICAL		legnd, sclflg, valflg
	CHARACTER	cdir*6, label*10, qprj*4, sclleg(3)*100, 
     +			title*100, true*20, trulat*5, units*2
C------------------------------------------------------------------------
	iret = 0
C
C*      Split mscale string into scale legend, text attributes, 
C*	and title string.
C
	CALL ST_CLS2 ( mscale, '|', ' ', 3, sclleg, num, ier )
	title = sclleg(3)
	CALL ST_LSTR ( title, lent, ier )
	CALL ST_CLST ( mscale, '|', ' ', 3, sclleg, num, ier )
C
C*	Parse the scale legend input.
C
	CALL IN_MSCL ( sclleg(1), sclflg, icolor, units, cenlat, nval,
     +			values, just, pos, size, lblfq, legnd, ier )
	IF  ( .not. sclflg )  RETURN
C
	IF ( nval .eq. 0 )  THEN
	    valflg = .false.
	ELSE
	    valflg = .true.
	END IF
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
C*      Parse the text_info input.  Set missing values to defaults.
C
        CALL IN_TXTN ( sclleg(2), ifont, iswhw, siztxt, 
     +                 itxwd, ibordr, irotat, ijstif, iret )
        if ( ifont  .le. 0 ) ifont = 21
        if ( iswhw  .le. 0 ) iswhw = 2
        if ( siztxt .le. 0 ) siztxt = 1.
        if ( itxwd  .le. 0 ) itxwd = 1
C
        ibordr = 111
        irotat = 1
        ijstif = 1
C
        CALL GSTEXT ( ifont, iswhw, siztxt, itxwd, 
     +                ibordr, irotat, ijstif, ier )
C
C*	Get plot bounds in View and Plot coordinates.
C
	CALL GQBND ( 'V', xl, yb, xr, yt, ier )
	view_size = xr - xl
	CALL GQBND ( 'P', pxl, pyb, pxr, pyt, ier )
	panel_size = pxr - pxl
C
C*	Compute distances.
C
C*	Horizontal
C
	ihoriz = 1
	ivert = 2
	CALL GQMPRJ  ( qprj, qang1, qang2, qang3, qlats,
     +                             qlonw, qlatn, qlone, ier )
	IF  ( ier .ne. 0 )  THEN
            iret = -5
            CALL ER_WMSG  ( 'GEMPLT', ier, ' ', ier2 )
        END IF
	cenlon = qang2
C
C*	Find center of panel display in Plot coordinates.
C
	IF ( cenlat .ne. RMISSD .and. cenlon .ne. RMISSD )  THEN
 	    CALL GTRANS ( "M", "P", 1, cenlat, cenlon, pxo, pyo, iret )
	ELSE
	    pxo = ( pxl + pxr ) / 2.0
	    pyo = ( pyb + pyt ) / 2.0
	    CALL GTRANS ( "P", "M", 1, pxo, pyo, cenlat, cenlon, iret )
	END IF
C
C*	Calculate distance for 1/10th of panel display.
C
	pdist = panel_size / 20.0
C
	CALL GTRANS ( "P", "M", 1, pxo - pdist, pyo, x1, y1, iret )
	CALL GTRANS ( "P", "M", 1, pxo + pdist, pyo, x2, y2, iret )
C 
	CALL CLO_DIST (x1, y1, 1, x2, y2, xdist, ier)
	xdist = xdist * 10.0
C
	IF ( units .eq. 'NM' )  THEN
	    xdist = xdist * M2NM
	ELSE IF ( units .eq. 'KM' )  THEN
	    xdist = xdist / 1000.0
	ELSE 
	    units = 'SM'
	    xdist = xdist * M2SM 
	END IF
C
C*	Create 'True at' legend
C
	IF ( legnd )  THEN
	    IF ( cenlat .gt. -0.1 .and. cenlat .lt. 0.1 )  THEN
	        trulat = 'the e'
	        cdir = 'quator'
	    ELSE 
	        CALL ST_RLCH ( ABS(cenlat), 2, trulat, ier )
	        IF ( cenlat .ge. 0 )  THEN
	            cdir = 'N'
	        ELSE
	            cdir = 'S'
	        END IF
	    END IF
	    true = "True at " // trulat // cdir
	END IF
C
C*	Check if we have a list of values.
C
	IF ( valflg )  THEN
	    size(ihoriz) = (values(nval)/xdist) * (panel_size/view_size)
	ELSE
	    nval = 4
	    scallbl = xdist * size(ihoriz)
	    iscllbl = scallbl
	    IF ( MOD(iscllbl, nval) .ne. 0 )  THEN
	        iscllbl = iscllbl + (nval - MOD(iscllbl,nval))
	    END IF
	    size(ihoriz) = size(ihoriz) * iscllbl / scallbl
	    size(ihoriz) = size(ihoriz) * panel_size / view_size 
	    values(1) = iscllbl / nval
	    DO ii = 2, nval
		values(ii) = values(ii-1) + values(1)
	    END DO 
	END IF
C
C*	Determine the corners of the scale legend.
C
	IF ( just(1) .eq. 3 ) THEN
C*	Right
	    xll = pos(1) - size(ihoriz)
	    xur = pos(1)
	ELSE IF ( just(1) .eq. 2 ) THEN
C*	Center
	    xll = pos(1) - size(ihoriz) / 2
	    xur = pos(1) + size(ihoriz) / 2
	ELSE
C*	Left
	    xll = pos(1)
	    xur = pos(1) + size(ihoriz)
	ENDIF
	IF ( just(2) .eq. 3 ) THEN
C*	Top	
	    yll = pos(2) - size(ivert)
	    yur = pos(2)
	ELSE IF ( just(2) .eq. 2 ) THEN
C*	Center
	    yll = pos(2) - size(ivert) / 2
	    yur = pos(2) + size(ivert) / 2
	ELSE
C*	Bottom	
	    yll = pos(2)
	    yur = pos(2) + size(ivert)
	ENDIF
	xll = xll * view_size + xl
	xur = xur * view_size + xl
	yll = yll * ( yt - yb ) + yb
	yur = yur * ( yt - yb ) + yb
C
C*	Set the fill attributes.
C
	CALL GQFILL ( szfil, ifltyp, ier )
	CALL GSFILL ( 1.0, 1, ier )
C
C*	Set color for background fill
C
	IF ( icolor(3) .ne. IMISSD )  THEN
	    CALL GSCOLR ( icolor(3), ier )
	    IF ( lblfq .lt. 0 )  THEN
		IF ( legnd ) THEN
	            botmargin = 0.03 * siztxt
		ELSE
		    botmargin = 0.02 * siztxt
		END IF
		IF ( num .eq. 3 )  THEN
		    topmargin = 0.02 * siztxt
		ELSE
	            topmargin = 0.01 
		END IF
	    ELSE
		IF ( num .eq. 3 )  THEN
		    botmargin = 0.02 * siztxt
		ELSE
	            botmargin = 0.01
		END IF
		IF ( legnd ) THEN
		    topmargin = 0.025 * siztxt
		ELSE
		    topmargin = 0.015 * siztxt
		END IF
	    ENDIF
	    xoff = 0.02 * siztxt
	    xbox(1) = xll - xoff 
	    ybox(1) = yll - botmargin 
	    xbox(2) = xbox(1)
	    ybox(2) = yur + topmargin 
	    xbox(3) = xur + xoff
	    CALL GQSYSZ ( szmx, szmy, sztx, szty, szwx, szwy, ier )
	    title_len = lent * sztx
	    IF ( title_len .gt. (xbox(3) - xbox(1)) )  THEN
		xbox(3) = xbox(1) + title_len + xoff
	    END IF
	    ybox(3) = ybox(2)
	    xbox(4) = xbox(3) 
	    ybox(4) = ybox(1)
	    CALL GFILL ( 'N', 4, xbox, ybox, ier )
	END IF
C
C*	Compute the number of decimal places to display.
C
	CALL GR_NDCP ( nval, values, ndecp, ier )
	IF  ( ndecp .eq. 0 )  THEN
	    iform = 1
	ELSE IF  ( ndecp .gt. 0 )  THEN
	    iform = 2
	ELSE
	    iform = 3
	END IF
C
C*	Draw the scale legend.
C
	IF ( .not. valflg )  THEN
	    diff = ( xur - xll ) / nval
	END IF
	ybox(1) = yll
	ybox(2) = ybox(1) 
	ybox(3) = yur
	ybox(4) = ybox(3) 
	ybox(5) = ybox(1)
	xbox(2) = xll
C
	DO  knt1 = 1, nval
C
C*	Set up the scale legend. 
C
	    xbox(1) = xbox(2)
	    IF ( .not. valflg )  THEN
		xbox(2) = xll + knt1 * diff
	    ELSE
		boxsiz = size(ihoriz) * (values(knt1)/values(nval))
		boxsiz = boxsiz * view_size 
		xbox(2) = xll + boxsiz
	    END IF
	    xbox(3) = xbox(2)
	    xbox(4) = xbox(1)
C
C*          Set color for box.
C
	    IF ( MOD(knt1,2) .eq. 0 )  THEN 
                CALL GSCOLR ( icolor(1), ier )
	    ELSE
		CALL GSCOLR ( icolor(2), ier )
	    END IF
            CALL GFILL ( 'N', 4, xbox, ybox, ier )
C
C*	    Draw box around fill if requested.
C
	    IF ( icolor(1) .ge. 0 ) THEN
		CALL GSCOLR ( icolor(1), ier )
		xbox(5) = xbox(1)
		CALL GLINE ( 'N', 5, xbox, ybox, ier )
	    END IF
C
C*	    Plot label.
C
	    IF ( lblfq .ne. 0 ) THEN
		IF ( MOD((knt1+(ABS(lblfq)-1)), lblfq ) .eq. 0 )  THEN 
C
C*		    Encode value into a character, checking to see
C*		    how many decimal places to use.
C
		    CALL GR_LABL ( values(knt1), iform, ndecp, label,
     +				   ixoff, ier )
C*
		    CALL GSCOLR ( icolor(1), ier )
		    ixoff = ixoff * (-1)
		    IF ( lblfq .lt. 0 ) THEN
			iyoff = -2
			iyoff2 = 2
			iyoff3 = -4
			xlabl = xbox(2)
			ylabl = yll 
			ytitl = yur 
		    ELSE
			iyoff = 1 
			iyoff2 = -2
			iyoff3 = 3 
			xlabl = xbox(3)
			ylabl = yur 
			ytitl = yll 
		    END IF
		    CALL GTEXT ( 'N', xlabl, ylabl, label, 0.,
     +				 ixoff, iyoff, ier )
		    IF ( knt1 .eq. 1 )  THEN
			CALL GTEXT ( 'N', xbox(1), ylabl, units, 0.,
     +					-1, iyoff, ier )
			IF ( num .eq. 3 ) CALL GTEXT ( 'N', xbox(1), 
     +						ytitl, title, 0.,
     +						-1, iyoff2, ier )
			IF ( legnd )  THEN
			    CALL GTEXT ( 'N', xbox(1), ylabl, true, 0.,
     +					-1, iyoff3, ier )
			END IF
		    END IF
		END IF
	    END IF
	END DO
C
C*	Reset User's Fill, Line, & Text Attributes.
C
	CALL GSFILL ( szfil, ifltyp, ier )
	CALL GSLINE ( jltyp, jlthw, jwidth, jwhw, ier )
	CALL GSTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr, 
     +		      irrotn, ijust, iret )
C
	RETURN
	END
