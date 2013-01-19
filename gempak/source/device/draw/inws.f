	SUBROUTINE INWS ( iwndw, x, y, size, iclmod, iret )
C************************************************************************
C* INWS									*
C*									*
C* This subroutine draws the NOAA seagull emblem.			*
C*									*
C* INWS ( IWNDW, X, Y, SIZE, ICLMOD, IRET )				*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	SIZE		REAL		Emblem size			*
C*	ICLMOD		INTEGER		Emblem color mode		*
C*						'1' = monochrome	*
C*						'2' = color		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 4/01	Copied from INOAA			*
C* S. Jacobs/NCEP	10/07	Change font to software font 3		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	PARAMETER	( SCLFAC = 220.0 )
C!			The scale factor was derived by trial and error.
C*
	INTEGER		iltng ( 16 ), icloud ( 234 ),
     +			iwater ( 67 ), iray ( 2, 4 )
	REAL		xr ( 250 ), yr ( 250 )
	CHARACTER	string*80
C------------------------------------------------------------------------
C
C@	The following data statements define the irregular curves that
C@	are a part of the emblem. The emblem is drawn on a 1000 x 1000
C@	grid.
C@
C@	Each X and Y coordinate pair is coded as a single number where
C@	values 1 to 999 represent the X coordinate, and values 1000 to
C@	999000 represent the Y coordinate. The origin of the grid is
C@	the lower left corner, specified by 1001. Point 999999 coincides
C@	with the upper right corner. The center of the grid is point
C@	500500.
C
	DATA  iray    /	305598, 360570,
     +			487719, 487625,
     +			588698, 552630,
     +			683619, 624585 /
C
	DATA  iwater  /	663365, 660365, 655365, 650365, 645365, 640366,
     +			635368, 633370, 625367, 614358, 610357, 605355,
     +			600352, 595352, 590351, 585351, 580351, 575352,
     +			570353, 565355, 560358, 555361, 550364, 548365,
     +			545362, 540356, 535352, 530350, 525350, 520350,
     +			515351, 512353, 510350, 505348, 500347, 495345,
     +			490345, 485345, 480346, 475347, 470349, 465352,
     +			460354, 455357, 450358, 445362, 440357, 435356,
     +			430355, 425354, 420353, 415352, 410351, 405350,
     +			400350, 395350, 390350, 385350, 380350, 375350,
     +			370350, 365350, 360350, 355351, 350352, 345354,
     +			340355 /
C
	DATA  icloud  / 287552, 285550, 280545, 276540, 272535, 269530,
     +			267525, 266520, 265515, 266510, 267505, 268500,
     +			273495, 277490, 270485, 265480, 262475, 260470,
     +			258465, 257460, 255455, 257450, 258445, 260440,
     +			263435, 266430, 270425, 275420, 280416, 285413,
     +			290411, 295410, 300408, 305409, 310410, 315412,
     +			320414, 325416, 330418, 335421, 340425, 345415,
     +			350408, 355402, 360398, 365395, 370393, 375392,
     +			380390, 385390, 390389, 395388, 400389, 405392,
     +			410394, 415396, 420399, 423402, 425400, 430397,
     +			435394, 440389, 445386, 450383, 455380, 460378,
     +			465376, 470375, 475375, 480376, 485377, 490378,
     +			495378, 500378, 505377, 510376, 515376, 520376,
     +			525377, 530378, 535379, 540380, 545383, 550385,
     +			556390, 560395, 596398, 600400, 606405, 607410,
     +			608412, 610410, 615409, 620408, 625407, 630408,
     +			635409, 640410, 645414, 651420, 653425, 654430,
     +			652435, 650439, 647442, 650442, 655442, 660443,
     +			665443, 670444, 675444, 680445, 685446, 690447,
     +			695448, 700450, 707455, 712460, 713465, 714470,
     +			714475, 715480, 715485, 717490, 724495, 727500,
     +			730505, 734510, 737515, 739520, 741525, 743530,
     +			742535, 741540, 737545, 735548, 730553, 725556,
     +			720560, 715563, 710564, 705565, 700566, 695566,
     +			690565, 685565, 680564, 675563, 670562, 665560,
     +			670565, 671570, 672572, 668575, 665577, 660578,
     +			655578, 650577, 645575, 640573, 635570, 632575,
     +			627580, 624585, 620589, 615594, 610597, 605602,
     +			600607, 595611, 590613, 585617, 580620, 575623,
     +			570626, 565628, 560629, 555630, 552630, 550630,
     +			545632, 540633, 535634, 530635, 525635, 520635,
     +			515634, 510633, 505631, 500630, 495628, 490626,
     +			487625, 485626, 480628, 475629, 470630, 465630,
     +			460631, 455630, 450630, 445630, 440629, 435627,
     +			430625, 407616, 405615, 400612, 395608, 390603,
     +			385597, 380590, 375583, 372575, 370575, 365573,
     +			360570, 357565, 353560, 352557, 350560, 345562,
     +			340564, 335564, 330565, 325565, 320564, 315564,
     +			310563, 305561, 300559, 295557, 290554, 287552 /
C
	DATA  iltng   / 320675, 385675, 457597, 443593, 515510, 505505,
     +			585420, 580417, 665327, 532410, 562432, 467497,
     +			495517, 400583, 428603, 320675 /
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Save current attributes that are modified in this routine.
C
	icolr  = mcolr
C
	itxfn  = mtxfn
	itxhw  = mtxhw
	sztxt  = txsize
	itxwd  = mtxwid
	ibrdr  = mbrdr
	irrotn = mrrotn
	ijust  = mjust
	msvft  = mfltyp
	svfsz  = tfilsz
	iltyp  = mltyp
	ilthw  = mlthw
	ilwid  = mlwid
	ilwhw  = mlwhw
C
C*	Set colors for a monochrome emblem.
C
        IF ( iclmod .eq. 1 ) THEN
	    iwht = 101
	    idkb = icolr
	    iltb = 101
	    ired = icolr
C
C*	Set colors for a full color emblem.
C
        ELSE IF ( iclmod .eq. 2 ) THEN
	    IF  ( ddev .eq. 'PS' )  THEN
		iwht = 32
	      ELSE
		iwht = 31
	    END IF
            idkb = 4
            iltb = 26
	    ired = 2
        ENDIF
C
C*	Draw the white circular field with a dark blue border.
C
	rad = size * bscall
C
	np = 100
	DO  i = 1, np
	    p = TWOPI * FLOAT ( i - 1 ) / FLOAT ( np - 1 )
	    xr ( i ) = x + rad * COS ( p ) * FLOAT ( ispanx )
	    yr ( i ) = y + rad * SIN ( p ) * FLOAT ( ispany )
	END DO
C
	CALL DSCOLR ( iwht, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, np, xr, yr, iret )
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSLINE ( 1, 0, 2, 1, j1, j2, j3, j4, ier )
	CALL DLINE  ( iwndw, np, xr, yr, iret )
C
C*	Draw a light blue half circular field for the sky.
C
	rad2 = rad * 0.72
C
	np = 100
	is = 1
	ie = np / 2
	DO  i = is, ie
	    p = TWOPI * FLOAT ( i - 1 ) / FLOAT ( np - 1 )
	    xr ( i ) = x + rad2 * COS ( p ) * FLOAT ( ispanx )
	    yr ( i ) = y + rad2 * SIN ( p ) * FLOAT ( ispany )
	END DO
C
	npt = ie - is + 1
	nnp = npt + 1
	xr ( nnp ) = xr ( 1 )
	yr ( nnp ) = yr ( 1 )
C
	CALL DSCOLR ( iltb, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, nnp, xr, yr, iret )
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSLINE ( 1, 0, 2, 1, j1, j2, j3, j4, ier )
	CALL DLINE  ( iwndw, nnp, xr, yr, iret )
C
C*	Draw the sun rays.
C
	rx2 = ( rad2 * FLOAT ( ispanx ) ) / SCLFAC
	ry2 = ( rad2 * FLOAT ( ispany ) ) / SCLFAC
	DO  j = 1, 4
	    DO  i = 1, 2
		ix =       iray ( i, j ) / 1000   - 500
		iy = MOD ( iray ( i, j ),  1000 ) - 500
		xr ( i ) = x + rx2 * FLOAT ( ix )
		yr ( i ) = y + ry2 * FLOAT ( iy )
	    END DO
C
	    CALL DSCOLR ( idkb, j1, iret )
	    CALL DSLINE ( 1, 0, 2, 1, j1, j2, j3, j4, ier )
	    CALL DLINE  ( iwndw, 2, xr, yr, iret )
	END DO
C
C*	Draw the water.
C
	np = 100
	is = 61
	ie = 90
	DO  i = is, ie
	    p = TWOPI * FLOAT ( i - 1 ) / FLOAT ( np - 1 )
	    xr ( i - is + 1 ) = x + rad2 * COS ( p ) * FLOAT ( ispanx )
	    yr ( i - is + 1 ) = y + rad2 * SIN ( p ) * FLOAT ( ispany )
	END DO
C
	npt = ie - is + 1
C
	rx2 = ( rad2 * FLOAT ( ispanx ) ) / SCLFAC
	ry2 = ( rad2 * FLOAT ( ispany ) ) / SCLFAC
	DO  i = 1, 67
	    ix =       iwater ( i ) / 1000   - 500
	    iy = MOD ( iwater ( i ),  1000 ) - 500
	    xr ( i + npt ) = x + rx2 * FLOAT ( ix )
	    yr ( i + npt ) = y + ry2 * FLOAT ( iy )
	END DO
C
	npt = npt + 67
	CALL DSCOLR ( iltb, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, npt, xr, yr, iret )
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSLINE ( 1, 0, 2, 1, j1, j2, j3, j4, ier )
	CALL DLINE  ( iwndw, npt, xr, yr, iret )
C
C*	Draw a dark blue line around the main part of the logo.
C
	np = 100
	DO  i = 1, np
	    p = TWOPI * FLOAT ( i - 1 ) / FLOAT ( np - 1 )
	    xr ( i ) = x + rad2 * COS ( p ) * FLOAT ( ispanx )
	    yr ( i ) = y + rad2 * SIN ( p ) * FLOAT ( ispany )
	END DO
C
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSLINE ( 1, 0, 2, 1, j1, j2, j3, j4, ier )
	CALL DLINE  ( iwndw, np, xr, yr, iret )
C
C*	Draw the cloud.
C
	rx2 = ( rad2 * FLOAT ( ispanx ) ) / SCLFAC
	ry2 = ( rad2 * FLOAT ( ispany ) ) / SCLFAC
	DO  i = 1, 234
	    ix =       icloud ( i ) / 1000   - 500
	    iy = MOD ( icloud ( i ),  1000 ) - 500
	    xr ( i ) = x + rx2 * FLOAT ( ix )
	    yr ( i ) = y + ry2 * FLOAT ( iy )
	END DO
C
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, 234, xr, yr, iret )
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSLINE ( 1, 0, 2, 1, j1, j2, j3, j4, ier )
	CALL DLINE  ( iwndw, 234, xr, yr, iret )
C
C*	Draw the lightning bolt.
C
	rx2 = ( rad2 * FLOAT ( ispanx ) ) / SCLFAC
	ry2 = ( rad2 * FLOAT ( ispany ) ) / SCLFAC
	DO  i = 1, 16
	    ix =       iltng ( i ) / 1000   - 500
	    iy = MOD ( iltng ( i ),  1000 ) - 500
	    xr ( i ) = x + rx2 * FLOAT ( ix )
	    yr ( i ) = y + ry2 * FLOAT ( iy )
	END DO
C
	CALL DSCOLR ( iwht, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, 16, xr, yr, iret )
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSLINE ( 1, 0, 2, 1, j1, j2, j3, j4, ier )
	CALL DLINE  ( iwndw, 16, xr, yr, iret )
C
C*	Plot the stars.
C
	ixs = 500
	iys = 240
	np  = 11
	rx2 = ( rad2 * FLOAT ( ispanx ) ) / SCLFAC
	ry2 = ( rad2 * FLOAT ( ispany ) ) / SCLFAC
	xc  = x + rx2 * FLOAT ( ixs - 500 )
	yc  = y + ry2 * FLOAT ( iys - 500 )
	DO  i = 1, 11
	    p = HALFPI + TWOPI * FLOAT ( i - 1 ) / FLOAT ( np - 1 )
	    IF  ( MOD(i,2) .eq. 0 )  THEN
		rx = (rad * 0.04) * COS ( p ) * FLOAT ( ispanx )
		ry = (rad * 0.04) * SIN ( p ) * FLOAT ( ispany )
	      ELSE
		rx = (rad * 0.10) * COS ( p ) * FLOAT ( ispanx )
		ry = (rad * 0.10) * SIN ( p ) * FLOAT ( ispany )
	    END IF
	    xr ( i ) = xc + rx
	    yr ( i ) = yc + ry
	END DO
C
	CALL DSCOLR ( ired, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, 11, xr, yr, iret )
C
	ixs = 580
	iys = 260
	np  = 11
	rx2 = ( rad2 * FLOAT ( ispanx ) ) / SCLFAC
	ry2 = ( rad2 * FLOAT ( ispany ) ) / SCLFAC
	xc  = x + rx2 * FLOAT ( ixs - 500 )
	yc  = y + ry2 * FLOAT ( iys - 500 )
	DO  i = 1, np
	    p = HALFPI + TWOPI * FLOAT ( i - 1 ) / FLOAT ( np - 1 )
	    IF  ( MOD(i,2) .eq. 0 )  THEN
		rx = (rad * 0.03) * COS ( p ) * FLOAT ( ispanx )
		ry = (rad * 0.03) * SIN ( p ) * FLOAT ( ispany )
	      ELSE
		rx = (rad * 0.075) * COS ( p ) * FLOAT ( ispanx )
		ry = (rad * 0.075) * SIN ( p ) * FLOAT ( ispany )
	    END IF
	    xr ( i ) = xc + rx
	    yr ( i ) = yc + ry
	END DO
C
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, 11, xr, yr, iret )
C
	ixs = 420
	iys = 260
	np  = 11
	rx2 = ( rad2 * FLOAT ( ispanx ) ) / SCLFAC
	ry2 = ( rad2 * FLOAT ( ispany ) ) / SCLFAC
	xc  = x + rx2 * FLOAT ( ixs - 500 )
	yc  = y + ry2 * FLOAT ( iys - 500 )
	DO  i = 1, np
	    p = HALFPI + TWOPI * FLOAT ( i - 1 ) / FLOAT ( np - 1 )
	    IF  ( MOD(i,2) .eq. 0 )  THEN
		rx = (rad * 0.03) * COS ( p ) * FLOAT ( ispanx )
		ry = (rad * 0.03) * SIN ( p ) * FLOAT ( ispany )
	      ELSE
		rx = (rad * 0.075) * COS ( p ) * FLOAT ( ispanx )
		ry = (rad * 0.075) * SIN ( p ) * FLOAT ( ispany )
	    END IF
	    xr ( i ) = xc + rx
	    yr ( i ) = yc + ry
	END DO
C
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, 11, xr, yr, iret )
C
C*	Plot the legend in a circle around the outer edge.
C
	trad  = rad * 0.90
	tsiz  = rad * 0.017 / bscalc
	itwid = NINT ( rad * 0.035 / bscalc )
	IF ( itwid .lt. 1 ) itwid = 1
C
	CALL DSCOLR ( ired, j1, iret )
	CALL DSTEXT ( 3, 1, tsiz, itwid, 111, 1, 2,
     +		      j1, j2, s3, j4, j5, j6, j7, iret )
C
	string = 'NATIONAL WEATHER SERVICE'
	CALL DCRVTXT ( iwndw, x, y, trad, 90., 29, 0, string, iret )
C
C*	Restore current attributes.
C
	CALL DSCOLR ( icolr, j1, iret )
	CALL DSTEXT ( itxfn, itxhw, sztxt, itxwd, ibrdr, irrotn, ijust,
     +		      j1, j2, s3, j4, j5, j6, j7, iret )
	CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
	CALL DSLINE ( iltyp, ilthw, ilwid, ilwhw,
     +		      j1, j2, j3, j4, iret )
C*
	RETURN
	END
