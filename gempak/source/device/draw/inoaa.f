	SUBROUTINE INOAA ( iwndw, x, y, size, iclmod, itxmod, iret )
C************************************************************************
C* INOAA								*
C*									*
C* This subroutine draws the NOAA seagull emblem.			*
C*									*
C* INOAA ( IWNDW, X, Y, SIZE, ICLMOD, IRET )				*
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
C* J. Wu/GSC		03/01  	Moved from the device/plot/dlogo.f	*
C* S. Jacobs/NCEP	 4/01	Fixed color for "white" for PS driver	*
C* S. Jacobs/NCEP	10/07	Change font to software font 3		*
C* S. Jacobs/NCEP	 4/10	Add option for plotting text		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		icurve1 ( 49 ), icurve2 ( 79 )
	INTEGER		inshape ( 23 ), ioshape ( 57 ), iashape ( 51 )
	REAL		xr ( 140 ), yr ( 140 )
	CHARACTER	string*80
C------------------------------------------------------------------------
C
C@	The following data statements define the irregular curves that
C@	are a part of the emblem.  The emblem is drawn on a 1000 x 1000
C@	grid.  The inner circle of the emblem has a diameter of 876
C@	units and it is centered at (500, 500).  Each X and Y coordinate
C@	pair is coded as a single number where values 1 to 999 represent
C@	the X coordinate, and values 1000 to 999000 represent the Y
C@	coordinate.  The origin of the grid is the lower left corner,
C@	specified by 1001.  Point 999999 coincides with the upper right
C@	corner.  The center of the grid is point 500500.
C
	DATA  icurve1 /	143754, 150750, 170736, 190720, 200711, 220691,
     +			260643, 290604, 310576, 320561, 330547, 340534,
     +			350522, 390482, 410464, 420456, 430449, 440443,
     +			460433, 480425, 500419, 510417, 520416, 523416,
     +			532417, 550421, 560424, 570428, 580433, 590439,
     +			600447, 610457, 630479, 650503, 660518, 680554,
     +			700592, 720632, 740670, 750688, 760702, 770714,
     +			780725, 790735, 800743, 810749, 830759, 840763,
     +			849765 /
C
	DATA  icurve2 /	079621, 090616, 100611, 110604, 120596, 130587,
     +			140577, 160555, 180531, 190518, 200507, 210497,
     +			230479, 250463, 270449, 300431, 320421, 370401,
     +			390395, 430387, 460384, 470384, 490386, 495386,
     +			470368, 440353, 410341, 380332, 360328, 350327,
     +			360321, 370316, 380312, 390310, 400309, 410309,
     +			430311, 450315, 470321, 490329, 520344, 530350,
     +			560365, 600381, 620387, 650393, 680396, 690396,
     +			700395, 700399, 699403, 698405, 695409, 693411,
     +			689414, 687415, 684416, 680417, 665418, 660418,
     +			645417, 635416, 620415, 610415, 595416, 590417,
     +			610423, 630431, 650441, 680459, 710480, 730496,
     +			750514, 770534, 790556, 820592, 850631, 890687,
     +			894692 /
C
	DATA  inshape /	316652, 316767, 365767, 372766, 377764, 379763,
     +			383760, 386756, 387754, 389749, 390742, 390652,
     +			365652, 365743, 364745, 363746, 361747, 345747,
     +			343746, 342745, 341743, 341652, 316652 /
C
	DATA  ioshape /	447767, 459767, 466766, 471764, 473763, 477760,
     +			480756, 481754, 483749, 484742, 484677, 483670,
     +			481665, 480663, 477659, 473656, 471655, 466653,
     +			459652, 435652, 428653, 423655, 421656, 417659,
     +			414663, 413665, 411670, 410677, 410742, 411749,
     +			413754, 414756, 417760, 421763, 423764, 428766,
     +			435767, 447767, 447747, 439747, 437746, 436745,
     +			435743, 435676, 436674, 437673, 439672, 455672,
     +			457673, 458674, 459676, 459743, 458745, 457746,
     +			455747, 447747, 447767 /
C
	DATA  iashape /	541767, 553767, 560766, 565764, 567763, 571760,
     +			574756, 575754, 577749, 578742, 578652, 553652,
     +			553693, 552695, 551696, 549697, 533697, 531696,
     +			530695, 529693, 529652, 504652, 504742, 505749,
     +			507754, 508756, 511760, 515763, 517764, 522766,
     +			529767, 541767, 541747, 533747, 531746, 530745,
     +			529743, 529721, 530719, 531718, 533717, 549717,
     +			551718, 552719, 553721, 553743, 552745, 551746,
     +			549747, 541747, 541767 /
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
C
C*	Set colors for a monochrome emblem.
C
        IF ( iclmod .eq. 1 ) THEN
	    iwht = 101
	    idkb = icolr
	    iltb = icolr
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
        ENDIF
C
C*	Generate a set of points in a circle.
C
	rad = size * bscall
	IF  ( itxmod .eq. 1 )  THEN
	    rad2 = rad
	ELSE
	    rad2 = rad * 0.825
	END IF
C
	np = 100
	DO  i = 1, np
	    p = TWOPI * FLOAT ( i - 1 ) / FLOAT ( np - 1 )
	    xr ( i ) = x + rad2 * COS ( p ) * FLOAT ( ispanx )
	    yr ( i ) = y + rad2 * SIN ( p ) * FLOAT ( ispany )
	END DO
C
C*	Draw the white circular field.
C
	CALL DSCOLR ( iwht, j1, iret )
	CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	CALL DFILL  ( iwndw, np, xr, yr, iret )
C
C*	Draw the dark blue top area.
C
	rx2 = rad * 0.8 * FLOAT ( ispanx )
	ry2 = rad * 0.8 * FLOAT ( ispany )
C
	rx3 = rx2 / 438.
	ry3 = ry2 / 438.
C
	DO  i = 11, 39
	    p = TWOPI * FLOAT ( i ) / FLOAT ( 99 )
	    xr ( i - 10 ) = x + rx2 * COS ( p )
	    yr ( i - 10 ) = y + ry2 * SIN ( p )
	END DO
C
	DO  i = 1, 49
	    ix =       icurve1 ( i ) / 1000   - 500
	    iy = MOD ( icurve1 ( i ),  1000 ) - 500
	    xr ( i + 29 ) = x + rx3 * FLOAT ( ix )
	    yr ( i + 29 ) = y + ry3 * FLOAT ( iy )
	END DO
C
	CALL DSCOLR ( idkb, j1, iret )
	CALL DFILL  ( iwndw, 29 + 49, xr, yr, iret )
C
C*	Draw the light blue bottom area.
C
	DO  i = 18, 78
	    p = TWOPI * FLOAT ( i ) / FLOAT ( 99 )
	    xr ( i - 17 ) = x + rx2 * SIN ( p )
	    yr ( i - 17 ) = y + ry2 * COS ( p )
	END DO
C
	DO  i = 1, 79
	    ix =       icurve2 ( i ) / 1000   - 500
	    iy = MOD ( icurve2 ( i ),  1000 ) - 500
	    xr ( i + 61 ) = x + rx3 * FLOAT ( ix )
	    yr ( i + 61 ) = y + ry3 * FLOAT ( iy )
	END DO
C
	CALL DSCOLR ( iltb, j1, iret )
	CALL DFILL  ( iwndw, 61 + 79, xr, yr, iret )
C
C*	Plot the legends in a circle around the outer edge.
C
	trad  = 0.9 * rad
	tsiz  = rad * 0.012 / bscalc
	itwid = NINT ( rad * 0.02 / bscalc )
	IF ( itwid .lt. 1 ) itwid = 1
C
	CALL DSCOLR ( idkb, j1, iret )
	CALL DSTEXT ( 3, 1, tsiz, itwid, 111, 1, 2,
     +		      j1, j2, s3, j4, j5, j6, j7, iret )
C
	IF  ( itxmod .eq. 1 )  THEN
	    string = 'NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION'
	    CALL DCRVTXT (iwndw, x, y, trad, 90., 75, 0, string, iret)
C
	    string = 'U.S.DEPARTMENT OF COMMERCE'
	    CALL DCRVTXT (iwndw, x, y, trad, 270., 76, 0, string, iret)
	END IF
C
C*	Plot the white "noaa" legend above the seagull.
C
	CALL DSCOLR ( iwht, j1, iret )
C
	DO  i = 1, 23
	    ix =       inshape ( i ) / 1000   - 500
	    iy = MOD ( inshape ( i ),  1000 ) - 500
	    xr ( i ) = x + rx3 * FLOAT ( ix )
	    yr ( i ) = y + ry3 * FLOAT ( iy )
	END DO
C
	CALL DFILL  ( iwndw, 22, xr, yr, iret )
C
	DO  i = 1, 57
	    ix =       ioshape ( i ) / 1000   - 500
	    iy = MOD ( ioshape ( i ),  1000 ) - 500
	    xr ( i ) = x + rx3 * FLOAT ( ix )
	    yr ( i ) = y + ry3 * FLOAT ( iy )
	END DO
C
	CALL DFILL  ( iwndw, 57, xr, yr, iret )
C
	DO  i = 1, 51
	    ix =       iashape ( i ) / 1000   - 500
	    iy = MOD ( iashape ( i ),  1000 ) - 500
	    xr ( i ) = x + rx3 * FLOAT ( ix )
	    yr ( i ) = y + ry3 * FLOAT ( iy )
	END DO
C
	CALL DFILL  ( iwndw, 51, xr, yr, iret )
C
	DO  i = 1, 51
	    ix = iashape ( i ) / 1000 - 500 + 94
	    xr ( i ) = x + rx3 * FLOAT ( ix )
	END DO
C
	CALL DFILL  ( iwndw, 51, xr, yr, iret )
C
C*	Restore current attributes.
C
	CALL DSCOLR ( icolr, j1, iret )
	CALL DSTEXT ( itxfn, itxhw, sztxt, itxwd, ibrdr, irrotn, ijust,
     +		      j1, j2, s3, j4, j5, j6, j7, iret )
	CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
C*
	RETURN
	END
