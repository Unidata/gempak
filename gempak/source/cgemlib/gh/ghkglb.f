	SUBROUTINE GH_KGLB ( timstr, type, sname, sped, adnm, wind,
     +                       alat, alon, iflags, rmnlat, rmnlon, rmxlat,
     +                       rmxlon, tzone, iwht, iblk, idisp, namstr,
     +                       poslb, xkey, ykey, nknt, wdir, jflags, 
     +			     disstr, zone2, iret )
C************************************************************************
C* GH_KGLB								*
C*									*
C* This subroutine draws the legend box (key) and plots the storm       *
C* type/name string.  It also creates and plots the date, time and day, *
C* advisory number, current  location, maximum wind speed, current      *
C* movement, marker keys, and track label strings (if any) for the      *
C* legend box, and plots the logos.                                     *
C*									*
C* GH_KGLB ( TIMSTR, TYPE, SNAME, SPED, ADNM, WIND, ALAT, ALON, IFLAGS, *
C*	     RMNLAT, RMNLON, RMXLAT, RMXLON, TZONE, IWHT, IBLK, IDISP,  *
C*           NAMSTR, POSLB, XKEY, YKEY, NKNT, WDIR, JFLAGS, DISSTR,     *
C*	     ZONE2, IRET )						*
C*									*
C* Input parameters:							*
C*	TIMSTR		CHAR*		Ending time for watches		*
C*	TYPE		CHAR*		Storm type			*
C*	SNAME		CHAR*		Storm name			*
C*	SPED		CHAR*		Current movement in kts		*
C*	ADNM		CHAR*		Storm advisory number		*
C*	WIND		CHAR*		Max sustained Wind in kts	*
C*	ALAT (*)	REAL		Current latitude		*
C*	ALON (*)	REAL		Current longitude		*
C*      IFLAGS(4)	INTEGER		Flags for storm watch/warning   *
C*	RMNLAT 		REAL		Minimum y axis map value	*
C*	RMNLON 		REAL		Minimum x axis map value	*
C*	RMXLAT 		REAL		Maximum y axis map value	*
C*	RMXLON 		REAL		Maximum x axis map value	*
C*      TZONE		CHAR*		Storm's current time zone	*
C*	IWHT		INTEGER  	White color value		*
C*	IBLK		INTEGER		Black color value		*
C*	IDISP		INTEGER		Storm dissipation flag		*
C*	NAMSTR		CHAR*		Storm type/name string          *
C*	POSLB		CHAR*		Flag for label/logo position    *
C*	XKEY (*)	REAL		Norm. x coords of legend box    *
C*	YKEY (*)	REAL		Norm. y coords of legend box    *
C*      NKNT		INTEGER         Number of forecast points	*
C*      WDIR		CHAR*           Direction of present movement   *
C*      JFLAGS(3)	INTEGER		Flags for forecast intensity    *
C*									*
C* Output parameters:							*
C*	DISSTR		CHAR*		Storm dissipation time string	*
C*	ZONE2		CHAR*		Local time zone                 *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01   					*
C* A. Hardy/GSC		 6/01	Added diss. check and label pos. parms  *
C* A. Hardy/GSC		 6/01	Added color tag; GH_SAVE, GH_REST	*
C* A. Hardy/SAIC	 8/01	Added TD type;widen box;add text,lines  *
C* A. Hardy/SAIC	10/01	Added current lat/lon to GH_KGIP	*
C* D. Kidwell/NCEP	 4/02	Removed code for label box placement;   *
C*			        default marker color 20->17             *
C* A. Hardy/NCEP	 8/02   Set 115 kts wind to 135 mph		*
C* J. Wu/SAIC		 8/03   Added legend for day 4-5 track area	*
C* D. Kidwell/NCEP	11/03   Added output argument zone2             *
C* D. Kidwell/NCEP	 1/04   Added wdir, 'TPC' line & intensity lines*
C* D. Kidwell/NCEP	 4/04   CSC - added jflags processing           *
C* S. Jacobs/NCEP	11/10	Removed "TPC" from the label string	*
C* M. Sardi/NHC	        10/02   Set 115 kts wind to 130 mph		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NUMH2 = 6 )
C*
	CHARACTER*(*)	timstr, type, sname, sped, wind, tzone, disstr,
     +                  adnm, namstr, poslb, zone2, wdir
        REAL		alat(*), alon(*), xkey(*), ykey(*)
        INTEGER		iflags(*), jflags (*)
C*
        CHARACTER       dtstr*40, strtm*20, clat*5, clon*6, 
     +			nslab*1, ewlab*1, curloc*48, maxwin*35,
     +			curmov*40, currlc*24, forpos*28, pttkar*28,
     +			smwd*4, cmov*3, coltag*33, hscolr(4)*12,
     +                  advstr*12, pttkar5*28, attstr*50, cmpdir*3,
     +			fintns(3)*27
      	CHARACTER       fmonth*9, fmon*9, fday*9, cyear*4, cdate*2, 
     +                  ctime*2, ampm*2
	INTEGER		jtarr(5), lenstp(4), ihs(4)
        CHARACTER       flgstr*240, stype(4)*36
	LOGICAL 	baddir
	REAL            xx(5), yy(5), xlin(6), ylin(6), xl(9), yl(9)
C*
	DATA		stype / 'Hurricane Warning', 
     +                          'Hurricane Watch', 
     +                          'Tropical Storm Warning' ,
     +                          'Tropical Storm Watch' /
	DATA		lenstp  / 17, 15, 22, 20 / 
	DATA		hscolr  / 'w_hur_warn', 'w_hur_watch',
     +                            'w_ts_warn', 'w_ts_watch' /
	DATA		ihs     / 2, 11, 4, 20 / 
	DATA		attstr  / 'NWS National Hurricane Center' /
	DATA		fintns  / 'H  Sustained wind > 73 mph ',
     +				  'S  Sustained wind 39-73 mph',
     +				  'D  Sustained wind < 39 mph ' /
C-----------------------------------------------------------------------
      	iret = 0
C
C*	Query attributes.
C
        CALL GH_SAVE ( ier )
C
C*	Create the date and time strings.
C
 	CALL GH_TIME ( timstr, tzone, jtarr, hours, zone2,
     +                 fmonth, fmon, fday, ampm, ier )
C
C*	Create date string.
C
	CALL ST_INCH ( jtarr(1), cyear, ier )
	CALL ST_INCH ( jtarr(3), cdate, ier )
	CALL ST_INCH ( jtarr(4), ctime, ier )
	CALL ST_LSTR( fmonth, lenm, ier )
	CALL ST_LSTR( cyear, leny, ier )
	CALL ST_LSTR( cdate, lend, ier )
	CALL ST_LSTR( ctime, lent, ier )
	CALL ST_LSTR( ampm, lena, ier )
	CALL ST_LSTR( zone2, lenz, ier )
C
	dtstr = fmonth(:lenm) //' '// cdate(:lend) //', '// cyear(:leny)
	strtm = ctime(:lent) // ' ' // ampm(:lena) // ' '
     +          // zone2(:lenz) // ' '// fday
        disstr = ctime(:lent) // ' ' // ampm(:lena) // ' '// 
     +		 zone2(:lenz) // ' ' // fmonth(:lenm) //' '// 
     +		 cdate(:lend)
C
        IF ( ( idisp .eq. 0 ) .or. ( idisp .eq. 2 ) ) THEN
C
C*	    Create the advisory number string.
C
	    CALL ST_LSTR( adnm, lenv, ier )
            advstr = 'Advisory '// adnm (:lenv)
C
C*	    Create the current location string.
C
	    IF ( alat(1) .gt. 0.0) THEN
                nslab = 'N'
              ELSE IF ( alat (1) .lt. 0.0 ) THEN
                nslab = 'S'
              ELSE 
                nslab = ' '
            END IF
C
	    IF ( alon(1) .lt. 0.0) THEN
                ewlab = 'W'
              ELSE IF ( alon (1) .gt. 0.0 ) THEN
                ewlab = 'E'
              ELSE 
                ewlab = ' '
            END IF
            CALL ST_RLCH ( alat(1), 1, clat, ier )
            CALL ST_RLCH ( alon(1), 1, clon, ier )
            IF ( clat(1:1) .eq. '-') clat = clat(2:)
            IF ( clon(1:1) .eq. '-') clon = clon(2:)
            CALL ST_LSTR (clat, leng, ier )
            CALL ST_LSTR (clon, lens, ier )
            CALL ST_LSTR (ewlab, lenw, ier )
            curloc = 'Current Center Location '// 
     +               clat(:leng)// ' ' // nslab //
     +                '  '//clon(:lens) // ' '//ewlab(:lenw)
C
C*	    Create the max sustained wind string. Round wind speed
C*          to the 5 mph. If the wind speed is equal to 115 knots then
C*          it will be automatically set to 130 mph.
C
            CALL ST_LSTR ( wind, lens, ier )
            CALL ST_CRNM ( wind(:lens), fwnd, ier )
            wnd = PR_KNMH ( fwnd )
            IF ( ( wnd .gt. 131.18 ) .and. ( wnd .lt. 133.0 ) ) THEN
                imswnd = 130
              ELSE
                mswnd = NINT ( wnd )
                imswnd = ( mswnd / 5 ) * 5
                IF ( (MOD (mswnd, 5) .ge. 2.5 ) ) imswnd  = imswnd + 5
            END IF
            CALL ST_INCH ( imswnd ,  smwd, ier )  
            CALL ST_LSTR ( smwd, lenx, ier )
	    maxwin = 'Max Sustained Wind  ' // smwd(:lenx) // ' mph'
C
C*	    Create the current movement string.
C
	    baddir = .true.
	    IF ( wdir .ne. ' ' ) THEN
	        CALL ST_CRNM ( wdir, dirmov, ier ) 
	        CALL CLO_COMPASS ( dirmov, cmpdir, leni, ier )
	        IF ( ier .eq. 0 ) baddir = .false.
	    END IF
	    IF ( baddir ) THEN
	        cmpdir = ' '
	        leni   = 1
	    END IF
C		
            CALL ST_CRNM ( sped, fspd, ier )
            spd = PR_KNMH ( fspd )
            mspd = NINT ( spd )
            CALL ST_INCH ( mspd , cmov, ier )  
            CALL ST_LSTR ( cmov, lens, ier )  
            IF (cmov(:lens) .ne. '0' ) THEN
	        curmov = 'Current Movement ' // cmpdir (:leni) // 
     +			 ' at  ' // cmov(:lens) // ' mph'
              ELSE IF (cmov(:lens) .eq. '0' ) THEN
	        curmov = 'Current Movement  Stationary' 
            END IF
C
C*	    Create the current location, forecast positions and 
C*          potential track area strings.
C
	    currlc = 'Current Center Location'
	    forpos = 'Forecast Center Positions'
	    pttkar = 'Potential Day 1-3 Track Area'
	    pttkar5 = 'Potential Day 4-5 Track Area'
C
	    CALL ST_LSTR ( attstr, lenr, ier ) 
            CALL ST_LSTR ( namstr, lenn, ier )  
            CALL ST_LSTR ( dtstr,  lend, ier )  
            CALL ST_LSTR ( strtm,  lens, ier )  
            CALL ST_LSTR ( advstr, lenv, ier )
            CALL ST_LSTR ( curloc, lenc, ier )  
            CALL ST_LSTR ( maxwin, lenm, ier )  
            CALL ST_LSTR ( curmov, lenu, ier )  
            CALL ST_LSTR ( currlc, lenl, ier )  
            CALL ST_LSTR ( forpos, leno, ier )  
            CALL ST_LSTR ( pttkar, lenp, ier )  
C           
	    IF ( nknt .gt. NUMH2 ) THEN
	        CALL ST_LSTR ( pttkar5, lenq, ier )
	    END IF
C
	    CALL GQSYSZ ( rxszmk,ryszmk,rxsztx,rysztx,rxszwb,ryszwb,ier)
C
	    CALL GSCOLR ( iwht, ier )
            CALL GSSMTH ( 0, 1.5, ier )
	    CALL GSFILL ( 1.0, 1, ier )
	    CALL GFILL ( 'N', 5, xkey, ykey, ier )
	    CALL GSCOLR ( iblk, ier )
            CALL GSLINE ( 1, 1, 2, 1, ier )
            CALL GLINE ( 'N', 5, xkey, ykey, ier )
C
	    CALL GSCOLR ( iblk, ier )
C
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 1, ier )
C
C*          Plot storm name line.
C
            xxz   = xkey (1) + .01
            yyz   = ykey (1) - .02
            ixoff = 0
            CALL GTEXT ( 'N', xxz, yyz, namstr(:lenn), 0.0, ixoff, 
     +                   0, ier )
C
C*          Plot date line.
C
            yyz = yyz - .022
            CALL GTEXT ( 'N', xxz, yyz, dtstr(:lend), 0.0, ixoff, 
     +                   0, ier )
C
C*          Plot time line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, strtm(:lens), 0.0, ixoff, 
     +                   0, ier )
C
            CALL GSTEXT ( 22, 2, 1.1, 1, 111, 1, 1, ier )
	    CALL GQSYSZ ( rxszmk,ryszmk,rxsztx,rysztx,rxszwb,ryszwb,ier)
C
C*          Plot attribution line
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, attstr(:lenr), 0.0, ixoff, 
     +                   0, ier )
C
C*          Plot advisory number line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, advstr(:lenv), 0.0, ixoff, 
     +               	 0, ier )
C
C*          Plot lat. lon. line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, curloc(:lenc), 0.0, ixoff, 
     +               	 0, ier )
C
C*          Plot wind line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, maxwin(:lenm), 0.0, ixoff, 
     +                   0, ier )
C
C*          Plot movement line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, curmov(:lenu), 0.0, ixoff, 
     +                   0, ier )
C
C*          Plot current location line.
C
            yyz   = yyz - .02
            ixoff = 12
            CALL GTEXT ( 'N', xxz, yyz, currlc(:lenl), 0.0, ixoff, 
     +                   0, ier )
            sym = 3.
            xzz = xxz + .03
	    CALL GSCOLR ( iblk, ier )
	    CALL GSSPCL ( 1.1, 1, ier )
	    CALL GSPCL ( 'N', 1, sym, xzz, yyz, 0, 0, ier )
	    coltag = 'w_lbl_curr_pos_mkr'
            CALL ST_LSTR ( coltag, lens, ier )
 	    CALL GH_COLR ( coltag(:lens), 17, ier )
	    CALL GSSPCL ( .9, 1, ier )
	    CALL GSPCL ( 'N', 1, sym, xzz, yyz, 0, 0, ier )
            CALL GSCOLR ( iblk, ier )
            CALL GSMRKR ( 17, 0, 0.8, 2, ier )
            CALL GMARK ( 'N', 1, xzz, yyz, ier )
	    CALL GSCOLR ( iblk, ier )
C
C*          Plot forecast position line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, forpos(:leno), 0.0, ixoff, 
     +                   0, ier )
            CALL GSCOLR ( iblk, ier )
            CALL GSMRKR ( 17, 0, 1.4, 2, ier )
            CALL GMARK ( 'N', 1, xzz, yyz, ier )
	    CALL GSCOLR ( iblk, ier )
C
C*          Plot forecast intensity lines.
C
	    ixoff = 15
	    DO ii = 1, 3
		IF ( jflags ( ii ) .gt. 0 ) THEN
                    yyz = yyz - .02
                    CALL GTEXT ( 'N', xxz, yyz, fintns (ii) (:27), 0.0,
     +			         ixoff, 0, ier )
		END IF
	    END DO
C
C*          Plot day 1-3 potential track area line.
C
            ixoff = 12
            yyz   = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, pttkar(:lenp), 0.0, ixoff, 
     +                   0, ier )
            xlin(1) = xxz + .05
            xlin(2) = xlin(1) - .033
            xlin(3) = xxz
            xlin(4) = xxz
            xlin(5) = xlin(4) + .010
            xlin(6) = xlin(1)
C
            ylin(1) = yyz - .006
            ylin(2) = ylin(1)
            ylin(3) = ylin(2) + .005
            ylin(4) = ylin(3) + .01
            ylin(5) = ylin(4) + .005
            ylin(6) = ylin(1)
C
            CALL GSLINE ( 1, 1, 2, 1, ier )
            CALL GLINE ( 'N', 6, xlin, ylin, ier )
C
C*          Plot and fill day 4-5 potential track area line.
C
	    IF ( nknt .gt. NUMH2 ) THEN
		yyz = yyz - .02
                CALL GTEXT ( 'N', xxz, yyz, pttkar5(:lenq), 0.0, ixoff, 
     +                       0, ier )
                xl(1) = xxz + .04
                xl(2) = xl(1) - .025
                xl(3) = xxz + 0.005
                xl(4) = xl(3)
                xl(5) = xl(4) + .01
                xl(6) = xl(5) + .025
                xl(7) = xl(6) - .01
                xl(8) = xl(7)
                xl(9) = xl(1)
C
                yl(1) = yyz - .008
                yl(2) = yl(1)
                yl(3) = yl(2) + .004
                yl(4) = yl(3) + .008
                yl(5) = yl(4) + .004
                yl(6) = yl(5)
                yl(7) = yl(6) - .006
                yl(8) = yl(7) - .004
                yl(9) = yl(1)
C
                CALL GQFILL ( szfil, iftyp, ier )
C
                CALL GSFILL ( 1.0, 2, ier )
                CALL GSLINE ( 1, 1, 2, 1, ier )
                CALL GLINE ( 'N', 9, xl, yl, ier )
     	        CALL GFILL ( 'N', 9, xl, yl, ier )
C
                CALL GSFILL ( szfil, iftyp, ier )
C
	    END IF	    
C
C*	    Plot active hurricane /T.S. watches and warnings text lines.
C
            DO is = 1,4
 	        IF ( iflags (is) .ne. 0 ) THEN
                    flgstr =   stype(is)(:lenstp(is))
                    CALL ST_LSTR (flgstr, lens, ier )
                    yyz = yyz - .02
                    CALL GTEXT ( 'N', xxz, yyz, flgstr(:lens), 0.0, 
     +                            ixoff, 0, ier )
C
C*	            Plot color legend for tropical storm.
C
                    xx (1) = xxz +.04
                    xx ( 2 ) = xx ( 1 ) - .033
	            xx ( 3 ) = xx ( 2 )
	            xx ( 4 ) = xx ( 1 )
	            xx ( 5 ) = xx ( 1 )
 	            yy ( 1 ) = yyz -.008
	            yy ( 2 ) = yy ( 1 )
                    IF ( is .lt. 3 ) yy ( 3 ) = yy ( 1 ) + .015
                    IF ( is .ge. 3 ) yy ( 3 ) = yy ( 1 ) + .010
	            yy ( 4 ) = yy ( 3 )
	            yy ( 5 ) = yy ( 1 )
C
                    coltag = hscolr (is)
                    CALL ST_LSTR ( coltag, lens, ier )
                    CALL GH_COLR ( coltag(:lens), ihs(is), ier )
C
	            CALL GSFILL ( 1.0, 1, ier )
	            CALL GFILL ( 'N', 5, xx, yy, ier )
	            CALL GSCOLR ( iblk, ier )
                END IF
            END DO
        END IF
C
C*      Plot logos opposite of text box label
C
        IF ( poslb .eq. 'B' ) THEN
             xnwx = rmnlon + .09
             ynwx = rmxlat - .28
             xnoaa = rmxlon + .13
             ynoaa = rmxlat - .28
            
          ELSE 
             xnwx = rmnlon + .09
             ynwx = rmnlat + .07
             xnoaa = rmxlon + .13
             ynoaa = rmnlat + .07
        END IF
C
C*      Plot NWS logo.
C
	CALL GLOGO ( 'N', xnwx, ynwx, 2.5, 2, 2, ier )
C
C*      Plot NOAA logo.
C
        CALL GLOGO ( 'N', xnoaa, ynoaa, 2.5, 2, 1, ier )
C
C*	Restore attributes.
C
        CALL GH_REST ( ier )
C*
	RETURN
	END
