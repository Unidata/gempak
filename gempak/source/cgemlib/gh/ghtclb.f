	SUBROUTINE GH_TCLB ( timstr, type, sname, sped, adnm, wind,
     +                       alat, alon, rmnlat, rmnlon, rmxlat,
     +                       rmxlon, tzone, iwht, iblk, idisp, namstr,
     +                       xkey, ykey, nknt, wdir, idays, iscale,
     +			     disstr, iret )
C************************************************************************
C* GH_TCLB								*
C*									*
C* This subroutine draws the legend box (key) and plots the storm       *
C* type/name string.  It also creates and plots the date, time and day, *
C* advisory number, current  location, maximum wind speed, current      *
C* movement, marker keys, and track label strings (if any) for the      *
C* legend box, and plots the logos.                                     *
C*									*
C* GH_TCLB ( TIMSTR, TYPE, SNAME, SPED, ADNM, WIND, ALAT, ALON,         *
C*	     RMNLAT, RMNLON, RMXLAT, RMXLON, TZONE, IWHT, IBLK, IDISP,  *
C*           NAMSTR, XKEY, YKEY, NKNT, WDIR, ISCALE, DISSTR, IRET )     *
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
C*	RMNLAT 		REAL		Minimum y axis map value	*
C*	RMNLON 		REAL		Minimum x axis map value	*
C*	RMXLAT 		REAL		Maximum y axis map value	*
C*	RMXLON 		REAL		Maximum x axis map value	*
C*	TZONE		CHAR*		Storm current time zone		*
C*	IWHT		INTEGER  	White color value		*
C*	IBLK		INTEGER		Black color value		*
C*	IDISP		INTEGER		Storm dissipation flag		*
C*	NAMSTR		CHAR*		Storm type/name string          *
C*	XKEY (*)	REAL		Norm. x coords of legend box    *
C*	YKEY (*)	REAL		Norm. y coords of legend box    *
C*	NKNT		INTEGER         Number of forecast points	*
C*	WDIR		CHAR*           Direction of present movement   *
C*      IDAYS           INTEGER         3 or 5 day forecast display     *
C*      ISCALE		INTEGER		Flag to plot scale legend	*
C*									*
C* Output parameters:							*
C*	DISSTR		CHAR*		Storm dissipation time string	*
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
C* D. Kidwell/NCEP       4/04   CSC - added jflags processing           *
C* m.gamazaychikov/SAIC	03/04	From GH_KGLB				*
C* T. Piper/SAIC	12/04	Added GG_SCAL				*
C* T. Piper/SAIC	01/05	Added title and mask color		*
C* m.gamazaychikov/SAIC	03/05	Added ilegend to make scale box optional*
C* S. Gilbert/NCEP	07/05	Moved scale legend to top of graphic	*
C* S. Gilbert/NCEP	02/06	Recognize intermediate advisories       *
C* S. Gilbert/NCEP	06/06	Added check for missing storm speed     *
C* S. Gilbert/NCEP	07/06	Added check for CPHC                    *
C* m.gamazaychikov/SAIC	04/08	Added iextra for extratropical storms,	*
C*				added string for two new categories,
C*				rearranged lines in the legend box	*
C* X. Guo/CWS		02/10   Changed extratropical to post-tropical  *
C* X. Guo/CWS		03/10   Draw the legend box underneath the graph*
C*                              and removed unused parameters           *
C* X. Guo/CWS		05/10   Changed "Current" to "Center"           *
C* X. Guo/CWS		05/10   Check idays flag to display 4-5 day     *
C*                              forecast legend                         *
C* S. Jacobs/NCEP	 6/10	Re-added flag for plotting the scale	*
C* S. Jacobs/NCEP	 8/10	Added minutes to the time, if not 00	*
C* S. Jacobs/NCEP	11/10	Removed "TPC" from the label		*
C* M. Sardi/NHC	        10/02   Set 115 kts wind to 130 mph		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NUMH2 = 6 )
C*
	CHARACTER*(*)	timstr, type, sname, sped, wind, tzone, disstr,
     +			adnm, namstr,  wdir
	REAL		alat(*), alon(*), xkey(*), ykey(*)
C*
	CHARACTER	dtstr*50, strtm*60, clat*5, clon*6, 
     +			nslab*1, ewlab*1, curloc*48, maxwin*35,
     +			curmov*40, forpos*28, pttkar*28,
     +			smwd*4, cmov*3, coltag*33, hscolr(4)*12,
     +			advstr*27, pttkar5*28, attstr*50, cmpdir*3,
     +			fintns*50, title*48, maskc*3,ptastr*50,
     +			tropcyc*22, etropcyc*22,currinfo*24,swind*40
      	CHARACTER	fmonth*9, fmon*9, fday*9, cyear*4, cdate*2, 
     +			ctime*4, ampm*2, zone2*3, mscale*100,
     +                  watchstr*15,warnstr*15,poslb, chour*2, cmin*2
	INTEGER		jtarr(5), lenstp(2), ihs(4)
	CHARACTER       flgstr*240, stype(2)*36, fgc*3, bgc*3,
     +			dsclmer*240, dsclm1*100, dsclm2*100
	LOGICAL 	baddir
	REAL		xx(5), yy(5), xlin(6), ylin(6), xl(9), yl(9)
C*
	DATA		stype / 'Hurricane', 
     +                          'Trop.Storm' /
	DATA		lenstp  / 9, 10 / 
	DATA		hscolr  / 'w_hur_watch', 'w_ts_watch',
     +                            'w_hur_warn', 'w_ts_warn' /
	DATA		ihs     / 11, 20, 2, 4 / 
	DATA	title / 'Approx. Distance Scale ( Statute Miles )' /
	DATA    dsclm1(:41) /
     +			'Note: The cone contains the probable path' /
	DATA    dsclm1(42:) /
     +			 ' of the storm center but does not show' /
	DATA    dsclm2(:32) /
     +			 'the size of the storm. Hazardous' /
	DATA    dsclm2(33:) /
     +			 ' conditions can occur outside of the cone.' /
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
	CALL ST_INCH ( jtarr(4), chour, ier )
	CALL ST_INCH ( jtarr(5), cmin, ier )
	CALL ST_LSTR ( chour, lenh, ier )
C
C*	Add the minutes only if they are not 00.
C
	IF  ( jtarr(5) .ne. 0 )  THEN
	    IF  ( jtarr(5) .lt. 10 )  THEN
		ctime = chour(:lenh) // '0' // cmin
	    ELSE
		ctime = chour(:lenh) // cmin
	    END IF
	ELSE
	    ctime = chour
	END IF
C
	CALL ST_LSTR ( fmonth, lenm, ier )
	CALL ST_LSTR ( cyear, leny, ier )
	CALL ST_LSTR ( cdate, lend, ier )
	CALL ST_LSTR ( ctime, lent, ier )
	CALL ST_LSTR ( ampm, lena, ier )
	CALL ST_LSTR ( zone2, lenz, ier )
C
	dtstr  = fday // ' ' // fmonth(:lenm) //' '
     +        // cdate(:lend) //', '// cyear(:leny) // ' '
        strtm = ctime(:lent) // ' ' // ampm(:lena) // ' '
     +          // zone2(:lenz)
        disstr = ctime(:lent) // ' ' // ampm(:lena) // ' '// 
     +		 zone2(:lenz) // ' ' // fmonth(:lenm) //' '// 
     +		 cdate(:lend)
C
C*	Plot Scale legend.
C
	IF  ( iscale .eq. 1 )  THEN
	    CALL GQTEXT ( ktxfn, ktxhw, tztext, ktxwid,
     +			  kbrdr, krrotn, kjust, ier )
	    CALL GQCOLR ( kcolr, ier )
C
	    CALL GSCOLR ( iwht, ier )
	    CALL GSTEXT ( 22, 2, 1.0, 1, 221, 1, 2, ier )
	    CALL ST_LSTR ( dsclm1, len1, ier )
	    dsclmer = dsclm1(:len1) // CHCR // dsclm2
	    CALL GTEXT ( 'N', 0.5, 0.78, dsclmer, 0.0, 0, 0, ier )
C
	    CALL GSTEXT ( ktxfn, ktxhw, tztext, ktxwid,
     +			  kbrdr, krrotn, kjust, ier )
	    CALL GSCOLR ( kcolr, ier )
C-----------------------------------------------
C	    CALL ST_INCH ( iblk, fgc, ier )
C	    CALL ST_INCH ( iwht, bgc, ier )
C	    IF ( iblk .eq. 32 )  THEN
C	      maskc = '1'
C	    ELSE
C	      maskc = '31'
C	    END IF
C
C*	    Put Scale legend Top Center
C
C	    mscale = fgc // ';' // bgc // ';' // maskc //
C     +	    		'/SM/30/125/UC/.5;0.95/.7/-1|Medium|' // title
C	    CALL GSSMTH ( 0, 1.5, ier )
C	    CALL GG_SCAL ( mscale, ier )
C-----------------------------------------------
	END IF
C
	IF ( ( idisp .eq. 0 ) .or. ( idisp .eq. 2 ) ) THEN
C
C*	    Create the advisory number string.
C
            CALL ST_NULL ( adnm, adnm, lenv, ier )
            CALL GH_ADVN ( adnm, iadnum, iaflag, ier )
            IF ( iaflag .eq. 0 ) THEN
                advstr = 'Advisory '// adnm (:lenv)
            ELSE
                advstr = 'Intermediate Advisory '// adnm (:lenv)
            ENDIF
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
C
C*          Recompute LOGO position top/bottom
C
            poslb = 'T'
            ddiff = 0.06
            dxdiff = ddiff + .08
            dydiff = ddiff + .07
            CALL GTRANS ( 'M', 'V', 1, alat(1),  alon(1), dcurlat,
     +                    dcurlon, ier )
            IF (dcurlon .le. (rmnlon + dydiff)) THEN
                IF ( dcurlat .le. dxdiff) THEN
                    poslb = 'B'
                  ELSE
                      IF (dcurlat .ge. (rmxlat-dxdiff)) THEN
                          poslb = 'B'
                      END IF
                END IF
            END IF
            CALL ST_RLCH ( alat(1), 1, clat, ier )
	    CALL ST_RLCH ( alon(1), 1, clon, ier )
	    IF ( clat(1:1) .eq. '-') clat = clat(2:)
	    IF ( clon(1:1) .eq. '-') clon = clon(2:)
	    CALL ST_LSTR (clat, leng, ier )
	    CALL ST_LSTR (clon, lens, ier )
	    CALL ST_LSTR (ewlab, lenw, ier )
            curloc = 'Center Location '// 
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
	    IF ( sped .eq. '-9999' ) THEN
	        curmov = 'Movement  -' 
	    ELSE IF (cmov(:lens) .ne. '0' ) THEN
	        curmov = 'Movement ' // cmpdir (:leni) // 
     +			 ' at  ' // cmov(:lens) // ' mph'
	    ELSE IF (cmov(:lens) .eq. '0' ) THEN
	        curmov = 'Movement  Stationary' 
	    END IF
C
C*	    Create the current location, forecast positions and 
C*	    potential track area strings.
C
            currinfo = 'Current Information:'
            swind = 'Sustained Winds:        D  < 39 mph'
            fintns = 'S  39-73 mph  H  74-110 mph  M  > 110mph'
	    forpos = 'Forecast Positions:'
            ptastr = 'Potential Track Area:'
	    pttkar = 'Day 1-3'
	    pttkar5 = 'Day 4-5'
            watchstr = 'Watches:'
            warnstr= 'Warnings:'
            attstr = 'NWS National Hurricane Center'
            tropcyc = 'Tropical Cyclone'
            etropcyc = 'Post-Tropical'
            IF ( tzone .eq. 'H' ) THEN
                 attstr = 'NWS Central Pacific Hurricane Center' 
            ENDIF
C
	    CALL ST_LSTR ( attstr, lenr, ier ) 
	    CALL ST_LSTR ( namstr, lenn, ier )  
	    CALL ST_LSTR ( dtstr,  lend, ier )  
	    CALL ST_LSTR ( advstr, lenv, ier )
	    CALL ST_LSTR ( curloc, lenc, ier )  
	    CALL ST_LSTR ( maxwin, lenm, ier ) 
            CALL ST_LSTR ( ptastr, lenpta, ier )  
	    CALL ST_LSTR ( curmov, lenu, ier )  
            CALL ST_LSTR ( currinfo, lenif, ier )  
            CALL ST_LSTR ( swind, lensw, ier )
            CALL ST_LSTR ( fintns, lenfi, ier )
	    CALL ST_LSTR ( forpos, leno, ier )  
	    CALL ST_LSTR ( pttkar, lenp, ier )  
	    CALL ST_LSTR ( strtm,  lens, ier )  
	    CALL ST_LSTR ( tropcyc,  lentr, ier )  
	    CALL ST_LSTR ( etropcyc, lenet, ier )  
C           
            strtm = strtm(:lens)//' '//advstr(:lenv)
	    CALL ST_LSTR ( strtm,  lens, ier )  
	    CALL ST_LSTR ( pttkar5, lenq, ier )
            CALL ST_LSTR ( watchstr, lenwc, ier )
            CALL ST_LSTR ( warnstr, lenwn, ier )
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
	    yyz   = ykey (1) - .015
	    CALL GTEXT ( 'N', xxz, yyz, namstr(:lenn), 0.0, 0, 
     +                   0, ier )
C
C*          Plot date line.
C
	    CALL GSTEXT ( 33, 2, 1.1, 1, 111, 1, 1, ier )
	    yyz = yyz - .022
	    CALL GTEXT ( 'N', xxz, yyz, dtstr(:lend), 0.0, 0, 
     +                   0, ier )
C
C*          Plot time line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, strtm(:lens), 0.0, 0, 
     +                   0, ier )
C
C*          Plot attribution line
C
            CALL GSTEXT ( 22, 2, 1.1, 1, 111, 1, 1, ier )
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, attstr(:lenr), 0.0, 0, 
     +                   0, ier )
C
C*          Plot lat. lon. line.
C
            xxz   = xkey (1) + .35
            IF ( tzone .eq. 'H' ) xxz = xxz + .03
            yyz   = ykey (1) - .015
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 1, ier )
            CALL GTEXT ( 'N', xxz, yyz, currinfo(:lenif), 0.0, 0,
     +                   0, ier )
            sym = 3.
            xzz = xxz + .22
            CALL GSCOLR ( iblk, ier )
            CALL GSSPCL ( 1.0, 1, ier )
            CALL GSPCL ( 'N', 1, sym, xzz, yyz, 0, 0, ier )
            coltag = 'w_lbl_curr_pos_mkr'
            CALL ST_LSTR ( coltag, lens, ier )
            CALL GH_COLR ( coltag(:lens), 17, ier )
            CALL GSSPCL ( .8, 1, ier )
            CALL GSPCL ( 'N', 1, sym, xzz, yyz, 0, 0, ier )
            CALL GSCOLR ( iblk, ier ) 
            CALL GSMRKR ( 17, 0, 0.8, 2, ier )
            CALL GMARK ( 'N', 1, xzz, yyz, ier )
            CALL GSCOLR ( iblk, ier )

            CALL GSTEXT ( 22, 2, 1.1, 1, 111, 1, 1, ier )
            yyz = yyz - .022
            CALL GTEXT ( 'N', xxz, yyz, curloc(:lenc), 0.0, 0, 
     +               	 0, ier )
C
C*          Plot wind line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, maxwin(:lenm), 0.0, 0, 
     +                   0, ier )
C
C*          Plot movement line.
C
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, curmov(:lenu), 0.0, 0, 
     +                   0, ier )
C
C*          Plot forecast position line.
C
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 1, ier )
            xxz   = xkey(1) + .67      
            yyz   = ykey (1) - .0155555      
            xzz = xxz
            
	    CALL GTEXT ( 'N', xxz, yyz, forpos(:leno), 0.0, 0, 
     +                   0, ier )
            CALL GSTEXT ( 22, 2, 1.1, 1, 111, 1, 1, ier )
            xxz = xxz + 0.02
	    yyz = yyz - .022
	    CALL GTEXT ( 'N', xxz, yyz, tropcyc(:lentr), 0.0, 0, 
     +                   0, ier )
	    CALL GSCOLR ( iblk, ier )
	    CALL GSMRKR ( 17, 0, 1.8, 2, ier )
	    CALL GMARK ( 'N', 1, xzz, yyz, ier )
	    CALL GSCOLR ( iblk, ier )
	    xzz = xxz + .15
            xxz = xxz + .17
	    CALL GTEXT ( 'N', xxz, yyz, etropcyc(:lenet), 0.0, 0, 
     +                   0, ier )
	    CALL GSCOLR ( iblk, ier )
	    CALL GSMRKR ( 17, 0, 1.8, 2, ier )
	    CALL GMARK ( 'N', 1, xzz, yyz, ier )
	    CALL GSCOLR ( iwht, ier )
	    CALL GSMRKR ( 17, 0, 1.4, 2, ier )
	    CALL GMARK ( 'N', 1, xzz, yyz, ier )
	    CALL GSCOLR ( iblk, ier )
C
C*          Plot forecast intensity lines.
C
            xxz = xkey(1) + 0.67
            yyz = yyz - .022
            CALL GTEXT ( 'N', xxz, yyz, swind(:lensw), 0.0, 0,
     +                   0, ier )
            
            yyz = yyz - .02
            CALL GTEXT ( 'N', xxz, yyz, fintns (:lenfi) , 0.0,
     +                           0, 0, ier )

C
C*          Draw one line
C
            xlin(1) = 0.0
            xlin(2) = 1.0
            ylin(1) = yyz - .01
            ylin(2) = yyz - .01

            CALL GLINE ( 'N', 2, xlin, ylin, ier )
C
C*          Potential Track Area
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 1, ier )
            xxz = xkey (1) + .01 
            yyz   = yyz - .03
            CALL GTEXT ( 'N', xxz, yyz, ptastr(:lenpta), 0.0, 0,
     +                   0, ier )

C
C*          Plot day 1-3 potential track area line.
C
            CALL GSTEXT ( 22, 2, 1.1, 1, 111, 1, 1, ier )
            ixoff = 12
            yyz   = yyz - .03
            IF (  idays .ne. 3 ) THEN
                    CALL GTEXT ( 'N', xxz, yyz, pttkar(:lenp), 
     +                            0.0, ixoff,0, ier )
	            xlin(1) = xxz + .05
	            xlin(2) = xlin(1) - .033
	            xlin(3) = xxz
                ELSE
                    CALL GTEXT ( 'N', xxz + 0.04, yyz, pttkar(:lenp), 
     +                            0.0, ixoff,0, ier )
                    xlin(1) = xxz + .09
                    xlin(2) = xlin(1) - .033
                    xlin(3) = xxz + .04
            END IF
	    xlin(4) = xlin(3)
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
            IF ( idays .ne. 3 ) THEN
                xxz = xxz + .12
                CALL GTEXT ( 'N', xxz, yyz, pttkar5(:lenq), 0.0,
     +                      ixoff, 0, ier )
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
            END IF
C
C*          Draw watches
C
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 1, ier ) 
            xxz = xkey (1) + .35
            yyz   = yyz + .03
            CALL GTEXT ( 'N', xxz, yyz, watchstr(:lenwc), 0.0, 0,
     +                   0, ier )

C
C*          Plot active hurricane /T.S. watches text lines.
C
            CALL GSTEXT ( 22, 2, 1.1, 1, 111, 1, 1, ier )
            yyz = yyz - .03
            DO is = 1,2
               flgstr =   stype(is)(:lenstp(is))
               CALL ST_LSTR (flgstr, lens, ier )
               xxz = xxz + .06
               if ( is .eq. 2 ) xxz = xxz + .08
               CALL GTEXT ( 'N', xxz, yyz, flgstr(:lens), 0.0,
     +                            0, 0, ier )
C
C*             Plot color legend for tropical storm.
C
               xx (1) = xxz - .01
               xx ( 2 ) = xx ( 1 ) - .05
               xx ( 3 ) = xx ( 2 )
               xx ( 4 ) = xx ( 1 )
               xx ( 5 ) = xx ( 1 )
               yy ( 1 ) = yyz -.008
               yy ( 2 ) = yy ( 1 )
               yy ( 3 ) = yy ( 1 ) + .02
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
            END DO
C
C*          Draw warnings
C
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 1, ier )
            xxz = xkey (1) + .67
            yyz   = yyz + .03
            CALL GTEXT ( 'N', xxz, yyz, warnstr(:lenwn), 0.0, 0,
     +                   0, ier )
           
C
C*	    Plot active hurricane /T.S. watches and warnings text lines.
C
            CALL GSTEXT ( 22, 2, 1.1, 1, 111, 1, 1, ier )
            yyz = yyz - .03

            DO is = 3,4
               flgstr =   stype(is-2)(:lenstp(is-2))
               CALL ST_LSTR (flgstr, lens, ier )
               xxz = xxz + .06
               if ( is .eq. 4 ) xxz = xxz + .08
               CALL GTEXT ( 'N', xxz, yyz, flgstr(:lens), 0.0,
     +                            0, 0, ier )
C
C*             Plot color legend for tropical storm.
C
               xx (1) = xxz - .01
               xx ( 2 ) = xx ( 1 ) - .05
               xx ( 3 ) = xx ( 2 )
               xx ( 4 ) = xx ( 1 )
               xx ( 5 ) = xx ( 1 )
               yy ( 1 ) = yyz -.008
               yy ( 2 ) = yy ( 1 )
               yy ( 3 ) = yy ( 1 ) + .02
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
            END DO
        END IF
C
C*      Plot logos opposite of text box label
C
        IF ( poslb .eq. 'B' ) THEN
             xnwx = rmnlat + .08
             ynwx = rmxlon - .07
             xnoaa = rmxlat - .08
             ynoaa = rmxlon - .07

          ELSE
             xnwx = rmnlat + .08
             ynwx = rmnlon + .07
             xnoaa = rmxlat - .08
             ynoaa = rmnlon + .07
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
