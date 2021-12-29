	SUBROUTINE GH_SWTH ( advno, wtype, timstr, wname, origc, wocen, 
     +                       wadnm, rlat, rlon, windft, nl, nstrm, iret)
C************************************************************************
C* GH_SWTH								*
C*									*
C* This subroutine plots the current tropical storm or hurricane        *
C* wind swath graphic.							*
C*									*
C* GH_SWTH ( ADVNO, WTYPE, TIMSTR, WNAME, ORIGC, WOCEN, WADNM, RLAT,   	*
C*           RLON, WINDFT, NL, NSTRM, IRET )         			*
C*									*
C* Input parameters:							*
C*    	ADVNO		CHAR*		Latest advisory number		*
C* 	WTYPE (*)	CHAR*		Tropical storm type		*
C*	TIMSTR (*)	CHAR*		Advisory valid time string	*
C*	WNAME (*)	CHAR*		Tropical storm name		*
C*	ORIGC (*)	CHAR*		Issuing center			*
C*     	WOCEN (*)	CHAR*		Ocean identifier		*
C*	WADNM (*)	CHAR*		Advisory number			*
C*	RLAT (NL,*) 	REAL		Current/forecasted Latitudes	*
C*	RLON (NL,*) 	REAL		Current/forecasted Longitudes	*
C*	WINDFT (NW,*) 	CHAR*		Current wind/seas radii (nm)	*
C*	NL 		INTEGER		Maximum number of lat/lon pairs *
C*	NSTRM 		INTEGER		Number of decoded files		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01   Modified from GH_HRCN			*
C* A. Hardy/GSC		 5/01   Added GSLINE,GQLINE;changed garea calc. *
C* A. Hardy/GSC		 6/01   Added color tag; GH_REST,GH_SAVE	*
C* D. Kidwell/NCEP	 8/01   Cleaned up                              *
C* A. Hardy/SAIC         8/01   Fixed missing lat/lon bad garea problem *
C* A. Hardy/SAIC        10/01   Modified center label box location	*
C* D. Kidwell/NCEP	 4/02   Checked for large radii; added IP_SVAR  *
C* D. Kidwell/NCEP	 5/02   Changed name of land bounds file        *
C* T. Piper/SAIC	12/04	Added GG_SCAL				*
C* T. Piper/SAIC	01/05	Added title and mask color		*
C* m.gamazaychikov/SAIC	06/06	Changed CS for GH_SWLB			*
C* S. Gilbert/NCEP      07/06   Added new argument origc                *
C* S. Guan/NCEP         07/18   Change hipowo.cia to hipowo.nws         * 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 4, NF = 5000 )
C*
	CHARACTER*(*)	advno, windft(NW,*), wname(*), timstr(*), 
     +                  wtype(*), wocen(*), wadnm(*), origc(*)
	REAL		rlat(nl,*), rlon(nl,*)
C*
	CHARACTER	anum*2, tnum*2, nostrm*80, latlon*128, cblk*2, 
     +                  bgsea*128, bgland*128, mapstr*128, ddev*12,
     +                  swthbg*180, bglake*128, coltag*33, cmap*3,
     +			mscale*100, title*48
        REAL            alat(200), alon(200), rmax(2), rmin(2)
C*
        INTEGER		ise(2,NF), ine(2,NF), inw(2,NF), isw(2,NF)
	INTEGER		iarr(5) 
        LOGICAL         hvhur, hvts, mapok
C*
	INCLUDE		'ERMISS.FNC'
C*
	DATA    title / 'Approx. Distance Scale ( Statute Miles )' /

C-----------------------------------------------------------------------
	iret = 0
	mapok = .true.
	hvhur = .false. 
	hvts = .false. 
	mscale = '32;6;1/SM/30/125/CC/.5;.07|Medium|' // title
C
C*	Query current settings.
C
	CALL GH_SAVE ( ier )
C
	coltag = 's_lat_lon_lines'
	CALL ST_LSTR ( coltag, lens, ier )
	CALL GH_COLR ( coltag(:lens), 26, ier)
	CALL GQCOLR ( icolr, ier)
	CALL ST_INCH ( icolr, cmap, ier )
	CALL ST_LSTR ( cmap, lens, ier )
	latlon = cmap(:lens) // '/1/1//5;5//2'
C
C*      Reverse black and white if device is for postscript.
C
        CALL GQDEV  ( ddev, iunit, iatyp, ier )
        IF  ( ddev(:2) .eq. 'PS' )  THEN
                iwht = 32
                iblk = 1
              ELSE
                iwht = 31
                iblk = 32
        END IF
C
C*	Set symbol and text attributes.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
C*      Fill out the 34kt and 64kt quadrant arrays.
C
        in = 1
	DO ip = 1, nstrm
            alat(in) = rlat(1,ip)
            alon(in) = rlon(1,ip)
C
C*          Determine graphics area.
C
            IF ( in .eq. 1 ) THEN
                rmax(1) = alat(in) + 7.5
                rmin(1) = alat(in) - 5.0
                rmax(2) = alon(in) + 5.0
                rmin(2) = alon(in) - 5.0
              ELSE IF ( ( .not. ERMISS ( alat(in) ) ) .and.
     +                  ( .not. ERMISS ( alon(in) ) ) ) THEN
                rmax(1) = AMAX1 ( rmax(1), ( alat(in) + 7.5 ) )
                rmin(1) = AMIN1 ( rmin(1), ( alat(in) - 5.0 ) )
                rmax(2) = AMAX1 ( rmax(2), ( alon(in) + 5.0 ) )
                rmin(2) = AMIN1 ( rmin(2), ( alon(in) - 5.0 ) )
            END IF
C
            DO iw = 1,4
                CALL ST_ILST ( windft(iw,ip), ' ', IMISSD,
     +                             5, iarr, nwnd, ier)
                IF ( iarr(1) .eq. 64 ) THEN
                    indx = 2
                  ELSE IF ( iarr(1) .eq. 34 ) THEN
                    indx = 1
		  ELSE
		    indx = 0
                END IF
                IF ( indx .gt. 0 ) THEN
                    IF ( iarr(2) .ne. IMISSD ) THEN
                        ise(indx,in) = iarr( 3 )
                        ine(indx,in) = iarr( 2 )
                        inw(indx,in) = iarr( 5 )
                        isw(indx,in) = iarr( 4 )
                        IF ( iarr(1) .eq. 64 ) hvhur = .true.
                        IF ( iarr(1) .eq. 34 ) hvts = .true.
                      ELSE
                        ise(indx,in) = 0
                        ine(indx,in) = 0
                        inw(indx,in) = 0
                        isw(indx,in) = 0
                    END IF
                END IF
            END DO
            in = in + 1
	END DO
        inct = in - 1
C
C*	Check for very large wind radii and expand graphics area.
C
	IF ( inct .gt. 0 ) THEN
	    maxnw = inw ( 1, 1 )
	    maxne = ine ( 1, 1 )
	    maxsw = isw ( 1, 1 )
	    maxse = ise ( 1, 1 )
	    DO in = 2, inct
	        maxnw = MAX0 ( maxnw, inw ( 1, in ) )
	        maxne = MAX0 ( maxne, ine ( 1, in ) )
	        maxsw = MAX0 ( maxsw, isw ( 1, in ) )
	        maxse = MAX0 ( maxse, ise ( 1, in ) )
	    END DO
	    IF ( ( maxnw .ge. 300 ) .or. ( maxne .ge. 300 ) )
     +		 rmax ( 1 ) = rmax ( 1 ) + 5.
	    IF ( ( maxsw .ge. 300 ) .or. ( maxse .ge. 300 ) )
     +		 rmin ( 1 ) = rmin ( 1 ) - 5.
	    IF ( ( maxne .ge. 300 ) .or. ( maxse .ge. 300 ) ) 
     +		 rmax ( 2 ) = rmax ( 2 ) + 5.
	    IF ( ( maxnw .ge. 300 ) .or. ( maxsw .ge. 300 ) ) 
     +		 rmin ( 2 ) = rmin ( 2 ) - 5.
	END IF
C
C*      Set up map and lat/lon lines.
C
        CALL GSMMGN ( 0., 0., 0., 0., ier )
        CALL GSMMAP ('MER', rmin(1), rmin(2), rmax(1), rmax(2), ier)
C
C*      Fill out the margin areas with the rest of the map.
C
        CALL GQBND ( 'N', xmnlat, xmnlon, xmxlat, xmxlon, ier )
        CALL GTRANS ( 'N', 'M', 1, xmnlat, xmnlon, rmnlat,
     +                        rmnlon, ier )
        CALL GTRANS ( 'N', 'M', 1, xmxlat, xmxlon, rmxlat,
     +                        rmxlon, ier )
        CALL GSMMAP ('MER', rmnlat, rmnlon, rmxlat, rmxlon, ier)
C
C*      Check to see if the minimum longitude is west of the dateline.
C*      Find center longitude for label box placement.
C
        IF ( rmnlon .gt. 0 ) THEN
            rstlon = 180.0 - rmnlon
            rmnlon = -rmnlon
            ylab = ( ( rmxlon + rmnlon ) / 2.0 ) - rstlon
          ELSE
            ylab = ( rmxlon + rmnlon ) / 2.0
        END IF
        xlab = rmxlat - 1.0
C
        CALL GQBND ( 'N', rmnlat, rmnlon, rmxlat, rmxlon, ier )
C
C*      Draw map boundary fills - draw ocean first, then land and lakes.
C
        coltag = 's_water_mask'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 24, ier)
        CALL GQCOLR ( icolr, ier)
        CALL ST_INCH ( icolr, cmap, ier )
        CALL ST_LSTR ( cmap, lens, ier )
        bgsea =  'bg/' // cmap(:lens) // '//1' 
        CALL GG_BND ( bgsea, ier )
        IF ( ier .lt. 0 ) mapok = .false.
C
        coltag = 's_land_mask'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 29, ier)
        CALL GQCOLR ( jcolr, ier)
        CALL ST_INCH ( jcolr, cmap, ier )
        CALL ST_LSTR ( cmap, lens, ier )
C
        bgland = 'hcn_bnds/' // cmap(:lens)
        CALL GG_BND ( bgland, ier )
        IF ( ier .lt. 0 ) mapok = .false.
C
        coltag = 's_water_mask'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 24, ier)
        CALL GQCOLR ( icolr, ier)
        CALL ST_INCH ( icolr, cmap, ier )
        CALL ST_LSTR ( cmap, lens, ier )
        bglake = 'lakes/' //cmap(:lens)
        CALL GG_BND ( bglake, ier )
        IF ( ier .lt. 0 ) mapok = .false.
C
C*      Call swath routine.
C
        IF ( ( hvts ) .and. ( mapok )  ) THEN
            CALL GH_SWLN (ise, ine, inw, isw, alat, alon, inct, hvhur, 
     +                    ier)
        END IF
C
C*	Set the color for the text box.
C
        im = 1
 	CALL GSCOLR ( 1, ier )
C
C*	Set the first and last advisory numbers.
C
        anum = wadnm(1)
        tnum = wadnm(nstrm)
C
C*      Draw map and lat/lon lines.
C
        IF ( mapok ) THEN
            CALL ST_INCH ( iblk, cblk, ier)
            CALL ST_LSTR ( cblk, lens, ier)
            mapstr = cblk(:lens) // '/1/1'
	    CALL IP_SVAR ( '$MAPFIL=hipowo.nws', ier )
            CALL GG_MAP  ( mapstr, ier )
            CALL GG_LTLN ( latlon, ier )
	    CALL GG_SCAL ( mscale, ier )
     	END IF
C
        coltag = 's_swath_box_text'
        CALL ST_LSTR ( coltag, lens, ier)
 	CALL GH_COLR ( coltag(:lens), iwht , ier )
C
C*	Create the label strings for the text in the box.
C
        CALL ST_LCUC ( wname(nstrm),wname(nstrm),ier)
        CALL GH_SWLB ( origc(nstrm), wname(nstrm), anum, tnum, 
     +                 xlab, ylab, ier)
C
C*      Plot text if wind history is not found.
C
        IF ( ( .not. hvts ) .and. ( mapok ) ) THEN
            ixoff  = 0
  	    iyoff  = 0
            coltag = 's_swath_box_text'
            CALL ST_LSTR ( coltag, lens, ier)
 	    CALL GH_COLR ( coltag(:lens), iwht , ier )
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 2, ier )
            IF ( advno .eq. '1' )  THEN
                swthbg = 'SWATH DISPLAY BEGINS WITH ADVISORY NUMBER 2'
                CALL ST_LSTR ( swthbg, lenw, ier )
 	        CALL GTEXT ( 'N', .5, .4, swthbg(:lenw), 0.0, ixoff, 
     +                   iyoff, ier )
            END IF
C
            nostrm = 'NO HISTORY OF TROPICAL STORM OR HURRICANE WINDS'
            CALL ST_LSTR ( nostrm, lens, ier )
  	    iyoff  = -5 
 	    CALL GTEXT ( 'N', .5, .4, nostrm(:lens), 0.0, ixoff, 
     +                   iyoff, ier )
        END IF
C
C*	Plot text if map is invalid.
C
        IF ( .not. mapok ) THEN
            ixoff  = 0
  	    iyoff  = 0
            coltag = 's_swath_box_text'
            CALL ST_LSTR ( coltag, lens, ier)
 	    CALL GH_COLR ( coltag(:lens), iwht , ier )
            CALL ST_LSTR ( wname, lenw, ier )
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 2, ier )
            swthbg = 'UNABLE TO DISPLAY WIND SWATH FOR '
     +                // wname(nstrm)(:lenw) //' ADVISORY ' // tnum
     +                
            CALL ST_LSTR ( swthbg, lenb, ier )
 	    CALL GTEXT ( 'N', .5, .4, swthbg(:lenb), 0.0, ixoff, 
     +                   iyoff, ier )
        END IF
C
C*	Plot NWS logo.
C
	CALL GLOGO ( 'N', .13, .07, 2.5, 2, 2, ier )
C
C*	Plot NOAA logo.
C
	CALL GLOGO ( 'N', .88, .07, 2.5, 2, 1, ier )
C
C*	Reset the saved attributes.
C
	CALL GH_REST ( ier )
C*
	RETURN
	END
