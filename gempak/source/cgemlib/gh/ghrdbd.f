	SUBROUTINE GH_RDBD ( advise, lenw, nfcst, tmptim, slat, slon,
     +                       wdir, wsped, wpres, wwnd, wgst, 
     +			     sixty, fifty, thirty, seaft, flat, flon,
     +                       adate, awind, agust, aldex, nknt, idisp,
     +                       syntim, synlat, synlon, iret )
C************************************************************************
C* GH_RDBD 								*
C*									*
C* This subroutine decodes the text body of a single forecast/advisory  *
C* report.      							*
C*                                                                      *
C* GH_RDBD ( ADVISE, LENW, NFCST, TMPTIM, SLAT, SLON, WDIR, WSPED,      *
C*           WPRES, WWND, WGST, SIXTY, FIFTY, THIRTY, SEAFT, FLAT, FLON,*
C*           ADATE, AWIND, AGUST, ALDEX, NKNT, IDISP, SYNTIM,  SYNLAT,	*
C*	     SYNLON, IRET )          			         	*
C*									*
C* Input parameters:							*
C*	ADVISE		CHAR*		Report advisory bulletin	*
C*	LENW		INTEGER		Length of bulletin              *
C*	NFCST		INTEGER		Number of fcst times allowed    *
C*									*
C* Output parameters:							*
C*	TMPTIM		CHAR*		Valid location time		*
C*	SLAT 		REAL		Latitude of points              *
C*	SLON 		REAL		Longitude of points             *
C*	WDIR 		CHAR*		Direction of storm movement(deg)*
C*	WSPED 		CHAR*		Speed of movement (knots)	*
C*	WPRES 		CHAR*		Minimum central pressure (mb)   *
C*	WWND		CHAR*		Max sustained wind		*
C*	WGST		CHAR*		Max wind gust			*
C*	SIXTY		CHAR*		64 kt current quadrant string	*
C*	FIFTY		CHAR*		50 kt current quadrant string	*
C*	THIRTY		CHAR*		34 kt current quadrant string	*
C*	SEAFT		CHAR*		12 ft seas quadrant string	*
C*	FLAT (*) 	REAL		Array of forecasted latitudes   *
C*	FLON (*)	REAL		Array of forecasted longitudes  *
C*	ADATE (*)	CHAR*		Forecast time array 		*
C*	AWIND (*)	CHAR*		Forecast wind array 		*
C*	AGUST (*)	CHAR*		Forecast wind gust array 	*
C*	ALDEX (*)	CHAR*		Forecast post-tropical array    *
C*	NKNT		INTEGER		Number of forecast time periods *
C*	IDISP		INTEGER		Storm dissipation flag		*
C*	SYNTIM		CHAR*		Synoptic time			*
C*	SYNLAT		REAL		Latitude at synoptic time       *
C*	SYNLON		REAL		Longitude at synoptic time      *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 5/01						*
C* A. Hardy/GSC		 6/01	Added idisp flag		        *
C* A. Hardy/GSC		 6/01 	Cleaned up prolog		        *
C* D. Kidwell/NCEP       8/01   Added synoptic time, lat/lon            *
C* A. Hardy/SAIC	 8/01 	Added error checking		        *
C* D. Kidwell/NCEP       4/02   Added lenw to ST_LCUC; lrec->lenw at end*
C* D. Kidwell/NCEP       3/03   Added nfcst; record->advise, lrec->lenw *
C* D. Kidwell/NCEP       1/04   Added check for leading R w/1 digit wdir*
C* m.gamazaychikov/SAIC 03/05   Modified locating the CENTER in bulletin*
C* m.gamazaychikov/SAIC 06/07   Add arguments wgst, agust decode wgst, 	*
C*				add agust to GH_RDFV CS			*
C* m.gamazaychikov/SAIC 04/08   Add aremn to CS, and to GH_RDFV CS	*
C* X. Guo/CWS           03/10   Removed argument aremn                  *
C* S. Jacobs/NCEP	 3/12	Fixed check for 'WITHIN' to not start	*
C*				at the beginning of the bulletin	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'BRIDGE.PRM'
C*
	CHARACTER*(*)	advise,  tmptim, wdir, wsped, wpres, awind(*),
     +                  sixty, fifty, thirty, seaft, adate(*), wwnd,
     +                  aldex (*), syntim, agust(*), wgst
	REAL   		flat(*), flon(*)
C*
	CHARACTER	direc*3, speed*3, presmn*5, carr (10)*8 
        LOGICAL         done, first
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
        icind = 0
C
C*	Search for correction indication.
C
	icind = INDEX( advise( :lenw), '..COR') 
        IF ( icind .ne. 0 ) icor = 1
C
C*	Search for the center location of storm.
C
        done  = .false.
        inear = 1
        ipnear = 0
	DO WHILE ( .not. done )
           CALL ST_NOCC ( advise( :lenw), 'NEAR', inear, ipnear, ier )
           IF ( ier .ge. 0 ) THEN
              CALL ST_NOCC ( advise( ipnear-25:ipnear), 'CENTER', 
     +                       1, ipcnt, ier2 )
              IF ( ier2 .ge. 0 ) THEN
                  done = .true.
                ELSE
                  inear = inear + 1
              END IF
           END IF
           IF ( ipnear .eq. 0 ) THEN 
              done = .true.
              ipnear = 1
           END IF
        END DO

        ipos = ipnear 
C
C*	Locate the latitude of the center.
C
        CALL ST_NOCC ( advise(ipos:lenw), '.', 1, ipos1, ier )
        CALL ST_C2R ( advise(ipos+5:ipos+ipos1), 1, slat, num, ier )
        IF ( ier .lt. 0 ) THEN
            slat = RMISSD
          ELSE 
            IF ( advise(ipos+ipos1+1:ipos+ipos1+1) .eq. 'S' ) 
     +          slat = -slat
        END IF
        ipos = ipos+ipos1+1
C
C*	Locate the longitude of the center.
C
        CALL ST_NOCC ( advise(ipos:lenw), '.', 1, ipos2, ier )
        CALL ST_C2R ( advise(ipos+1:ipos+ipos2), 1, slon, num, ier )
        IF ( ier .lt. 0 ) THEN
            slon = RMISSD
          ELSE
            IF ( advise(ipos+ipos2+1:ipos+ipos2+1) .eq. 'W' ) 
     +           slon = -slon
        END IF
C
C*	Search for the date time string.
C
	iposdat = INDEX( advise(ipos:lenw), 'AT') 
        IF ( ERMISS ( slat ) .and. ERMISS ( slon ) ) THEN
            icen = INDEX (  advise(:lenw), 'CENTER')
            CALL ST_NOCC ( advise(icen:lenw), '/', 1, ipos6, ier )
            IF ( ier .eq. 0 ) THEN
                tmptim = advise(icen+ipos6-3:icen+ipos6+3)
              ELSE
               tmptim = ' '
            END IF
          ELSE 
            CALL ST_NOCC ( advise(ipos:lenw), 'Z', 1, ipos3, ier )
            IF ( ier .eq. 0 ) THEN
                tmptim = advise(ipos+iposdat+2:ipos+ipos3-2)
                CALL ST_LSTR ( tmptim, lent, ier )
                IF ( lent .eq. 6 ) THEN
                   tmptim = advise(ipos+iposdat+1:ipos+ipos3-2)
                END IF
              ELSE
               tmptim = ' '
            END IF
        END IF
C
C*	Search for the accuracy position.
C
	iposnm = INDEX( advise(ipos:lenw), 'WITHIN') 
C
C*	Search for the movement and speed.
C
        CALL ST_NOCC ( advise(ipos:lenw), 'DEGREES', 1, ipos1, ier )
        IF ( ipos1 .ne. 0 ) THEN
            direc = advise(ipos+ipos1-5:ipos+ipos1)
	    IF ( direc ( 1:1 ) .eq. 'R' ) direc ( 1:1 ) = ' '
	    iposnm = INDEX( advise(:lenw), 'DEGREES') 
            CALL ST_NOCC ( advise(iposnm:lenw), 'KT', 1, ipos1, ier )
            IF ( ier .lt. 0 ) THEN
                direc = ' '
                speed = '0'
              ELSE
                speed = advise(iposnm+ipos1-4:iposnm+ipos1)
            END IF
          ELSE   
            CALL ST_NOCC ( advise(ipos:lenw), 'STATIONARY', 1, 
     +                     ipos1, ier )
            IF ( ipos1 .ne. 0 ) THEN
                direc = ' '
                speed = '0'
            END IF
	    iposnm = ipos+ipos1
        END IF
        wdir  = direc
        wsped = speed 
C
C*	Search for the central pressure.
C
	iposprs = INDEX( advise(iposnm:lenw), 'PRESSURE')
        IF ( iposprs .ne. 0 ) THEN
            ipres = iposnm+iposprs+7
            presmn = advise(ipres+1:ipres+5)
          ELSE 
            presmn = 'XXX'
        END IF
        wpres = presmn
C
C*	Search for the max sustained winds.
C
        ipos3 = INDEX( advise(iposnm+7:lenw), 'WINDS')
        ipos4 = iposnm+ipos3 + 12
        ikt = INDEX( advise(ipos4:lenw), 'KT')
        IF ( ikt .ne. 0 ) THEN
            wwnd = advise(ipos4:ipos4+2)
          ELSE
            wwnd = 'XXX'
        END IF
C
C*      Search for the current wind gusts.
C
        igst = INDEX( advise(iposnm+12:lenw), 'GUSTS')
        igst2 = iposnm+igst + 10
        ikt2 = INDEX( advise(igst2:igst2+16), 'KT')
        IF ( ikt2 .ne. 0 ) THEN
            wgst = advise(igst2+9:igst2+12)
          ELSE
            wgst = 'XXX'
        END IF
C
C*	Search for the current quadrant information.
C
	igust = INDEX( advise(ipos:lenw), 'GUSTS') 
        ipos2 = ipos+igust
	irep = INDEX( advise(ipos2:lenw), 'REPEAT') 
        CALL GH_RDQD ( advise(ipos2:ipos2+irep), sixty, fifty,
     +                 thirty, seaft, ier )
C
C*	Get the synoptic time and location.
C
	syntim = tmptim
	synlat = RMISSD
	synlon = RMISSD
	locwas = INDEX ( advise(ipos2+irep:lenw), 'WAS LOCATED' )
	IF ( locwas .gt. 0 ) THEN
	    ipos3 = ipos2 + irep + locwas - 18 
	    CALL ST_CLST ( advise (ipos3:ipos3+50), ' ', ' ', 10, carr,
     +			   num, ier )
	    done  = .false.
	    first = .true.
	    ii    = 1
	    DO WHILE ( .not. done )
		iz = INDEX ( carr ( ii ), 'Z' ) 
		IF ( iz .gt. 0 ) THEN
		    syntim = carr ( ii ) ( 1:iz - 1 )	
		  ELSE
		    idec = INDEX ( carr ( ii ) , '.' )
		    IF ( idec .gt. 0 ) THEN
		        IF ( first ) THEN
			    CALL ST_CRNM ( carr (ii)(1:idec+1), 
     +					   synlat, ier )
			    IF ( ( ier .eq. 0 ) .and.
     +			         ( carr (ii)(idec+2:idec+2) .eq. 'S' ) )
     +				 synlat = -synlat 
   			    first = .false.
		          ELSE
			    CALL ST_CRNM ( carr (ii)(1:idec+1), 
     +					   synlon, ier )
			    IF ( ( ier .eq. 0 ) .and. 
     +			         ( carr (ii)(idec+2:idec+2) .eq. 'W' ) )
     +				 synlon = -synlon 
			    done = .true.
		        END IF
		    END IF
		END IF
		ii = ii + 1
		IF ( ii .gt. num ) done = .true.
	    END DO
	END IF
	IF ( ERMISS ( synlat ) ) synlat = slat
	IF ( ERMISS ( synlon ) ) synlon = slon
	CALL ST_LSTR ( syntim, lens, ier )
	IF ( lens .eq. 5 ) syntim ( 6:7 ) = '00'
C
C*	Search for all valid forecast positions, up to nfcst.
C
        CALL GH_RDFV ( advise(ipos2+irep:lenw), nfcst, flat, flon,
     +                 adate, awind, agust, aldex, nknt, ier )
C
C*	Check for dissipating storm.
C
        IF ( ERMISS ( flat(1) ) ) THEN
            CALL GH_RDDS (  advise, lenw, slat, slon, wdir, wsped, 
     +                      wpres, wwnd, tmptim, idisp, iret )
          ELSE
            idisp = 0
        END IF
C*
	RETURN
	END
