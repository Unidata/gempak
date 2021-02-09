	SUBROUTINE GH_RDAD ( strnam, advno, nl, wtype, timstr, wname,
     +                       origc, wocen, wadnm, wwnd, wgst, wdir, 
     +                       wsped, wpres, rlat, rlon, windft, iwflag, 
     +                       fdate, mxwd, fgst, lndext, nstrm, nknt,
     +                       idisp, sytim, sylat, sylon, iret )
C************************************************************************
C* GH_RDAD								*
C*									*
C* This subroutine reads the forecast/advisory text advisories for      *
C* tropical storms.							*
C*									*
C* GH_RDAD ( STRNAM, ADVNO, NL, WTYPE, TIMSTR, WNAME, ORIGC, WOCEN,  	*
C*           WADNM, WWND, WGST, WDIR, WSPED, WPRES, RLAT, RLON, WINDFT, *
C*           IWFLAG, FDATE, MXWD, FGST, LNDEXT, NSTRM, NKNT,IDISP,	*
C*	     SYTIM, SYLAT, SYLON, IRET )				*
C*									*
C* Input parameters:							*
C*	STRNAM		CHAR*		Tropical storm name		*
C*	ADVNO		CHAR*           Advisory number			*
C*      NL              INTEGER		Maximum number of track points  *
C*									*
C* Output parameters:							*
C* 	WTYPE (*)	CHAR*		Tropical storm type		*
C* 	TIMSTR (*)	CHAR*		Date/time time string 		*
C*	WNAME (*)	CHAR*		Tropical storm name		*
C*	ORIGC (*)	CHAR*		Issuing Center  		*
C*     	WOCEN (*)	CHAR*		Storm identifier		*
C*	WADNM (*)	CHAR*		Advisory number			*
C*	WWND(*) 	CHAR*		Max sustained winds		*
C*	WGST(*) 	CHAR*		Max wind gust			*
C*	WDIR(*) 	CHAR*		Movement direction		*
C*	WSPED (*)	CHAR*		Movement speed (kts)		*
C*	WPRES (*)	CHAR*		Central minimum pressure (mb)	*
C*	RLAT (NL,*)	REAL		Current/forecasted Latitudes	*
C*	RLON (NL,*)	REAL		Current/forecasted Longitudes	*
C*	WINDFT (4,*) 	CHAR*		Current wind/seas radii (nm)	*
C*	IWFLAG 		INTEGER		Array of correction indicators  *
C*	FDATE (NL,*)	CHAR*		Forecast date/time strings	*
C*	MXWD (NL,*) 	INTEGER		Forecast max wind		*
C*	FGST (NL,*) 	INTEGER		Forecast wind gust		*
C*	LNDEXT (NL,*) 	CHAR*		Post-Tropical flag	        *
C*	NSTRM 		INTEGER		Number of decoded files		*
C*	NKNT		INTEGER		Number of forecast periods	*
C*	IDISP		INTEGER		Storm dissipating flag		*
C*	SYTIM (*)	CHAR*		Synoptic time                   *
C*	SYLAT (*)	REAL		Latitude at synoptic time       *
C*	SYLON (*)	REAL		Longitude at synoptic time      *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 5/01   					*
C* A. Hardy/GSC		 5/01  	Changed iret -> ier;add idisp;	        *
C*				removed NF from call sequence   	*
C* D. Kidwell/NCEP	 8/01	Added synoptic time, lat/lon            *
C* A. Hardy/GSC		 8/01  	Added tmptim chk & wadnm return         *
C* A. Hardy/SAIC         2/02   Changed call FL_SCND		        *
C* D. Kidwell/NCEP	 3/03	Added NFCST & ST_UNPR; fixed prologue   *
C* A. Hardy/NCEP 	 8/03	Added error check for missing date/time *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 03/05   Included 4-dig year in filename comparsn*
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* S. Gilbert/NCEP	05/06	Removed parsing of stime from GH_RDHD   *
C* S. Gilbert/NCEP	07/06	Added new variable origc                *
C* m.gamazaychikov/SAIC 06/07   Add arguments wgst and fgst, 		*
C*				add wgst and agust to GH_RDBD CS	*
C* m.gamazaychikov/SAIC 04/08   Add remnlo to CS, aremn to GH_RDBD CS	*
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* X. Guo/CWS           03/10   Used POST-TROP instead of Extratropical *
C*                              and Remnant Low.Removed argument remnlo *
C* A. Krautkramer	04/10   Add variable definition, change the 	*
C*				calculation the time difference, change	*
C*				the call to TO_CTOI			*
C* B. Hebbard/NCEP	 5/20	Update for new 60hr forecast - SCN20-20	*
C* 				increased PARAMETER NFCST 7 -> 8	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	strnam, advno, wtype(*), wname(*), origc(*),
     +                  wadnm(*), wocen(*), wwnd(*), wdir(*), wsped(*), 
     +                  wpres(*), fdate(nl,*), lndext(nl,*),
     +                  windft(4,*), timstr(*), sytim (*), wgst (*)
        INTEGER         mxwd(nl,*), fgst(nl,*)
        REAL            rlat(nl,*), rlon(nl,*), sylat (*), sylon (*)
C*
	PARAMETER	( NFCST = 8 ) 
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
	CHARACTER	path*25, templ*(MXTMPL), tfile*128,
     +                  adnum*3, record*10000, rec*128
C*
        CHARACTER       adate(NFCST)*10, awind(NFCST)*6, stime*30, 
     +			tmptim*20, sixty*50, fifty*50, thirty*50, 
     +			seaft*40, aldex(NFCST), syntim*20,
     +			agust(NFCST)*6
	INTEGER		idtarr (5)
	INTEGER		hrsyn, hradv, hrdiff
        REAL            flat(NFCST), flon(NFCST)
C*
	INCLUDE		'ERMISS.FNC'
C*
C-----------------------------------------------------------------------
	iret = 0
	nstrm = 0
C
C*	Scan the directory for all of the hurricane data files.
C
	filnam = 'HCNADV'
	path  = ' '
	templ = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +  	ihb, mnb, iha, mna, mstrct, idtmch, ier )
        CALL ST_RNUL ( path, path, lens, ier )
        CALL ST_RNUL ( templ, templ, lens, ier )
C
	CALL ST_LSTR ( path, lenp, ier )
        nexp   = MXNMFL
        iorder = 1
	CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
        IF ( ier .ne. 0 ) THEN 
            CALL ER_WMSG ( 'GPTPC', -6, ' ', ier )
            RETURN
        END IF
C
        CALL ST_LSTR( advno, lens, ier )
        IF ( lens .eq. 1 ) THEN
            adnum = '00' // advno(:lens) 
          ELSE IF ( lens .eq. 2 ) THEN
            adnum = '0' // advno(:lens) 
          ELSE
            adnum = advno(:lens) 
        END IF
        CALL ST_LSTR( strnam, lens, ier )
        filnam = strnam(1:lens)//'.fstadv.' // adnum
        CALL ST_LSTR( filnam, lenf, ier )
C
C*	Decode each file until the most current file is reached.
C
 	DO ifl = 1, nfile
	    IF  ( (files(ifl)(1:8) .eq. filnam(1:8) ) .and.
     +                ( files(ifl)(17:19) .le. filnam(17:19) ) ) THEN
	        nstrm = nstrm + 1
		tfile = path(:lenp) // '/' // files(ifl)
                CALL ST_LSTR( tfile, lent, ier )
 		CALL FL_SOPN ( tfile, lunf, ier )
C
                lrec = 0
                iostat = 0
                DO WHILE  ( iostat .eq. 0 )
                    READ  ( lunf, 1000, IOSTAT = iostat ) rec
 1000               FORMAT (A)
                    CALL ST_LSTR( rec, lent, ier )
                    IF ( lrec .eq. 0 ) THEN
                        lrec = lent
                        record = rec (:lent)
                      ELSE
                        CALL ST_LSTR( record, lrec, ier )
                        record = record(:lrec) // ' ' // rec (:lent)
                        CALL ST_LSTR( record, lrec, ier )
                    END IF     
                END DO
                CALL FL_CLOS ( lunf, ier )
C
		CALL ST_LSTR ( record, lrec1, ier )
		CALL ST_UNPR ( record, lrec1, record, lrec, ier )
		CALL ST_LCUC ( record, record, ier )
C
C*              Decode the advisory header lines.
C
                CALL GH_RDHD ( record, wtype(nstrm), wname(nstrm), 
     +                         origc(nstrm), wadnm(nstrm), wocen(nstrm),
     +                             stime, iedtm, ier )
                IF ( ier .ne. 0 ) THEN
                    CALL ER_WMSG ( 'GPTPC', -13, ' ', ier )
                END IF
C
C*              Find the advisory text paragraph.
C
                CALL GH_RDTX ( record, wtype(nstrm),adnum, 
     +                             wadnm(nstrm), iedtm, 
     +                             iwflag, ier )
C
C*              Decode the advisory body.
C
 		CALL GH_RDBD ( record, lrec, NFCST, tmptim,
     +                       slat, slon, wdir(nstrm), wsped(nstrm), 
     +                       wpres(nstrm), wwnd(nstrm), wgst(nstrm), 
     +                       sixty, fifty, thirty, seaft, flat, 
     +                       flon, adate, awind, agust, 
     +                       aldex, nknt, idisp, 
     +                       syntim, synlat, synlon, ier )
C
C*              If missing time string, use one from header time.
C
                IF ( tmptim .ne. ' ' ) THEN
                    timstr(nstrm) = stime(1:4) // tmptim(1:7)
                ELSE
                    timstr(nstrm) = stime
                END IF
C
                rlat(1,nstrm) = slat
                rlon(1,nstrm) = slon
C
C*              If storm is dissipating and missing lat/lon, 
C*              use previous advisory position.
C
                IF ( ( idisp .ne. 0 ) .and. ( ERMISS ( slat )
     +             .or. ERMISS ( slon ) ) ) THEN
                    rlat(1,nstrm) = rlat(1,nstrm-1)
                    rlon(1,nstrm) = rlon(1,nstrm-1)
                END IF
C
C*	        Store wind/sea feet information.
C
                windft ( 1,nstrm ) = sixty
                windft ( 2,nstrm ) = fifty
                windft ( 3,nstrm ) = thirty
                windft ( 4,nstrm ) = seaft
                DO kk = 2, NFCST + 1
                    rlat(kk,nstrm) = flat(kk-1)
                    rlon(kk,nstrm) = flon(kk-1)
                END DO
                lndext(1,nstrm) = ' '
                DO kk = 1, NFCST
                    fdate(kk,nstrm) = adate(kk)
                    lndext(kk,nstrm) = aldex(kk)
                    CALL ST_NUMB (awind(kk), mxwd(kk,nstrm), ier)
                    CALL ST_NUMB (agust(kk), fgst(kk,nstrm), ier)
                END DO
C
		CALL ST_LSTR ( syntim, lens, ier )
		IF ( syntim ( :lens ) .le. tmptim ( :lent ) ) THEN
		    sytim ( nstrm ) = timstr ( nstrm ) ( :4 ) 
     +				      // syntim ( :lens )
		  ELSE
		    CALL ST_INTG ( tmptim ( 4:5 ), hradv, ier )
		    CALL ST_INTG ( syntim ( 4:5 ), hrsyn, ier )
		    hrdiff = (hradv - hrsyn) * 60
		    CALL TI_CTOI ( timstr(nstrm), idtarr, ier )
		    CALL TI_SUBM ( idtarr, hrdiff, idtarr, ier )
		    CALL TI_ITOC ( idtarr, sytim ( nstrm ), ier )
		    IF ( ier .ne. 0 ) sytim ( nstrm ) = timstr ( nstrm ) 
		END IF
		sylat ( nstrm ) = synlat
		sylon ( nstrm ) = synlon
            END IF
        END DO
C*
	RETURN
	END


