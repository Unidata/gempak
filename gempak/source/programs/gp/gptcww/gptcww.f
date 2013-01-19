	PROGRAM GPTCWW
C************************************************************************
C* GPTCWW								*
C* This program plots the tropical cyclone watch/warn graphic. 		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/01   Split out code from GPTPC               *
C* D. Kidwell/NCEP	 4/02   Mapped device PS to PSC                 *
C* D. Kidwell/NCEP	 3/03   Allowed 3 or 5 day forecast display     *
C* D. Kidwell/NCEP	 4/03   Added GH_BKRD call                      *
C* D. Kidwell/NCEP	11/03   Added processing for WWA text product   *
C* D. Kidwell/NCEP	12/03   Added FL_CLOS for lunbk,lunbko          *
C* D. Kidwell/NCEP	 1/04   Incr. ibkel,istsnd; CSC GH_KGPH, GH_BKRC*
C* m.gamazaychikov/SAIC	03/04	from GPKGRF				*
C* m.gamazaychikov/SAIC	01/05	Added input parms itrack and szmrk	*
C* m.gamazaychikov/SAIC	03/05	Added input parms ltype, lwidth, lcolor	*
C*				and scale flag				*
C* S. Gilbert/NCEP	 1/06   Added call to GH_RDPB to read public    *
C*                              advisory for intermediate advisories    *
C* S. Gilbert/NCEP	 6/06   Increased size of wsped                 *
C* S. Gilbert/NCEP	 7/06   Added variable origc                    *
C* m.gamazaychikov/SAIC	06/07	Add ability to output to VG device 	*
C*				Change CS to GH_RDAD			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* S. Jacobs/NCEP	 2/08	Added init of device using GN driver	*
C* m.gamazaychikov/SAIC	04/08	Added remnlo to GH_RDAD CS,		*
C*				added lndext and remnlo to GH_TCWW CS	*
C* m.gamazaychikov/SAIC	10/08	Added lndext and remnlo to GH_TC2VG CS  *
C* X. Guo		03/10   Moved the legend box underneath graphic *
C*                              Removed remnlo and ilegend flag         *
C* Krautkramer	        04/10   Added variable definition,  calculate   *
C*				the correct first tau, pass the inital  *
C*                              tau to GC_TC2VG                         *
C* X. Guo/CWS		05/10   Added idays to GH_TCWW			*
C* S. Jacobs/NCEP	 6/10	Re-added flag for plotting the scale	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        PARAMETER	( NF = 5000, NL = 25 )
C*
	CHARACTER	device*(LLMXLN), panel*(LLMXLN), 
     +			garea*(LLMXLN), proj*(LLMXLN), 
     +			strnam*(LLMXLN), advno*(LLMXLN),
     +                  strmid*(LLMXLN)
C*
	CHARACTER	imgfls*132, starr(11)*72
	LOGICAL		respnd, done, proces, hist, reset
C*
	CHARACTER	wname(NF)*16, timstr(NF)*30, wtype(NF)*2,
     +                  wwnd (NF)*3, wgst (NF)*4,
     +                  wdir(NF)*4, wsped(NF)*10, 
     +                  wpres(NF)*4, wocen(NF)*6, wadnm(NF)*4, 
     +                  windft(4,NF)*60, fdate(NL,NF)*7, tmp*20,
     +                  lndext(NL,NF)*1, tdev*12, origc(NF)*5,
     +			storms(NF)*8, advnms(NF)*4, sytim(NF)*20
	REAL		rlat(NL,NF), rlon(NL,NF), sylat(NF), sylon(NF),
     +                  szmrk(2)
	INTEGER		mxwd (NL,NF), ltype (2), lwidth (2), lcolor (2),
     +			gust (NL,NF) 
	INTEGER 	tau, hradv, hrsyn	
	CHARACTER       darr(4)*80, tail*4, tnam*22, devck*12,
     +                  chyy*2, cadvno*4
        CHARACTER       pwtype*30, ptimstr*30, pwname*30, pwadnm*30, 
     +                  pwwnd*30, pwdir*30, pwsped*30, pwpres*30, 
     +                  pgust*30
        REAL            prlat, prlon
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	done = .false.
	reset = .false.
C
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPTCWW', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*      Initialize grid library common area grdcmn.cmn
C 
        CALL GD_INIT  ( ier )
C
C*	Initialize graphics.
C
        mode = 1
C
	CALL GG_INIT  ( mode, iret )
	CALL IP_IDNT ( 'GPTCWW', ier )
        CALL GH_RTBL ( ier )
	CALL GG_SDEV ( 'GN', ier )
C
C*	Read the breakpoint plotting table.
C
	CALL GH_BKTB ( ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'GPTCWW', ier, ' ', ierr )
	    done = .true.
	END IF
C
	DO WHILE  ( .not. done )
C
C*	    Get input parameters.
C
	    CALL GPWINP  ( strmid, iperr )
	    IF  ( iperr .ne. 0 )  THEN
	        CALL ER_WMSG  ( 'GPTCWW', iperr, ' ', ier )
	        CALL SS_EXIT
	    END IF
C
	    CALL ST_CLST ( strmid, '/', ' ', 11, starr, numw, ier )
	    IF  ( starr(1)(1:1) .ne. ' ' )  THEN
	        CALL ST_UCLC ( starr(1), strnam, ier )
              ELSE
                strnam = ' '
	    END IF
C
            IF ( starr(2)(1:1) .ne. ' ' )  THEN
	        advno = starr(2)
              ELSE
                advno = ' '
	    END IF
C
C*	    Check to see if the history file is to be used.
C
	    IF ( ( strnam .eq. ' ' ) .or. ( advno .eq. ' ' ) ) THEN
		CALL GH_RHST ( 'history_tc', nstrms, storms, advnms, 
     +			       ier )
                IF ( ier .eq. -1 ) THEN
	    	    CALL ER_WMSG ('GPTCWW', -5, ' ', ier1 )
                  ELSE IF ( ier .eq. -2 ) THEN
	    	    CALL ER_WMSG ('GPTCWW', -6, ' ', ier1 )
		  ELSE IF ( nstrms .eq. 0 ) THEN
		    CALL ER_WMSG ('GPTCWW', 1, ' ', ier1 )
                END IF
		hist = .true.
	      ELSE 
		hist   = .false.
		nstrms = 1
	    END IF
C
C*          Check device and create filename.
C
            IF ( starr(3)(1:1) .ne. ' ' )  THEN
                device = starr(3)
                CALL ST_CLST ( device, '|', ' ', 4, darr, numd, ier )
                CALL ST_UCLC ( darr(1), devck, ier )
                CALL ST_LSTR (  devck, lens, ier )
                IF ( devck (:lens) .eq. 'xw' ) THEN
                     device = 'XW'
                     tail = '  '
                  ELSE IF ( ( devck(:lens) .eq. 'ps' ) .or.
     +                      ( devck(:lens) .eq. 'psc' ) ) THEN
                     device = 'PSC'
                     tail = '.psc'
                  ELSE IF ( ( devck(:lens) .eq. 'vg' ) ) THEN
                     device = 'VG'
                     tail = '.vgf'
                  ELSE
                    device = 'GF'
                    tail = '.gif'
                END IF
              ELSE 
                device = 'GF'
                tail = '.gif'
            END IF
            tdev = device
C
C*	    Check for a 3 or 5 day forecast display.
C
	    ntimes = 8
            IF ( starr (4) (1:1) .ne. ' ' )  THEN
		CALL ST_LSTR ( starr (4), lens, ier )
		CALL ST_INTG ( starr (4) ( :lens ), ndays, ier )
		IF ( ndays .eq. 3 ) ntimes = 6
              ELSE
                ndays = 5
	    END IF 
C
C*	    Check for track flag.
C
            IF ( starr (6) (1:1) .eq. 'N' .or. 
     +           starr (6) (1:1) .eq. 'n')  THEN
               itrack = 0
             ELSE
               itrack = 1
            END IF
C
C*	    Get the size multiplier for forecast position markers.
C
            CALL ST_RLST (starr(7), ';', RMISSD, 2, szmrk, nszmrk, ier)
            IF ( nszmrk .eq. 0 )  THEN
               szmrk (1) = 2
               szmrk (2) = 1.5
             ELSE IF ( nszmrk .eq. 1) THEN
               szmrk (2) = 0.75 * szmrk (1)
             ELSE IF ( nszmrk .eq. 2 .and. ERMISS ( szmrk (1) ) ) THEN
               szmrk (1) = szmrk (2) + szmrk (2) / 3
            END IF 
C
C*          Get the forecast track line type
C
            CALL ST_ILST (starr(8), ';', IMISSD, 2, ltype, nltp, ier)
            IF ( nltp .eq. 0 )  THEN
               ltype (1) = 1
               ltype (2) = 3
             ELSE IF ( nltp .le. 2 .and. ltype (1) .eq. IMISSD ) THEN
               ltype (1) = 1
             ELSE IF ( nltp .le. 2 .and. ltype (2) .eq. IMISSD ) THEN
               ltype (2) = 3
            END IF
C
C*          Get the forecast track line width
C
            CALL ST_ILST (starr(9), ';', IMISSD, 2, lwidth, nlwd, ier)
            IF ( nlwd .eq. 0 )  THEN
               lwidth (1) = 4
               lwidth (2) = 4
             ELSE IF ( nlwd .le. 2 .and. lwidth (1) .eq. IMISSD ) THEN
               lwidth (1) = 4
             ELSE IF ( nlwd .le. 2 .and. lwidth (2) .eq. IMISSD ) THEN
               lwidth (2) = 4
            END IF
C
C*          Get the forecast track line color
C
            CALL ST_ILST (starr(10), ';', IMISSD, 2, lcolor, nlcl, ier)
            IF ( nlcl .eq. 0 )  THEN
               lcolor (1) = 32
               lcolor (2) = 32
             ELSE IF ( nlcl .le. 2 .and. lcolor (1) .eq. IMISSD ) THEN
               lcolor (1) = 32
             ELSE IF ( nlcl .le. 2 .and. lcolor (2) .eq. IMISSD ) THEN
               lcolor (2) = 32
            END IF
C
C*          Check for scale legend plotting flag.
C
	    IF ( starr (11) (1:1) .eq. 'N' .or.
     +		 starr (11) (1:1) .eq. 'n')  THEN
		iscale = 0
	     ELSE
		iscale = 1
	    END IF

C
C*	    Loop over storm advisories to be processed.
C
	    DO iproc = 1, nstrms
	        IF ( hist ) THEN
		    strnam = storms ( iproc )
		    advno  = advnms ( iproc )
		    WRITE (6,*) 'Processing storm ', strnam ( :8 ),
     +				' advisory number ', advno ( :4 )
	        END IF
	        proces = .true.
C
C*		Construct file/title name.
C
                CALL ST_LSTR( strnam, lenn, ier )
                IF ( lenn .eq. 6 ) THEN
                    CALL ST_NUMB ( strnam(5:6), istrm, ier )
                    IF ( istrm .le. 25 ) THEN
                        chyy = '20'
                      ELSE
                        chyy = '19'
                    END IF
                    strnam = strnam(1:4) // chyy // strnam(5:6)
                END IF
C
                CALL ST_LSTR( advno, lens, ier )
                CALL ST_LSTR( strnam, lenn, ier )
                CALL ST_LSTR( tail, lent, ier )
                tnam = strnam(:lenn) // '?.' //
     +                 advno (:lens) // tail(:lent)
                CALL ST_LCUC ( tnam, tnam, ier )
                CALL ST_LSTR( tnam, lenm, ier )
C
C*              Read each text advisory and store information.
C
                CALL ST_NULL ( advno, advno, lena, ier )
                CALL GH_ADVN ( advno, iadnum, iaflag, ier )
                CALL ST_INCH ( iadnum, cadvno, ier )
                CALL GH_RDAD ( strnam, cadvno, NL, wtype, timstr,wname,
     +                         origc, wocen, wadnm, wwnd, wgst, wdir, 
     +                         wsped, wpres, rlat, rlon, windft, iwflag,
     +                         fdate, mxwd, gust, lndext, nstrm,
     +                         nknt, idisp, sytim, sylat, sylon, iret )
		IF  ( (iret .ne. 0 ) .or. ( nstrm .eq. 0 ) )  THEN
                    proces = .false.
	            CALL ER_WMSG  ( 'GPTCWW', -3, ' ', ier )
                END IF
C
C*              Get updated storm info from intermediate advisory,
C*              if appropriate.
C
                IF ( proces .AND. iaflag .ne. 0 ) THEN
                    CALL GH_RDPB ( strnam, advno, pwtype, ptimstr,
     +                             pwname, pwadnm, pwwnd, pgust, pwdir,
     +                             pwsped, pwpres, prlat, prlon, iret )
                    IF ( iret .eq. 0 ) THEN
                        wadnm(nstrm) = pwadnm
                        wname(nstrm) = pwname
                        wtype(nstrm) = pwtype
                        timstr(nstrm) = ptimstr
                        rlat(1,nstrm) = prlat
                        rlon(1,nstrm) = prlon
                        wdir(nstrm) = pwdir
                        wsped(nstrm) = pwsped
                        wwnd(nstrm) = pwwnd
                        wgst(nstrm) = pgust
                    ELSE
                        proces = .false.
	                CALL ER_WMSG  ( 'GPTCWW', -13, ' ', ier )
                    ENDIF
                ENDIF
C
		IF  ( proces )  THEN
                    CALL GSTEXT ( 22, 2, 1.0, 1, 111, 1, 2, ier )
C
C*		    Set map projection.
C		  
                    proj  = 'MER'
		    garea = '-10.;-90.;35.;-15.'
		    CALL GG_MAPS ( proj, garea, imgfls, idrpfl, ier )
		    IF  ( ier .ne. 0 )  proces = .false.
C
		    IF  ( proces )  THEN
C
C*			Display user options, allow program exit.
C
                        CALL GPWOPT ( strmid, ier )
			IF  ( ier .ne. 0 )  proces = .false.
C
			IF  ( proces .AND. device .ne. 'VG'  ) THEN
C
C*			    Set the panel.
C
                            panel = '0.0;0.195;1.0;0.9999/0/27'
			    CALL GG_PANL ( panel, iret )
                            CALL GQBND ( 'V', rmnlat, rmnlon,
     +                             rmxlat, rmxlon, ier )
                            CALL GTRANS ( 'V', 'M', 1, rmnlat, rmnlon,
     +                                         rmnlat, rmnlon, iret )
                            CALL GTRANS ( 'V', 'M', 1, rmxlat, rmxlon,
     +                                         rmxlat, rmxlon, iret )
                            CALL GSMMGN ( 0., 0., 0., 0., ier )
                            CALL GSMMAP ('MER', rmnlat, rmnlon,
     +                                          rmxlat, rmxlon, ier )
C
C*			    Plot the current trop. storm or hurricane 
C*                          as the tropical cyclone watch/warning graphic.
C
                            WRITE(6,*)' '
                            WRITE(6,*)'Starting TRACK ERROR/'
     +                              // 'WATCH/WARN graphic '
                            IF ( device .eq. 'XW' ) CALL GCLEAR ( iret )
                            tnam(9:9) = 'W'
C
C*			    Adjust number of times if necessary.
C
			    IF ( nknt .gt. ntimes ) nknt = ntimes
 			    CALL GH_TCWW ( device, tnam, 
     +                              wtype(nstrm), timstr(nstrm), 
     +                              wname(nstrm), origc(nstrm), 
     +                              wocen(nstrm), wsped(nstrm), 
     +                              wadnm(nstrm), rlat, rlon, 
     +                              NL, nstrm, nknt, fdate, 
     +                              wwnd(nstrm), lndext,
     +                              idisp,mxwd, wdir(nstrm),
     +                              itrack, szmrk,ltype, lwidth,
     +                              lcolor,ndays,iscale,reset, iret )
                         ELSE IF ( proces .AND. device .eq. 'VG'  ) THEN
                            tmp = tnam
                            CALL ST_LSTR (  tmp, lent, ier )
                            tnam = tmp (:8)//'TCx'//tmp(10:lent)
                            WRITE(6,*)' '
                            WRITE(6,*)'Starting TRACK ERROR WATCH/WARN/'
     +                              // 'graphic for VG device driver'
C
C*			    Calculate the first tau
C
			    CALL ST_INTG ( timstr(nstrm) ( 8:9 ),
     +					   hradv, ier )
		    	    CALL ST_INTG ( sytim(nstrm) ( 8:9 ),
     +					   hrsyn, ier )
			    tau = hradv - hrsyn
C
C*                          Adjust number of times if necessary.
C
                            IF ( nknt .gt. ntimes ) nknt = ntimes
     			      CALL GH_TC2VG ( device, tnam,
     +                              wtype(nstrm), timstr(nstrm),
     +                              wname(nstrm), origc(nstrm),
     +                              wocen(nstrm), wsped(nstrm),
     +                              wadnm(nstrm), wpres (nstrm), 
     +				    sylat(nstrm), sylon(nstrm), 
     +				    sytim(nstrm), tau,
     +                              rlat, rlon,
     +                              NL, nstrm, nknt, fdate,
     +                              wwnd(nstrm), wgst(nstrm), 
     +                              lndext, idisp,
     +                              mxwd, gust, 
     +                              wdir(nstrm),itrack, szmrk,
     +                              ltype, lwidth, lcolor, 
     +                              reset, iret )
			END IF
		    END IF
		END IF
                IF ( tdev(:2) .eq. 'XW' ) CALL TM_WCR ( iret )
                IF ( proces ) THEN
C
C*		    Write the storm name file.
C
		    tnam = '^' // tnam ( :8 ) // '.NAM'
		    CALL FL_SWOP ( tnam, lun, ier )
		    IF ( ier .eq. 0 ) THEN
			CALL ST_LCUC ( wname(nstrm), wname(nstrm), ier1)
		        WRITE (lun,100) wname ( nstrm )
  100			FORMAT (A)
			CALL FL_CLOS ( lun, ier1 )
		      ELSE
			CALL ER_WMSG ( 'GPTCWW', -12, ' ', ier1 )
		    END IF
		END IF
	    END DO
	    IF ( hist .and. ( nstrms .gt. 0 ) ) THEN
C
C*		Update the history file.
C
	        CALL GH_UHST ( 'history_tc', nstrms, storms, advnms, 
     +			       ier )
                IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'GPTCWW', -5, ' ', ier1 )
                END IF
            END IF
C
C*	    Call the dynamic tutor.
C
	    CALL GEPLOT  ( iret )
 	    CALL IP_DYNM ( done, iret )
	END DO
C 
        CALL GH_FTBL ( ier )
        IF ( reset ) CALL GH_RSCL ( ier )
C
	CALL GENDP   ( 1, iret )
	CALL IP_EXIT ( iret )
C*
	END


