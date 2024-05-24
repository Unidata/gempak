	PROGRAM GPTPC
C************************************************************************
C* GPTPC								*
C* This program plots four hurricane graphics for the TPC, excluding    *
C* the track/watch/warn (Kelly) graphic.  These 4 are the wind swath    *
C* graphic, the strike probability graphic, the wind intensity graph    *
C* and the forecast probability table.         				*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01   Copied from GPMAP			*
C* A. Hardy/GSC		 5/01   Added flag for color check		*
C* D. Kidwell/NCEP	 6/01   Moved proces def; added name file;      *
C*				clean up; renamed GPTINP, GPTOPT -> GPP *
C* A. Hardy/GSC		 6/01   Added dissipation argument              *
C* A. Hardy/GSC		 6/01   Added GH_RTBL,GH_SVCL,GH_RSCL		*
C* D. Kidwell/NCEP	 8/01	Split out Kelly graphic                 *
C* D. Kidwell/NCEP	 4/02	Mapped device PS to PSC                 *
C* S. Gilbert/NCEP	 1/06	Drop intermediate advisory extension    *
C* S. Gilbert/NCEP       6/06   Increase length of advnms               *
C* m.gamazaychikov/SAIC	06/06	Added option for setting tzone to HST	*
C* S. Gilbert/NCEP       7/06   Added variable origc                    *
C* m.gamazaychikov/SAIC	06/07	Add wgst and gust to GH_RDAD CS		*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* m.gamazaychikov/SAIC 06/08   Added remnlo to GH_RDAD CS		*
C* X. Guo/CWS           03/10   Used Post-Tropical instead of Extratrop *
C*                              and Remnant Low. Removed remnlo         *
C* B. Hebbard/SDB       02/24   Changed 2-digit-year century break from *
C*                              2025/2026 to 2040/2041 to match others  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        PARAMETER	( NF = 5000, NL = 25, IMAX = 96, JMAX = 60 )
C*
	CHARACTER	device*(LLMXLN), panel*(LLMXLN), 
     +			garea*(LLMXLN), proj*(LLMXLN), 
     +			strnam*(LLMXLN), advno*(LLMXLN),
     +                  strmid*(LLMXLN)
C*
	CHARACTER	imgfls*132, starr(3)*72, tzone*1
	LOGICAL		respnd, done, proces, itest, hist, hvgrid,
     +                  reset
C*
	CHARACTER	wname(NF)*12, timstr(NF)*30, wtype(NF)*2,
     +                  wwnd (NF)*3,wgst (NF)*3,wdir(NF)*4,wsped(NF)*2,
     +                  wpres(NF)*4, wocen(NF)*6, wadnm(NF)*2, 
     +                  windft(4,NF)*60, fdate(NL,NF)*7, origc(NF)*5,
     +                  lndext(NL,NF)*1, option*50, name*7, tdev*12,
     +			storms(NF)*8, advnms(NF)*4, sytim(NF)*20
	REAL		rlat(NL,NF), rlon(NL,NF), grid(IMAX,JMAX),
     +			sylat (NF), sylon (NF)
	INTEGER		mxwd (NL,NF), gust (NL,NF)
        CHARACTER       darr(4)*80, tail*4, tnam*20, devck*12, adnum*3,
     +                  chyy*2
C-----------------------------------------------------------------------
	done = .false.
	reset = .false.
C
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPTPC', -1, ' ', ier )
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
	CALL IP_IDNT ( 'GPTPC', ier )
        CALL GH_RTBL ( ier )
C
	DO WHILE  ( .not. done )
C
C*	    Reset mode back to 1 - map mode.
C
            mode = 1
            CALL GSMODE ( mode, ier )
C	
            itest = .false.
C
C*	    Get input parameters.
C
	    CALL GPPINP  ( strmid, iperr )
	    IF  ( iperr .ne. 0 )  THEN
	        CALL ER_WMSG  ( 'GPTPC', iperr, ' ', ier )
	        CALL SS_EXIT
	    END IF
C
	    CALL ST_CLST ( strmid, '/', ' ', 3, starr, numw, ier )
	    IF  ( starr(1)(1:1) .ne. ' ' )  THEN
	        CALL ST_UCLC ( starr(1), strnam, ier )
              ELSE
                strnam = ' '
	    END IF
            IF ( starr(2)(1:1) .ne. ' ' )  THEN
                CALL ST_NULL ( starr(2), starr(2), lena, ier )
                CALL GH_ADVN ( starr(2), numadv, iadflag, ier )
                CALL ST_INCH ( numadv, advno, ier )
              ELSE
                advno = ' '
	    END IF
C
C*	    Check to see if the history file is to be used.
C
	    IF ( ( strnam .eq. ' ' ) .or. ( advno .eq. ' ' ) ) THEN
		CALL GH_RHST ( 'history', nstrms, storms, advnms, ier )
                IF ( ier .eq. -1 ) THEN
	    	    CALL ER_WMSG ('GPTPC', -5, ' ', ier1)
                  ELSE IF ( ier .eq. -2 ) THEN
	    	    CALL ER_WMSG ('GPTPC', -6, ' ', ier1)
		  ELSE IF ( nstrms .eq. 0 ) THEN
		    CALL ER_WMSG ('GPTPC', 1, ' ', ier1)
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
                IF ( devck (:lens) .eq. 'test' ) THEN
                     device = 'XW'
                     tail = '  '
                     itest = .true.
                  ELSE IF ( devck (:lens) .eq. 'xw' ) THEN
                     device = 'XW'
                     tail = '  '
                  ELSE IF ( ( devck(:lens) .eq. 'ps' ) .or.
     +                      ( devck(:lens) .eq. 'psc' ) ) THEN
                     device = 'PSC'
                     tail = '.psc'
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
C*	    Loop over storm advisories to be processed.
C
	    DO iproc = 1, nstrms
	        IF ( hist ) THEN
		    strnam = storms ( iproc )
		    advno  = advnms ( iproc )(1:3)
		    WRITE (6,*) 'Processing storm ', strnam ( :8 ),
     +				' advisory number ', advno ( :3 )
	        END IF
	        proces = .true.
C
C*		Construct file/title name.
C
                CALL ST_LSTR ( advno, lens, ier)
                IF ( lens .eq. 1 ) THEN
		    adnum = '00' // advno (:lens)
                  ELSE IF ( lens .eq. 2 ) THEN
		    adnum = '0' // advno (:lens)
                  ELSE
		    adnum =  advno (:lens)
                END IF
C
                CALL ST_LSTR( strnam, lenn, ier )
                IF ( lenn .eq. 6 ) THEN
                    CALL ST_NUMB ( strnam(5:6), istrm, ier )
C                   Two-digit-year century break is currently 2040/2041;
C                   will require pushback before then.
                    IF ( istrm .le. 40 ) THEN
                        chyy = '20'
                      ELSE
                        chyy = '19'
                    END IF
                    strnam = strnam(1:4) // chyy // strnam(5:6)
                END IF
C
                CALL ST_LSTR( adnum, lens, ier )
                CALL ST_LSTR( strnam, lenn, ier )
                CALL ST_LSTR( tail, lent, ier )
                tnam = strnam(:lenn) // '?.' //
     +                 adnum (:lens) // tail(:lent)
                CALL ST_LCUC ( tnam, tnam, ier )
                CALL ST_LSTR( tnam, lenm, ier )
C
C*              Read each text advisory and store information.
C
                CALL GH_RDAD ( strnam, advno, NL, wtype, timstr,
     +                         wname,origc,wocen,wadnm,wwnd,wgst,wdir,
     +                         wsped, wpres, rlat, rlon, windft, iwflag, 
     +                         fdate, mxwd, gust, lndext, nstrm, nknt, 
     +                         idisp, sytim, sylat, sylon, iret )
		IF  ( (iret .ne. 0 ) .or. ( nstrm .eq. 0 ) )  THEN
                    proces = .false.
	            CALL ER_WMSG  ( 'GPTPC', -3, ' ', ier )
                END IF
C
C*		Set device and projection.
C
		IF  ( proces )  THEN
                    CALL GSTEXT ( 22, 2, 1.0, 1, 111, 1, 2, ier )
C
C*		    Set map projection.
C		  
                    proj = 'MER'
                    garea = '-10.;-90.;35.;-15.'
		    CALL GG_MAPS ( proj, garea, imgfls, idrpfl, ier )
		    IF  ( ier .ne. 0 )  proces = .false.
C
C*		    Start loop over input image files.
C
		    IF  ( proces )  THEN
C
C*			Display user options, allow program exit.
C
                        CALL GPPOPT ( strmid, ier )
			IF  ( ier .ne. 0 )  proces = .false.
C
			IF  ( proces )  THEN
C
C*			    Set the panel.
C
                            panel = '0'
			    CALL GG_PANL ( panel, iret )
C
C*			    Plot the cumulative tropical storm 
C*                          as the swath graphic.
C
                            IF ( itest ) THEN
                                WRITE(6,*)' '
                                option = 'Enter <cr> to run'
     +                                // ' WIND SWATH graphic '
                                CALL ST_LSTR (option, leno, ier)
             		        CALL TM_CHAR ( option(:leno), 
     +                              .false., .false., 1, name, n, iret )
                              ELSE
                                iret = 1
                                WRITE(6,*)' '
                                WRITE(6,*)'Starting WIND SWATH graphic '
                            END IF
C
                            IF ( iret .eq. 1 ) THEN
                                IF ( tdev(:2) .eq. 'XW' ) THEN
                                    device = tdev // '| SWATH'
				  ELSE
                                    tnam(9:9) = 'S'
                                    device = tdev // '|' // tnam(:lenm)
				END IF
		                CALL GG_SDEV  ( device, ier )
                                IF ( .not. reset ) THEN
                                    CALL GH_SVCL ( ier )
                                    reset = .true.
                                  ELSE
                                    CALL GH_RSCL ( ier )
                                END IF
			        CALL GCLEAR ( iret )
                                CALL GSTANM ( ier )
 			        CALL GH_SWTH ( advno, wtype, timstr,
     +                                         wname, origc, wocen, 
     +                                         wadnm, rlat, rlon, 
     +                                         windft, NL, nstrm, iret)
C
C*			        Flush the graphics buffer.
C
			        CALL GEPLOT ( iret )
				CALL GENANM ( iret )
                            END IF
C
C*			    Plot the strike probability graphic.
C*			    Get the strike probability grid.
C
                            IF ( wocen(nstrm)(:2) .eq. 'AL' ) THEN
                              IF ( itest ) THEN
                                  WRITE(6,*)' '
                                  option = 'Enter <cr> to run STRIKE'
     +                                  // ' PROBABILITY graphic '
                                  CALL ST_LSTR (option, leno, ier)
             		          CALL TM_CHAR ( option(:leno), 
     +                              .false., .false., 1, name, n, iret )
                                ELSE
                                  iret = 1
                                  WRITE(6,*)' '
                                  WRITE(6,*)'Starting STRIKE ' 
     +                                    // 'PROBABILITY graphic '
                              END IF
C
                              IF ( iret .eq. 1 ) THEN
                                  IF ( tdev(:2) .eq. 'XW' ) THEN
                                      device = tdev // '| STRIKE'
				    ELSE
                                      tnam(9:9) = 'P'
                                      device = tdev// '|' //tnam(:lenm)
				  END IF	
		                  CALL GG_SDEV  ( device, ier )
                                  IF ( .not. reset ) THEN
                                      CALL GH_SVCL ( ier )
                                      reset = .true.
                                    ELSE
                                      CALL GH_RSCL ( ier )
                                  END IF
			          CALL GCLEAR ( iret )
                                  CALL GSTANM ( ier )
 			          CALL GH_RPRB ( strnam, advno,
     +					       IMAX, JMAX, grid, iret)
C
C*			          Contour the grid.
C
			 	  IF ( iret .eq. 0 ) THEN
				      hvgrid = .true.
				    ELSE
				      hvgrid = .false.
				      CALL ER_WMSG ( 'GPTPC', -4, ' ',
     +                                             ier )
				  END IF
 			          CALL GH_SPRB ( sytim (nstrm), 
     +				  	wname (nstrm), wadnm (nstrm),
     +					grid, IMAX, JMAX, 
     +					sylat (nstrm), sylon (nstrm), 
     +                                  hvgrid, wdir (nstrm), iret )
C
C*			          Flush the graphics buffer.
C
			          CALL GEPLOT ( iret )
			  	  CALL GENANM ( iret )
                              END IF
                            END IF
C
C*			    Plot the intensity probability graphic.
C
                            IF ( rlon(1,nstrm) .le. -85.0 .and.
     +                           rlon(1,nstrm) .gt. -140.0 ) THEN
                                tzone = 'C'
                              ELSE IF ( rlon(1,nstrm) .le. -140.0 ) THEN
                                tzone = 'H'
                              ELSE 
                                tzone = 'E'
                            END IF
C
                            IF ( itest ) THEN
                                WRITE(6,*)' '
                                option = 'Enter <cr> to run WIND '
     +                                // 'INTENSITY graphic '
                                CALL ST_LSTR (option, leno, ier)
             		        CALL TM_CHAR ( option(:leno), 
     +                              .false., .false., 1, name, n, iret )
                              ELSE
                                iret = 1
                                WRITE(6,*)' '
                                WRITE(6,*)'Starting WIND INTENSITY '
     +                                  // 'graphic '
                            END IF
C
                            mode = 2
                            IF ( iret .eq. 1 ) THEN
                                IF ( tdev(:2) .eq. 'XW' ) THEN
                                    device = tdev // '| INTENSITY'
				  ELSE
                                    tnam(9:9) = 'I'
                                    device = tdev // '|' // tnam(:lenm)
 				END IF
		                CALL GG_SDEV  ( device, ier )
                                IF ( .not. reset ) THEN
                                    CALL GH_SVCL ( ier )
                                    reset = .true.
                                  ELSE
                                    CALL GH_RSCL ( ier )
                                END IF
			        CALL GCLEAR ( iret )
                                CALL ST_NUMB ( wadnm(nstrm), nadnm, ier)
                                CALL GSMODE ( mode, ier)
                                CALL GSTANM ( ier )
 			        CALL GH_WIPL ( timstr(nstrm), 
     +                                     wname(nstrm), origc(nstrm),
     +                                     wocen(nstrm), 
     +                                     wadnm(nstrm), wwnd(nstrm), 
     +                                     fdate, mxwd, lndext, NL, 
     +                                     nstrm, tzone, wtype(nstrm),
     +					   idisp, iret )
C
C*			        Flush the graphics buffer.
C
			        CALL GEPLOT ( iret )
				CALL GENANM ( iret )
                            END IF
C
C*			    Plot the wind speed forecast table.
C
                            IF ( itest ) THEN
                                WRITE(6,*)' '
                                option = 'Enter <cr> to run WIND SPEED '
     +                                // 'TABLE graphic '
                                WRITE(6,*)' '
                                CALL ST_LSTR (option, leno, ier)
             		        CALL TM_CHAR ( option(:leno), 
     +                              .false., .false., 1, name, n, iret )
                              ELSE
                                iret = 1
                                WRITE(6,*)' '
                                WRITE(6,*)'Starting WIND SPEED TABLE'
     +                                  // ' graphic '
                                WRITE(6,*)' '
                            END IF
C
                            IF ( iret .eq. 1 ) THEN
                                IF ( tdev(:2) .eq. 'XW' ) THEN
                                    device = tdev // '| TABLE'
				  ELSE
                                    tnam(9:9) = 'T'
                                    device = tdev // '|' // tnam(:lenm)
				END IF
		                CALL GG_SDEV  ( device, ier )
                                IF ( .not. reset ) THEN
                                    CALL GH_SVCL ( ier )
                                    reset = .true.
                                  ELSE
                                    CALL GH_RSCL ( ier )
                                END IF
			        CALL GCLEAR ( iret )
                                CALL ST_NUMB ( wadnm(nstrm), nadnm, ier)
                                CALL GSTANM ( ier )
 			        CALL GH_WTBL ( timstr(nstrm), 
     +                                   wname(nstrm), origc(nstrm),
     +                                   wadnm(nstrm),
     +                                   tzone, NL, wwnd(nstrm), mxwd,
     +                                   wocen(nstrm), nstrm, iret )
C
C*			        Flush the graphics buffer.
C
			        CALL GEPLOT ( iret)
				CALL GENANM ( iret )
                            END IF
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
			CALL ER_WMSG ( 'GPTPC', -12, ' ', ier1 )
		    END IF
		END IF
	    END DO
	    IF ( hist .and. ( nstrms .gt. 0 ) ) THEN
C
C*		Update the history file.
C
	        CALL GH_UHST ( 'history', nstrms, storms, advnms, ier )
                IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'GPTPC', -5, ' ', ier1)
                END IF
            END IF
C
C*	    Call the dynamic tutor.
C
	    CALL GEPLOT ( iret)
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
