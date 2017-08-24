	SUBROUTINE GH_TCWW ( device, tnam, wtype, timstr, wname, origc, 
     +                       wocen, wsped, wadnm, rlat, rlon, nc,
     +                       nstrm, nknt, fdate,wwnd, lext, idisp, mxwd, 
     +                       wdir, itrack, szmrk, ltype, lwidth, lcolor, 
     +                       idays, iscale, reset, iret )
C************************************************************************
C* GH_TCWW								*
C*									*
C* This subroutine plots the current tropical storm or hurricane        *
C* track and watches/warnings.						*
C*									*
C* GH_TCWW ( DEVICE, TNAM, WTYPE, TIMSTR, WNAME, ORIGC, WOCEN, WSPED,   *
C*	     WADNM, RLAT, RLON, NC, NSTRM, NKNT, FDATE, WWND,LEXT,      *
C*	     IDISP, MXWD, WDIR, ITRACK, SZMRK, LTYPE, LWIDTH, LCOLOR, 	*
C*	     ISCALE, RESET, IRET ) 				        *
C*									*
C* Input parameters:							*
C*	DEVICE		CHAR*		Current device                  *
C*	TNAM		CHAR*		Output filename			*
C* 	WTYPE    	CHAR*		Tropical storm type		*
C*	TIMSTR    	CHAR*		Advisory valid time string	*
C*	WNAME    	CHAR*		Tropical storm name		*
C*	ORIGC    	CHAR*		Issuing center                  *
C*     	WOCEN    	CHAR*		Tropical storm identifier       *
C*	WSPED    	CHAR*		Movement speed (kts)		*
C*	WADNM    	CHAR*		Storm advisory number		*
C*	RLAT (NC,*) 	REAL		Current/forecasted Latitudes	*
C*	RLON (NC,*) 	REAL		Current/forecasted Longitudes	*
C*	NC 		INTEGER		Maximum number of lat/lon pairs *
C*	NSTRM 		INTEGER		Number of decoded files		*
C*      NKNT		INTEGER         Number of forecast points	*
C*      FDATE (NC,*)	CHAR*		Forecast dates			*
C*	WWND		CHAR*		Current storm speed             *
C*	LEXT (NC,*)	CHAR*		Forecast Post-Trop indicators   *
C*	IDISP		INTEGER		Storm dissipating flag		*
C*      MXWD (NC,*)	INTEGER		Forecast max wind speeds        *
C*	WDIR		CHAR*		Direction of present movement   *
C*      ITRACK		INTEGER		Track plot flag			*
C*	SZMRK (2)	REAL		Marker size multiplier		*
C*	LTYPE (2)	INTEGER		Forecast track line type	*
C*	LWIDTH (2)	INTEGER		Forecast track line width	*
C*	LCOLOR (2)	INTEGER		Forecast track line color	*
C*      IDAYS	        INTEGER		3 or 5 day forecast display     *
C*      ISCALE	        INTEGER		Scale legend box plot flag	*
C*                                                                      *
C* Input/Output parameters:						*
C*	RESET		LOGICAL		Color reset flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 5/01   Modified from GG_HRCN			*
C* D. Kidwell/NCEP       6/01   Fixes for bkpt file write; added sounds	*
C* A. Hardy/GSC		 6/01   Diss. check; GQ/SLINE;cleaned up garea  *
C*				calculation and general clean up	*
C* A. Hardy/GSC		 6/01   Added color tag;GH_SAVE, GH_REST	*
C* A. Hardy/SAIC	 8/01   Added PS check,wadnm,upd trak line sz   *
C* D. Kidwell/NCEP       4/02   Formatted write for lunbh; color mods;  *
C*				changed GH_KGLB and GH_KGFL call seqs.  *
C* D. Kidwell/NCEP       4/02   Increased number of sounds from 3 to 5  *
C* D. Kidwell/NCEP       5/02   Changed name of land bounds file        *
C* D. Kidwell/NCEP       3/03   Allowed 3 or 5 day fcst (6 or 8 times)  *
C* D. Kidwell/NCEP       3/03   Used dashed fill, dashed line, smaller  *
C*				points for fcst days 4 and 5            *
C* D. Kidwell/NCEP       4/03   Changed breakpoint plotting calls       *
C* D. Kidwell/NCEP       4/03   Added error check for bad wocen value   *
C* J. Wu/SAIC       	 8/03   Added param. NKNT to GH_KGLB/GH_KGLP	*
C* D. Kidwell/NCEP	11/03	Added arg zone2; use mult. bkpt files   *
C* D. Kidwell/NCEP	 1/04	Incr. ibkel, istsnd; added mxwd & wdir; *
C*				CSC for GH_BKIN, GH_BKRC, GH_BKUS       *
C* D. Kidwell/NCEP       4/04   Added jflags, CSC for GH_BKLP, GH_BKLB  *
C* m.gamazaychikov/SAIC	03/04	From GH_KGPH				*
C* m.gamazaychikov/SAIC	05/04	Made VGF file all lower case		*
C* m.gamazaychikov/SAIC	08/04	Add numbkp and icnt, change breakpoint 	*
C*				plotting calls				*
C* B. Yin/SAIC		12/04	Added time zone in gh_bkrv		*
C* m.gamazaychikov/SAIC	01/05	Added itrack and szmrk to CS		*
C* m.gamazaychikov/SAIC	01/05	Changed the total number of breakpoints *
C*				for map area calculations		*
C* m.gamazaychikov/SAIC	03/05	Added ltype, lwidth, lcolor and ilegend	*
C*				to CS to plot with user specified parms	*
C* B. Yin/SAIC		04/05	Added issue status into gh_bkrv		*
C* S. Gilbert/NCEP	12/05	Added check to set hvbkpt to false when *
C*                              GH_BKRV returns icount == 0 (no bkpts)  *
C* S. Gilbert/NCEP	01/06	Handle intermediate advsry nos in filnam*
C* S. Gilbert/NCEP	07/06	added new argument origc, and added     *
C*                              central Pacific case                    *
C* S. Gilbert/NCEP	03/07	removed check for west of 180 deg lon   *
C* m.gamazaychikov/SAIC	04/08	Added lext and remn to CS, added two 	*
C*				tropical cyclone intensity category, 	*
C*				changed the plotting of the track	*
C* m.gamazaychikov/SAIC	06/08	Fixed the bug in setting flag to plot   *
C*                              string "Extratropical" in legend box	*
C* m.gamazaychikov/SAIC	09/08	Added breakpoints lat lon extraction,	*
C*				added them to GH_KGLP CS, increased the *
C*                              number of iterations to 15, improved	*
C*                              calculation of min max of the map,	*
C*                              set the GF, PS dims to 895;716		*
C* m.gamazaychikov/SAIC	10/08	Fixed AL basin positive longitude bug   *
C* S. Jacobs/NCEP	 8/09	Increase size of tdev to include dims	*
C* X. Guo/CWS		03/10   Change extratropical to post-tropical	*
C* X. Guo/CWS		03/10   Moved legend box underneath the graphic *
C*                              and removed unused parameters           *
C* X. Guo/CWS		04/10   Fixed POST-TROP point display problem   *
C* X. Guo/CWS		05/10   Changed "DISSIPATING" to "DISSIPATED"   *
C* X. Guo/CWS           05/10   Added idays to GH_TCLB                  *
C* S. Jacobs/NCEP	 6/10	Re-added flag for plotting the scale	*
C* S. Jacobs/NCEP	 4/13	Added smoothing for the track and cone	*
C* D. Zelinsky/NCEP/NHC 10/15   Added time zones (P/M/C) to EastPac     *
C* M. Sardi/NCEP/NHC    11/15   Above change required fix to ensure     *
C*                              GH_KGAR only called for AL systems.     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	PARAMETER	( NUMHR = 8, MXPTS = 500 )
	PARAMETER	( NUMH2 = NUMHR - 2 )
C*
	CHARACTER*(*)	device, wname, timstr, wtype, wsped, tnam, 
     +                  wocen, fdate(nc,*), wwnd, wadnm, wdir, origc,
     +                  lext (nc,*)
	INTEGER		mxwd (nc,*), ltype (*), lwidth (*), lcolor (*)
        REAL		rlat(nc,*), rlon(nc,*), szmrk (*)
	LOGICAL		reset
C*
	CHARACTER	table*128, tbtyp*128, bgsea*180, bgland*180, 
     +                  mapstr*128, latlon*128, ddev*12, cblk*2, 
     +                  ddate(NUMHR)*20, tzone, filnam*40,
     +                  tdev*40, noprob*180, disstr*30, bglake*180,
     +                  coltag*33, cmap*3, namstr*36,
     +			adnum*4, cyint (5), bpnam(100)*33, stnum*4,
     +			yyyy*4, stname*32, vtime*10, bpstr*3300,
     +			zone*4, status, cadvnm*5,  bklntp*4, dims*7
C*
	INTEGER	        numbkp(50),numb(4), ibkpts(4,100),
     +                  iarea(4,100),isev(50), iadvtp(50),
     +                  igeog(50), icnt (4, 50), kpos(MXPTS),
     +                  ibkcur(MAXBK), iarcur(MAXBK), nppart(20)
C*
        REAL            alat(NUMHR), alon(NUMHR),
     +			slnlat(MXPTS), slnlon(MXPTS), 
     +			xkey (5), ykey (5),
     +			slnlt6(MXPTS), slnln6(MXPTS), dist(NUMHR),
     +			bplat(100), bplon(100),
     +                  lbklat(20,MAXBK), lbklon(20,MAXBK),
     +                  bkplat(MAXBK), bkplon (MAXBK),
     +			blat(MXPTS), blon(MXPTS), bdst(MXPTS),
     +			bx(MXPTS), by(MXPTS), dx(NUMHR), dy(NUMHR)
C*
	LOGICAL		hvbkpt, done, finish, plotit, extra
C*
	INCLUDE		'ERMISS.FNC'
C*
	DATA		cyint / 'M', 'H', 'S', 'D', 'L'/
	DATA		dims / '895;716'/
C-----------------------------------------------------------------------
	iret = 0
C
C*	Query current settings.
C
	CALL GH_SAVE ( ier )
C
	finish = .false.
	extra = .false.
        ddev = 'XW'
        filnam = ' '
        yyyy = ' '
        stname = ' '
        bklntp = ' ' 
        stnum = ' '
        vtime = ' '
        bpstr = ' '
        DO ibpnam = 1, 100
            bpnam(ibpnam) = ' '
        END DO
        DO ii = 1, MAXBK
            bkplat (ii) = RMISSD
            bkplon (ii) = RMISSD
        END DO
C
C*      Set the flag for post-tropical line in legend box
C
        ii = 1
        ipost = 0
        DO WHILE ( (.not. extra) .and. (ii .le. nknt) )
           IF ( lext(ii,nstrm) .eq. 'P' ) THEN
              ipost = 1
              extra = .true.
           END IF
           ii = ii + 1
        END DO
        CALL ST_LSTR ( tnam, lenm, ier )
        tdev= ddev // '| TRACK-W/W |' // dims
	CALL GG_SDEV ( tdev, ier )
        IF ( .not. reset ) THEN
            CALL GH_SVCL ( ier )
            reset = .true.
          ELSE
            CALL GH_RSCL ( ier )
        END IF
        CALL GSTANM ( ier )
	DO WHILE ( .not. finish )
C
C*          Reverse black and white if device is for postscript.
C
            CALL GQDEV  ( ddev, iunit, iatyp, ier )
            IF  ( ddev(:2) .eq. 'PS' )  THEN
                iwht = 32
                iblk = 1
              ELSE
                iwht = 31
                iblk = 32
            END IF
	    IF  ( ddev ( :2) .ne. 'XW' ) THEN
	        finish = .true.
	    END IF
C
C*	    Set symbol and text attributes.
C
	    CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
	    isytyp = 17
            ssize  = 1.0
            iwidth = 2
C
C*          Read in average track error file. 
C
	    iertb = 0
            IF ( wocen(1:2)  .eq. 'AL' ) THEN
                table = 'altker.tbl'
                IF ( rlon(1,nstrm) .le. -85.0 ) THEN
                    tzone = 'C'
                  ELSE 
                    tzone = 'E'
                END IF
              ELSE IF ( wocen(1:2)  .eq. 'EP' ) THEN
                table = 'eptker.tbl'
                IF ( rlon(1,nstrm) .gt. -106.0 ) THEN
                    tzone = 'C'
                  ELSE IF ( rlon(1,nstrm) .gt. -115.0 ) THEN
                    tzone = 'M'
                  ELSE
                    tzone = 'P'
                END IF

C*               Used to be always Pacific
C                tzone = 'P'
              ELSE IF ( wocen(1:2)  .eq. 'CP' ) THEN
                table = 'cptker.tbl'
                tzone = 'H'
	      ELSE
		iertb = -10
            END IF
            IF ( origc(1:4)  .eq. 'CPHC' ) THEN
                table = 'cptker.tbl'
                tzone = 'H'
            END IF
C
	    IF ( iertb .eq. 0 ) THEN
                tbtyp = 'hcnadv'
C
C*              Read the average track error file.
C
                CALL GH_KGTB ( table, tbtyp, NUMHR, dist, iertb )
	    END IF
	    IF ( iertb .ne. 0 ) THEN
		CALL ER_WMSG ( 'GPTCWW', iertb, ' ', ierr )
	    END IF
C
            im    = nstrm
            nltln = 0
            jmax  = NUMHR
	    IF ( nknt .le. NUMH2 ) jmax = NUMH2
C
            IF ( ERMISS ( rlat(1,im) ) .or. ERMISS ( rlon(1,im) ) ) THEN
                im = im - 1
                jmax = 1
            END IF
C
C*          Find the average track error lat. lon. points.
C
            DO jj = 1, jmax
                IF ( ( .not. ERMISS ( rlat (jj,im) ) ) .and.
     +               ( .not. ERMISS ( rlon (jj,im) ) ) ) THEN
                    nltln = nltln + 1
                    alat (nltln) = rlat(jj,im)
                    alon (nltln) = rlon(jj,im)
		    IF ( jj .gt. 1 ) ddate (nltln) = fdate(jj-1,im)
                END IF
            END DO
C
C*	    Smooth the central track line and the track error distances.
C
	    CALL GTRANS ( 'M', 'D', nltln, alat, alon, dx, dy, iergt )
	    dens = 5.0
	    cscl = 30.0
	    is = 0
	    ie = nltln+1
	    CALL CV_PRM3 ( nltln, dx, dy, dist, dens, MXPTS, cscl,
     +			   is, ie, nout, bx, by, bdst, iercv )
	    CALL GTRANS ( 'D', 'M', nout, bx, by, blat, blon, iergt )
C
C*	    Find the original points in the smoothed line.
C
	    knumh2 = nout
	    DO  kk = 1, nout
	        kpos(kk) = 0
		DO  mm = 1, nltln
		    IF  ( ( ABS(blat(kk)-alat(mm)) .lt. 0.0005 ) .and.
     +			  ( ABS(blon(kk)-alon(mm)) .lt. 0.0005 ) )  THEN
			kpos(kk) = mm
			IF ( mm .eq. NUMH2 ) knumh2 = kk
		    END IF
		END DO
	    END DO
C
C*	    Calculate the area under the cone.
C
            IF ( iertb .ge. 0 ) THEN
		plotit = .true.
		inner  = MIN0 ( nout, knumh2 )
                CALL GH_KGAT ( blat, blon, inner, bdst, MXPTS, slnlt6, 
     +                         slnln6, numln6, ier )
		IF ( nout .gt. knumh2 ) THEN
                    CALL GH_KGAT ( blat, blon, nout, bdst, MXPTS, 
     +                             slnlat, slnlon, numln, ier )
		  ELSE
		    numln = numln6
		    DO jj = 1, numln
			slnlat ( jj ) = slnlt6 ( jj ) 
			slnlon ( jj ) = slnln6 ( jj )
		    END DO
		END IF
C
C*              Determine graphics area.
C
                DO jj = 1, numln
		    IF ( jj .eq. 1 ) THEN
                        xmxlat = slnlat(jj) 
                        xmnlat = slnlat(jj) 
                        IF ( slnlon(jj) .gt. 0 .and. 
     +                       wocen(1:2)  .ne. 'AL' ) THEN
C
C*                         Subtruct 360 for non-Atlantic storms
C
                           xmxlon = slnlon(jj) - 360.
                           xmnlon = slnlon(jj) - 360.
                        ELSE
                           xmxlon = slnlon(jj)
                           xmnlon = slnlon(jj)
                        ENDIF
		      ELSE
                        IF ( .not. ERMISS ( slnlon(jj) ) ) THEN
                            xmxlat = AMAX1 ( xmxlat, slnlat(jj) )
                            xmnlat = AMIN1 ( xmnlat, slnlat(jj) )

                            IF ( slnlon(jj) .gt. 0 .and. 
     +                           wocen(1:2)  .ne. 'AL' ) THEN
C
C*                             Subtruct 360 for non-Atlantic storms
C
                               xmxlon = AMAX1( xmxlon, slnlon(jj)-360.)
                               xmnlon = AMIN1( xmnlon, slnlon(jj)-360.)
                            ELSE
                               xmxlon = AMAX1 ( xmxlon, slnlon(jj) )
                               xmnlon = AMIN1 ( xmnlon, slnlon(jj) )
                            ENDIF
                        END IF
		    END IF
                END DO
C
C*		Save off the max/min lat/lon for the track error for 
C*              use in determining the label box placement.
C
                tmxlat =  xmxlat + 2.0
                tmnlat =  xmnlat - 2.0 
                tmxlon =  xmxlon + 5.0 
                tmnlon =  xmnlon - 5.0 
C
C*		Set up track error polygon.
C
                IF ( ( ABS( tmxlat - tmnlat) .lt. 10.0 ) 
     +                 .or.  ( ABS( tmxlon - tmnlon) .lt. 10.0 ) ) THEN
                    xmxlat =  tmxlat 
                    xmnlat =  tmnlat 
                    xmxlon =  tmxlon 
                    xmnlon =  tmnlon 
                  ELSE
                    xmxlat =  xmxlat + 3.0
                    xmnlat =  xmnlat - 3.0 
                    xmxlon =  xmxlon + 6.0 
                    xmnlon =  xmnlon - 6.0 
                END IF
C
                IF ( tzone .eq. 'H')  THEN
C
C*                  Make Sure Hawaiian islands are plotted
C
                    rmxlat =  xmxlat 
                    IF ( rmxlat .lt. 24.0 ) rmxlat = 24.0
                    rmnlat =  xmnlat 
                    IF ( rmnlat .gt. 18.0 ) rmnlat = 18.0
                    rmnlon =  180.
                    rmnlon =  xmnlon
                    IF ( rmnlon .gt. -162. ) rmnlon = -162.0
                    rmxlon =  -140.
                    rmxlon =  xmxlon
                    IF ( rmxlon .lt. -152. ) rmxlon = -152.0
                ELSE IF ( wocen(1:2)  .eq. 'EP' )  THEN
                    rmxlat =  xmxlat 
                    rmnlat =  xmnlat 
                    rmxlon =  xmxlon 
                    rmnlon =  xmnlon 
                ELSE
                    CALL GH_KGAR (xmnlat, xmnlon, xmxlat, xmxlon, 
     +                            rmnlat, rmnlon, rmxlat, rmxlon, ier )
		END IF
C
C*              Check for breakpoints.
C
                hvbkpt = .true.
C
C*              Get the name of VGF file containing breakpoints.
C
                CALL ST_LSTR ( wocen, lenocn, ier )
                CALL ST_NULL ( wadnm, wadnm, lena, ier )
                CALL GH_ADVN ( wadnm, numadv, iadflag, ier )
                CALL TI_C2I  (timstr, iyyyy, immdd, ihhmm, ier )
                IF ( numadv .gt. 0 ) THEN
                    numadv = numadv + 1000
                    CALL ST_INCH ( numadv, adnum, ier )
                    CALL ST_INCH ( iyyyy, yyyy, ier )
                    CALL ST_UCLC ( wocen (:4), stnum, ier) 
                    IF ( iadflag .eq.0 ) THEN
                        filnam = 'tca_' // stnum // yyyy //'_'//
     +                            adnum ( 2:4 ) //'.vgf'
                    ELSE IF ( iadflag .eq.1 ) THEN
                        filnam = 'tca_' // stnum // yyyy //'_'//
     +                            adnum ( 2:4 ) //'a.vgf'
                    ELSE IF ( iadflag .eq.2 ) THEN
                        filnam = 'tca_' // stnum // yyyy //'_'//
     +                            adnum ( 2:4 ) //'b.vgf'
                    ELSE
                        filnam = ' '
                    END IF
                  ELSE
                    filnam = ' '
                END IF
                CALL ST_NULL (filnam, filnam, ilen, iret)
C
C*              Get the breakpoint information from the VGF file
C
                CALL GH_BKRV ( filnam, istnum, iyear, ibasin, cadvnm,
     +                         isstyp, stname, vtime, icount, 
     +                         isev,  iadvtp, igeog, 
     +                         bplat, bplon, bpstr, numbkp, zone,
     +                         status, iret)
                IF ( iret .eq. -1) THEN
		    CALL ER_WMSG ( 'GPTCWW', 4, ' ', ierr )
                    hvbkpt = .false.
                ELSEIF ( icount .EQ. 0 ) THEN
                    hvbkpt = .false.
                ELSE
C
C*                  Separate name string into an array of strings
C
                    CALL ST_CLSL ( bpstr, ';', ' ', 100,
     +                             bpnam, nbpn, iret)
C
C*                  Get the information to plot breakpoints
C
                    CALL GH_BKGB ( icount, isev, iadvtp, bpnam, numbkp,
     +                             ibkpts, numb, iarea, icnt, iret) 
C
                    ntbkpt = 0
C
C*                  Get the lat lon of all the breakpoint segments
C
                    DO inumb = 1, 4
                      IF ( numb (inumb) .gt. 0 ) THEN
                         indbkc = 1
                         indbkp = 1
C
C*                       Start loop over the number of segments
C
                         DO ilines = 1, numb(inumb)
                          inpseg = icnt (inumb,indbkc)
C
C*                        Zero out the breakpoint and area arrays
C*                        for current segment
C
                          DO ipts = 1, inpseg
                             ibkcur(ipts) = 0
                             iarcur(ipts) = 0
                          END DO
                          indb2=1
C
C*                        Start loop over the segments number of points
C
                          DO ipts = 1, icnt (inumb,indbkc)
                             ibkcur(indb2) = ibkpts(inumb, indbkp)
                             iarcur(indb2) = iarea(inumb, indbkp)
                             IF ( iarcur(indb2) .eq. IUSGEC .or.
     +                            iarcur(indb2) .eq. IMXCSA .or.
     +                            iarcur(indb2) .eq. IPACIF .or.
     +                            iarcur(indb2) .eq. IKEYS ) THEN
                                 bklntp = "pair"
                              ELSE IF ( iarcur(indb2) .eq. ICUBA .or.
     +                                  iarcur(indb2) .eq. IHISP.or.
     +                                  iarcur(indb2) .eq. IPRICO ) THEN
                                 bklntp = "ordr"
                              ELSE IF ( iarcur(indb2) .eq. IOTHER .or.
     +                                  iarcur(indb2) .eq. IWATER ) THEN
                                 bklntp = "list"
                                 CALL GH_GBKL ( ibkcur(indb2),
     +                                          iarcur(indb2),
     +                                          1, bklntp,
     +                                          nparts, nppart,
     +                                          lbklat, lbklon,
     +                                          iret)
                                 DO inparts = 1, nparts
                                   npline = nppart(inparts)
                                   DO ibk= 1, npline
                                     bkplat(ntbkpt)=lbklat(inparts,ibk)
                                     bkplon(ntbkpt)=lbklon(inparts,ibk)
                                     ntbkpt = ntbkpt + 1
                                   END DO
                                END DO
                             END IF
                          indb2 = indb2+1
                          indbkp = indbkp + 1
C
C*                        End of POINTS loop
C
                          END DO

                          IF  ( bklntp .eq. "ordr" .or.
     +                          bklntp .eq. "pair" ) THEN
                                CALL GH_GBKL ( ibkcur, iarcur,
     +                                         inpseg, bklntp,
     +                                         nparts, nppart,
     +                                         lbklat, lbklon,
     +                                         iret)
                                npline = nppart(1)
                                DO ibk =1, npline
                                   bkplat(ntbkpt) = lbklat(1,ibk)
                                   bkplon(ntbkpt) = lbklon(1,ibk)
                                   ntbkpt = ntbkpt + 1
                                END DO
                          END IF

                          DO ipts = 1, inpseg
                              ibkcur(ipts) = 0
                              iarcur(ipts) = 0
                          END DO
                          indbkc = indbkc + 1
C
C*                       End of SEGMENTS loop
C
                         END DO
                      END IF
C
C*                  End of NUMB loop
C
                    END DO
                    ntbkpt = ntbkpt - 1
C
C*                  Get the updated max and min for the map.
C
                    amnlat = bplat(1)
                    amnlon = bplon(1)
                    amxlat = bplat(1)
                    amxlon = bplon(1)
                    DO ii = 2, ntbkpt
                        amnlat = AMIN1 ( amnlat, bkplat(ii) )
                        amnlon = AMIN1 ( amnlon, bkplon(ii) )
                        amxlat = AMAX1 ( amxlat, bkplat(ii) )
                        amxlon = AMAX1 ( amxlon, bkplon(ii) )
                    END DO
                    amxlat =  amxlat + 1.0
                    amnlat =  amnlat - 1.0 
                    amxlon =  amxlon + 2.0 
                    amnlon =  amnlon - 2.0 
                    rmnlat = AMIN1 ( rmnlat, amnlat )
                    rmnlon = AMIN1 ( rmnlon, amnlon )
                    rmxlat = AMAX1 ( rmxlat, amxlat )
                    rmxlon = AMAX1 ( rmxlon, amxlon )
                END IF
C
C*              Set up map.  Make sure there is enough room for the
C*		label box (key).  Make 6 attempts before quitting.  In
C*		that case, the label box defaults to the lower left 
C*		corner.
C
		done = .false.
		iter = 1 
C
		DO WHILE ( .not. done )
C
		    CALL GH_KGMP ( rmnlat, rmnlon, rmxlat, rmxlon, 
     +			           fmnlat, fmnlon, fmxlat, fmxlon,
     +			           dellon, ier )
		    CALL GH_KGLP ( wtype, wname, fmnlat, fmnlon,
     +                             fmxlat,fmxlon,
     +				   namstr, xkey, ykey, ier )
		    IF ( ier .eq. 0 .or. iter .gt. 15 ) THEN
			done = .true.
		      ELSE 
			iter = iter + 1
			rmnlat = rmnlat - 1.
			rmnlon = rmnlon - 1.
			rmxlat = rmxlat + 1.
			rmxlon = rmxlon + 1.
                        if ( rmxlat .ge. 85. ) rmxlat = 85.
		    END IF
		END DO
C
C*              Do the map boundary fill.
C
		coltag = 'w_water_mask'
        	CALL ST_LSTR ( coltag, lens, ier )
        	CALL GH_COLR ( coltag(:lens), 27, ier )
        	CALL GQCOLR ( icolr, ier )
        	CALL ST_INCH ( icolr, cmap, ier )
        	CALL ST_LSTR ( cmap, lens, ier )
        	bgsea =  'bg/' // cmap(:lens) // '//1' 
        	CALL GG_BND ( bgsea, ier )
C
        	coltag = 'w_land_mask'
        	CALL ST_LSTR ( coltag, lens, ier )
        	CALL GH_COLR ( coltag(:lens), 22, ier )
        	CALL GQCOLR ( jcolr, ier )
        	CALL ST_INCH ( jcolr, cmap, ier )
        	CALL ST_LSTR ( cmap, lens, ier )
        	bgland = 'hcn_bnds/' // cmap(:lens) // '//1'
        	CALL GG_BND ( bgland, ier )
C
        	coltag = 'w_water_mask'
        	CALL ST_LSTR ( coltag, lens, ier )
        	CALL GH_COLR ( coltag(:lens), 27, ier )
        	CALL GQCOLR ( icolr, ier )
        	CALL ST_INCH ( icolr, cmap, ier )
        	CALL ST_LSTR ( cmap, lens, ier )
        	bglake = 'lakes/' //cmap(:lens)
        	CALL GG_BND ( bglake, ier )
C
C*	        Plot the average track error line.
C
                IF ( idisp .ge. 0 ) THEN
                    iatclr = 1
                    dens = 1.5
	            CALL GSCOLR ( iblk, ier )
                    CALL GSLINE ( 1, 0, 3, 0, ier )
                    CALL GSSMTH ( 2, dens, ier )
		    IF ( nout .gt. knumh2 ) THEN
C
C*			Use slanted dashed line fill for 5 day forecast.
C
	                CALL GSCOLR ( iwht, ier )
                        CALL GSFILL ( 1.5, 2, ier )
     	                CALL GLINE ( 'M', numln, slnlat, slnlon, ier )
     	                CALL GFILL ( 'M', numln, slnlat, slnlon, ier )
	                CALL GSCOLR ( iblk, ier )
		    END IF
C
C*		    Use solid fill for 3 day forecast.
C
                    CALL GSFILL ( 1.5, 1, ier )
     	            CALL GLINE ( 'M', numln6, slnlt6, slnln6, ier )
	            CALL GSCOLR ( iwht, ier )
     	            CALL GFILL ( 'M', numln6, slnlt6, slnln6, ier )
                END IF
	      ELSE
		plotit = .false.
		hvbkpt = .false.
		finish = .true.
            END IF
C
            IF ( hvbkpt ) THEN
C
C*              Plot the breakpoints for the watches and warnings.
C
                CALL GH_BKPL ( ibkpts, numb, ddev, iarea, icnt, iret)
            END IF
C
	    IF ( plotit ) THEN
C
C*              Draw map.
C
                CALL ST_INCH ( iblk, cblk, ier )
                CALL ST_LSTR ( cblk, lens, ier )
                latlon = cblk(:lens) // '/1/1//5;5//2'
                mapstr = cblk(:lens) // '/1/1'
C
 	        CALL IP_SVAR ( '$MAPFIL=hipowo.cia', ier )
                CALL GG_MAP  ( mapstr, ier )
C
C*              Draw latitude/longitude lines.
C
     	        CALL GG_LTLN ( latlon, ier )
C
C*              Plot state ids and country names.
C
                CALL GH_KGST ( iblk, ier )
C
C*	        Plot the label box.
C
                CALL GH_TCLB ( timstr, wtype, wname, wsped, wadnm, wwnd,
     +                       alat, alon, fmnlat, fmnlon, fmxlat,fmxlon,
     +                       tzone, iwht, iblk, idisp, namstr,
     +                       xkey, ykey, nknt, wdir,idays, iscale,
     +			     disstr, ier)
	    END IF
C
C*          Plot text for dissipating storm.
C
            IF ( plotit .and. ( idisp .ne. 0 ) ) THEN
                CALL ST_LSTR ( disstr, lent, ier )
                CALL ST_LSTR ( wname, lennam, ier )
                noprob = 'TROPICAL DEPRESSION ' // wname(:lennam)
     +                   // ' DISSIPATED AT ' // disstr(:lent)
                CALL ST_LCUC ( noprob, noprob, ier )
                CALL ST_LSTR ( noprob, lens, ier )
                CALL GSCOLR ( iblk, ier )
                CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 2, ier )
 	        CALL GTEXT ( 'N', .5, .5, noprob (:lens), 0.0, 
     +                       0, 0, ier )
            END IF
C
C*	    Plot the forecasted track. 
C*	    Set the color and symbol for the current location.
C
            IF ( plotit .and. ( idisp .ge. 0 ) ) THEN
		ip = nstrm
C
C*		Draw line to connect the points.
C
                IF ( itrack .eq. 1 ) THEN
                   CALL GSSMTH ( 0, dens, ier )
                   iline = lwidth (1)
                   ilcol = lcolor (1)
                   IF  ( ddev(1:2) .eq. 'PS' ) THEN 
                      iline = 2 * lwidth (1) + lwidth (1) / 2 
                      IF ( ilcol .eq. 32 ) ilcol = 1 
                      IF ( ilcol .eq. 31 ) ilcol = 32 
		   END IF
                   CALL GSLINE ( ltype (1), 0, iline, 0, ier )
		   n3 = MIN0 ( nout, knumh2 )
                   CALL GSCOLR ( ilcol, ier )
                   CALL GLINE ( 'M', n3, blat, blon, ier )
		   IF ( nout .gt. knumh2 ) THEN
                      iline = lwidth (2)
                      ilcol = lcolor (2)
                      IF  ( ddev(1:2) .eq. 'PS' ) THEN 
                         iline = 2 * lwidth (2) + lwidth (2)/2 
                         IF ( ilcol .eq. 32 ) ilcol = 1 
                         IF ( ilcol .eq. 31 ) ilcol = 32 
		      END IF
                      CALL GSLINE ( ltype (2) , 0, iline, 0, ier )
		      next = nout - knumh2 + 1
                      CALL GSCOLR ( ilcol, ier )
		      CALL GLINE ( 'M', next, blat(knumh2), blon(knumh2),
     +				 ier )
		   END IF
                   CALL GSCOLR ( iblk, ier )
		END IF
C
C*              Plot the forecast points first.
C
                szm = szmrk(1)
                DO kk = 2, nknt
                    IF ( kk .eq. ( NUMH2 + 1 ) ) isize = szmrk(2)
C
C*                  For post-tropical storms
C
                    IF ( lext(kk-1,nstrm) .eq. 'P' ) THEN
                       sz2= szm - 0.5 
                       CALL GSCOLR ( iblk, ier ) 
                       CALL GSMRKR ( 17, 0, szm, 2, ier )
                       CALL GMARK ('M',1,rlat(kk,ip),rlon(kk,ip),ier)
                       CALL GSCOLR ( iwht, ier ) 
                       CALL GSMRKR ( 17, 0, sz2, 2, ier )
                       CALL GMARK ('M',1,rlat(kk,ip),rlon(kk,ip),ier)
C
C*                   For tropical storms
C
                     ELSE 
                       CALL GSMRKR ( 17, 0, szm, 2, ier )
                       CALL GSCOLR ( iblk, ier )
                       CALL GMARK ('M',1,rlat(kk,ip),rlon(kk,ip),ier)
                    END IF
                END DO
C
C*              Plot the current location markers next.
C
                CALL GSCOLR ( iblk, ier ) 
                sym = 3.
                CALL GSSPCL ( 1.2, 1, ier )
                CALL GSPCL ( 'M', 1, sym, rlat(1,ip), rlon(1,ip), 0, 0,
     +                       ier )
		coltag = 'w_lbl_curr_pos_mkr'
        	CALL ST_LSTR ( coltag, lens, ier )
 		CALL GH_COLR ( coltag(:lens), 17, ier )
                CALL GSSPCL ( ssize, iwidth, ier )
                CALL GSPCL ( 'M', 1, sym, rlat(1,ip), rlon(1,ip), 0, 0,
     +                       ier )
C
                CALL GSCOLR ( iblk, ier )
                CALL GSMRKR ( 17, 0, .8, 2, ier )
                CALL GMARK ( 'M', 1, rlat(1,ip), rlon(1,ip), ier )
C
C*              Plot current location and forecast track labels.
C
                ddate(1) = timstr
                CALL GH_KGFL ( alat, alon, ddate, nltln, tzone, dellon,
     +			       xkey, ykey, ier )
C
C*              Plot the letter for the storm intensity at each forecast
C*		point.
C
                CALL GSTEXT ( 22, 2, .7, 1, 111, 1, 2, ier )
                DO kk = 2, nknt
C
C*                  Set the color of the letter to:
C*                  white - for tropical storms
C*                  black - for extratropical storms
C
                    CALL GSCOLR ( iwht, ier )
                    IF ( lext(kk-1, nstrm) .eq. 'P' ) THEN
                      CALL GSCOLR ( iblk, ier ) 
                    END IF
		    IF ( mxwd ( kk - 1, ip ) .gt. 96 ) THEN
		       ii = 1
		    ELSE IF ( mxwd ( kk - 1, ip ) .ge. 64 ) THEN
		        ii = 2
		    ELSE IF ( mxwd ( kk - 1, ip ) .ge. 34 ) THEN
			ii = 3 
		    ELSE
			ii = 4
		    END IF		
                    CALL GTEXT ( 'M', rlat(kk,ip), rlon(kk,ip), 
     +				 cyint ( ii ), 0., 0, 0, ier )
                END DO
                CALL GSCOLR ( iblk, ier )
            END IF
C
C*	    Flush the graphics buffer.
C
	    CALL GEPLOT ( ier )
            CALL GENANM ( ier )
C
C*	    Check for an acceptable display.
C
	    IF ( .not. finish ) THEN
                IF ( device(:2) .eq. 'XW' ) THEN
                    finish = .true.
                ELSE
                    ddev = device
                    CALL ST_LSTR(tnam, lenm, ier )
                    CALL ST_LSTR(ddev, lend, ier )
                    device = ddev(:lend) // '|'
     +                    // tnam(:lenm)//'|'//dims
                    CALL GG_SDEV ( device, ier )
                    CALL GSTANM ( ier )
                END IF
	    END IF
	END DO
C
C*	Reset the saved attributes.
C
	CALL GH_REST ( ier )
C*
	RETURN
	END
