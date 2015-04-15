	SUBROUTINE GH_TC2VG ( device, tnam, wtype, timstr, wname, origc, 
     +                       wocen, wsped, wadnm, wpres, sylat,sylon,
     +			     sytim, tau0, rlat, rlon,
     +                       nc, nstrm, nknt, fdate,wwnd, wgst, lext,
     +                       idisp, mxwd, gust,wdir, itrack, szmrk,
     +                       ltype, lwidth, lcolor, reset, iret )
C************************************************************************
C* GH_TC2VG								*
C*									*
C* This subroutine writes the current tropical storm or hurricane       *
C* track, error cone  and watches/warnings to three VG files.		*
C*									*
C* GH_TC2VG ( DEVCIE, TNAM, WTYPE, TIMSTR, WNAME, ORIGC, WOCEN, WSPED,  *
C*	     WADNM, WPRES,RLAT,RLON,NC,NSTRM,NKNT,FDATE,WWND,WGST,      *
C*           LEXT,IDISP, MXWD,WGST,WDIR,ITRACK,SZMRK,LTYPE,LWIDTH,      *
C*	     LCOLOR, RESET, IRET )	         			*
C*									*
C* Input parameters:							*
C*      DEVICE          CHAR*           Current device                  *
C*	TNAM		CHAR*		Output filename			*
C* 	WTYPE    	CHAR*		Tropical storm type		*
C*	TIMSTR    	CHAR*		Advisory valid time string	*
C*	WNAME    	CHAR*		Tropical storm name		*
C*	ORIGC    	CHAR*		Issuing center                  *
C*     	WOCEN    	CHAR*		Tropical storm identifier       *
C*	WSPED    	CHAR*		Movement speed (kts)		*
C*	WADNM    	CHAR*		Storm advisory number		*
C*	WPRES    	CHAR*		Storm center pressure		*
C*	RLAT (NC,*) 	REAL		Current/forecasted Latitudes	*
C*	RLON (NC,*) 	REAL		Current/forecasted Longitudes	*
C*	NC 		INTEGER		Maximum number of lat/lon pairs *
C*	NSTRM 		INTEGER		Number of decoded files		*
C*      NKNT		INTEGER         Number of forecast points	*
C*      FDATE (NC,*)	CHAR*		Forecast dates			*
C*	WWND		CHAR*		Current storm speed             *
C*	WGST		CHAR*		Current storm wind gust         *
C*      LEXT (NC,*)     CHAR*           Forecast Post-Trop indicators   *
C*	IDISP		INTEGER		Storm dissipating flag		*
C*      MXWD (NC,*)	INTEGER		Forecast max wind speeds        *
C*      GUST (NC,*)	INTEGER		Forecast wind gusts             *
C*	WDIR		CHAR*		Direction of present movement   *
C*      ITRACK		INTEGER		Track plot flag			*
C*	SZMRK (2)	REAL		Marker size multiplier		*
C*	LTYPE (2)	INTEGER		Forecast track line type	*
C*	LWIDTH (2)	INTEGER		Forecast track line width	*
C*	LCOLOR (2)	INTEGER		Forecast track line color	*
C*									*
C* Input/Output parameters:						*
C*	RESET		LOGICAL		Color reset flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* m.gamazaychikov/SAIC	06/07	From GH_TCWW				*
C* m.gamazaychikov/SAIC	10/07	Fixed the misspelling of "Storm"	*
C* m.gamazaychikov/SAIC	10/08	Cleaned up, added stsrc strings to 	*
C*				gh_wtct CS, updated cyint and cylong	*
C* A. Krautkramer/NHC	 5/09	Properly set storm type and time zone	*
C* X. Guo/CWS		03/10	Removed argument rmnt and ilegend  	*
C* A. Krautkramer/NHC	 6/09	Add variable definition, replace the	*
C*				first element of the tau array with the	*
C*				passed in tau				*
C* A. Krautkramer	10/10	Corrected initial point inconsistency	*
C* 				with the calculation of dvlbl and tcdvlp*
C* A. Krautkramer/NHC	09/11	Added 'Post-Tropical Cyclone'		*
C* S. Jacobs/NCEP	 4/13	Added smoothing for the track and cone	*
C* S. Jacobs/NCEP	 9/13	Fixed problem with VG output for track	*
C*				less than 3 days			*
C* M. Sardi/NHC		 7/14	Fixed problem with 3 day cone by	*
C*				reverting first GH_KGAT call.		*
C* S. Jacobs/NCEP	 8/14	Removed previous change - did not work	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	PARAMETER	( NUMHR = 8, MXPTS = 500 )
	PARAMETER	( NUMH2 = NUMHR - 2 )
C*
	CHARACTER*(*)	device, wname, timstr, wtype, wsped, tnam, 
     +                  wocen, fdate(nc,*), wwnd, wadnm, wdir, origc,
     +                  wpres, wgst, lext (nc,*)
	CHARACTER	sytim*20, ctau*4
	INTEGER		mxwd (nc,*), ltype (*), lwidth (*), lcolor (*),
     +                  gust (nc,*)
        REAL		rlat(nc,*), rlon(nc,*), szmrk (*)
        REAL		sylat, sylon
	LOGICAL		reset
C*
	CHARACTER	table*128, tbtyp*128,
     +                  ddev*12, 
     +                  ddate(NUMHR)*20, tzone, filnam*40,
     +                  tdev*30, 
     +			adnum*4, cyint (5), bpnam(100)*33, stnum*4,
     +			yyyy*4, stname*32, vtime*10, bpstr*3300,
     +			zone*4, status, cadvnm*5, 
     +			fcstpd*10, wbas*4,
     +			wtyp*4, dummy*30, dummy2*30,
     +			cylong(5)*20, yymm*4, tdate*20, yyy*2, mm*2,
     +			fmonth*9, fmon*9, fday*9, ctime*2,
     +                  zone2*4, ampm*2, bklntp*4
	CHARACTER*30	tau (NUMHR), maxwnd(nc), wgust(nc), mslp(nc), 
     +			tcdvlp(nc), dvlbl(nc), tcdir(nc), tcspd(nc),
     +			dtlbl(nc), advdat(nc), stsrc (nc)
	CHARACTER*240	advstr, taustr, wndstr, gststr, prsstr, 
     +			dvpstr, dvlstr, dirstr, spdstr, dtlstr, 
     +			srcstr 
	CHARACTER*240	advstr6, taustr6, wndstr6, gststr6, prsstr6, 
     +			dvpstr6, dvlstr6, dirstr6, spdstr6, dtlstr6,
     +			srcstr6 
C*
	INTEGER		numbkp(50),
     +			numb(4), ibkpts(4,100), iarea(4,100),
     +                  isev(50), iadvtp(50), igeog(50), icnt (4, 50),
     +			jtarr(5),ibkcur(MAXBK), iarcur(MAXBK),
     +			nppart(20), kpos(MXPTS)
	CHARACTER	cmin*2,cyear*5,cmonth*3,cday*3
        INTEGER		tau0, len, leny, lenm, lend, lenmn
C Start Krautkramer 10/10 modification
	INTEGER         wwnd_int (1), num
C End Krautkramer     	
C*
        REAL            slat(NUMHR), slon(NUMHR), alat(NUMHR), 
     +			alon(NUMHR), slnlat(MXPTS), slnlon(MXPTS), 
     +			slnlt6(MXPTS), slnln6(MXPTS), dist(NUMHR),
     +			bplat(100), bplon(100), 
     +			lbklat(20,MAXBK), lbklon(20,MAXBK),
     +			bkplat(MAXBK), bkplon(MAXBK),
     +			blat(MXPTS), blon(MXPTS), bdst(MXPTS),
     +			bx(MXPTS), by(MXPTS), dx(NUMHR), dy(NUMHR)
C*
	LOGICAL		hvbkpt, finish, plotit, first, extra
C*
	INCLUDE		'ERMISS.FNC'
C*
	DATA		cyint / 'M','H', 'S', 'D', 'L'/
	DATA		cylong / 'Major Hurricane',
     +                           'Hurricane', 
     +				 'Tropical Storm', 
     +				 'Tropical Depression',
     +                           'Remnant Low' /
	DATA		tau / '0','12','24','36','48','72','96','120'/
C-----------------------------------------------------------------------
	iret = 0
C
C*	Query current settings.
C
        CALL GH_SAVE ( ier )
C
	finish = .false.
	extra  = .false.
        ddev = 'XW'
        filnam = ' '
        yyyy = ' '
        stname = ' '
        stnum = ' '
        wbas = ' '
        vtime = ' '
        bpstr = ' '
        srcstr = ' '
        srcstr6= ' '
        dummy = '9999'
        dummy2 = ' '
        iwht = 31
        iblk = 32
        npline = 0
        DO ibpnam = 1, 100
            bpnam(ibpnam) = ' '
        END DO
        DO ii = 1, MAXBK
            bkplat (ii) = RMISSD
            bkplon (ii) = RMISSD
        END DO
C
C*	convert tau0 from int to char
C
	WRITE  ( ctau, '(I2)', IOSTAT = iostat ) tau0	
C
C*	append null character to the string	
C
	CALL ST_NULL(ctau,ctau,len,iret)
C
C* 	replace the first tau in the array with the passed in tau0	
C
	tau(1) = ctau	
C
C*      Set the flag for extratropical line in legend box
C
        ii = 1
        iextra = 0
        DO WHILE ( (.not. extra) .and. (ii .le. nknt) )
           IF ( lext(ii,nstrm) .eq. 'E' ) THEN
              iextra = 1
              extra = .true.
           END IF
           ii = ii + 1
        END DO
        CALL ST_LSTR ( tnam, lenm, ier )
        tdev= ddev // '| TRACK-W/W'
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
                tzone = 'P'
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
C	    knumh2 = nout
	    DO  kk = 1, nout
	        kpos(kk) = 0
		DO  nn = 1, nltln
		    IF  ( ( ABS(blat(kk)-alat(nn)) .lt. 0.0005 ) .and.
     +			  ( ABS(blon(kk)-alon(nn)) .lt. 0.0005 ) )  THEN
			kpos(kk) = nn
			IF ( nn .eq. NUMH2 ) knumh2 = kk
		    END IF
		END DO
	    END DO
C
C*	    Calculate the area under the cone.
C
            IF ( iertb .ge. 0 ) THEN
		plotit = .true.
		inner  = MIN0 ( nltln, NUMH2 )
		minpt  = MIN0 ( nout, knumh2 )
                CALL GH_KGAT ( blat, blon, nout, bdst, MXPTS, slnlt6, 
     +                         slnln6, numln6, ier )
		IF ( nout .gt. knumh2 .and. knumh2 .ne. 0 ) THEN
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
                END IF
                CALL ST_LSTR ( wocen, lenocn, ier )
                CALL ST_UCLC ( wocen (3:4), stnum, ier)
                CALL ST_UCLC ( wocen (:2), wbas, ier)
C
C*	        Plot the average track error line.
C
                IF ( idisp .ge. 0 ) THEN
                  CALL ST_NULL ( tnam, tnam, lena, ier )
                  CALL ST_NULL ( wadnm, wadnm, lena, ier )
                  CALL ST_NULL ( wtype, wtyp, lena, ier )
                  CALL ST_NULL ( timstr,timstr, lena, ier )
                  CALL ST_NULL ( stnum, stnum, lena, ier )
                  CALL ST_NULL ( wname,wname, lena, ier )
                  CALL ST_NULL ( wbas, wbas, lena, ier )
C
C*                3day track error cone.
C
                  fcstpd = '072'
                  iclr = iblk
                  iltyp = 1
                  ifclr = iwht
                  iftyp = 1
                  CALL ST_NULL ( fcstpd, fcstpd, lena, ier )
                  CALL GH_WTCE ( tnam, wtyp, timstr, stnum,
     +                            wname, wbas, wadnm,
     +                            fcstpd,
     +                            ilclr, iltyp, ifclr, iftyp,
     +                            numln6, slnlt6, slnln6,
     +                            iret)
                  IF ( nout .gt. knumh2 .and. knumh2 .ne. 0 ) THEN
C
C*                    5day track error cone.
C
                      fcstpd = '120'
                      iclr = iwht
                      iltyp = 1
                      ifclr = iwht
                      iftyp = 2
                      CALL ST_NULL ( fcstpd, fcstpd, lena, ier )
                      CALL GH_WTCE ( tnam, wtyp, timstr, stnum,
     +                            wname, wbas, wadnm, 
     +                            fcstpd,
     +                            ilclr, iltyp, ifclr, iftyp,
     +                            numln, slnlat, slnlon, 
     +                            iret)
                  END IF
C
C*	    Plot the forecasted track. 
C*	    Set the color and symbol for the current location.
C
                tnam = tnam (:8)//'TCt'//tnam(12:)
                CALL ST_NULL ( tnam, tnam, lena, ier )
                iclr = iblk
                iltyp = 1
C
C*              Lat/Lons and other TC Track parameters

                DO kk = 1, nknt
                    stsrc(kk) = 'Tropical Cyclone'
                    slat(kk) = rlat(kk,im)
                    slon(kk) = rlon(kk,im)
                    IF ( kk .eq. 1 ) THEN
                      advdat(kk) = timstr(5:)
                      maxwnd(kk) = wwnd
                      wgust(kk)  = wgst
                      mslp(kk)   = wpres 
                      tcdir(kk)  = wdir 
                      tcspd(kk)  = wsped 

		      IF (wtype .eq. 'PT') THEN
		           stsrc(kk) = 'Post-Tropical Cyclone'
		      END IF 
		      
C START Krautkramer Sept 2, 2010
C Changed due to inconsitency between the calculation of dvlbl and tcdvlp between
C the initial point (k=1) and k > 1

C	Convert the string stored in maxwnd into a integer for comparison
		      
		      CALL ST_C2I (maxwnd(kk), 1, wwnd_int, num, ier)	      
		      
		      IF (wwnd_int(1) .ge. 96 ) THEN
                         ii = 1
                      ELSE IF ( wwnd_int(1) .ge. 64 ) THEN
                         ii = 2
                      ELSE IF ( wwnd_int(1) .ge. 34 ) THEN
                         ii = 3
                      ELSE
                         ii = 4
                      END IF
		      		      
		      dvlbl(kk) = cyint (ii)
                      tcdvlp(kk) = cylong (ii)
		      
C                      IF ( wtype (1:1) .eq. 'T') THEN
C                         dvlbl(kk)  = wtype(2:2)
C                         IF ( wtype (2:2) .eq. 'D') THEN
C                            tcdvlp(kk) = "Tropical Depression"
C                          ELSE
C                            tcdvlp(kk) = "Tropical Storm"
C                         END IF
C		      ELSE IF ( wtype (1:1) .eq. 'S') THEN
C		      	 dvlbl(kk) = wtype(2:2)
C			 IF  (wtype (2:2) .eq. 'S') THEN
C			    tcdvlp(kk) = "Tropical Storm"
C			 ELSE
C			    tcdvlp(kk) = "Tropical Depression"
C			 END IF
C                      ELSE
C                         dvlbl(kk)  = wtype (1:1)
C                         tcdvlp(kk) = "Hurricane"
C                      END IF
C END Krautkramer Sept 2, 2010		      
		       
                    ELSE

                      mslp(kk)  = dummy
                      tcdir(kk) = dummy 
                      tcspd(kk) = dummy 

                      advdat(kk) = fdate(kk-1, im)
                      CALL ST_INLN (mxwd (kk-1,im),maxwnd(kk),lena,ier)
                      CALL ST_INLN (gust (kk-1,im),wgust(kk), lena,ier)

                      IF ( mxwd ( kk - 1, im ) .ge. 96 ) THEN
                         ii = 1
                      ELSE IF ( mxwd ( kk - 1, im ) .ge. 64 ) THEN
                         ii = 2
                      ELSE IF ( mxwd ( kk - 1, im ) .ge. 34 ) THEN
                         ii = 3
                      ELSE
                         ii = 4
                      END IF

                      dvlbl(kk) = cyint (ii)
                      tcdvlp(kk) = cylong (ii)
		      
                      IF ( lext(kk-1, im) .eq. 'P' ) THEN
                          stsrc(kk) = 'Post-Tropical Cyclone'
                      ELSE
                          stsrc(kk) = 'Tropical Cyclone'
                      ENDIF
                    ENDIF
C
C*                  Create the day/time label string.
C
                    IF ( kk .eq. 1 ) THEN
                        yymm = timstr ( 1:4 )
                        tdate = timstr 
                        CALL ST_INTG ( timstr ( 5:6 ), iday, ier )
                        first = .true.
                      ELSE
                        CALL ST_INTG ( fdate (kk-1,im) (1:2), nday,ier)
                        IF ( ( nday .lt. iday ) .and. first ) THEN
                            first = .false.
                            CALL ST_INTG ( timstr(3:4), month, ier )
                            month = month + 1
                            IF ( month .gt. 12 ) THEN
                                month = 1
                                CALL ST_INTG ( timstr(1:2), iyear, ier)
                                iyear = iyear + 1
                                CALL ST_INCH ( iyear, yyy, ier )
                                IF ( iyear .lt. 10 )
     +                               yyy = '0' // yyy ( 1:1 )
                                yymm ( 1:2 ) = yyy
                            END IF
                            CALL ST_INCH ( month, mm, ier )
                            IF ( month .lt. 10 ) mm = '0' // mm ( 1:1 )
                            yymm ( 3:4 ) = mm
                        END IF
                        tdate = yymm // fdate ( kk-1,im )
                    END IF
                    CALL GH_TIME ( tdate, tzone, jtarr, hours, zone2,
     +                             fmonth, fmon, fday, ampm, ier )
		    CALL ST_NULL ( zone2, zone2, lena, ier )
		    CALL ST_INCH ( jtarr (5), cmin, ier )
		    if ( jtarr (5) .eq. 0) THEN
		    	cmin = '00'
		    ENDIF
		    CALL ST_LSTR ( cmin, lenmn, ier )
  		    CALL ST_INCH ( jtarr (4), ctime, ier )
                    CALL ST_LSTR ( ctime, lent, ier )
                    CALL ST_LSTR ( ampm, lena, ier )
		    CALL ST_INCH ( jtarr (1), cyear, ier )
		    CALL ST_LSTR ( cyear, leny, ier )
		    CALL ST_INCH ( jtarr (2), cmonth, ier )
		    IF ( jtarr(2) .lt. 10 ) THEN
		    	cmonth = '0' // cmonth
		    END IF	
		    CALL ST_LSTR ( cmonth, lenm, ier )
		    CALL ST_INCH ( jtarr (3), cday, ier )
		    IF ( jtarr(3) .lt. 10 ) THEN 
		    	cday = '0' // cday
		    END IF	
		    CALL ST_LSTR ( cday, lend, ier )
		    dtlbl(kk)=cyear(:leny) // cmonth(:lenm) 
     +   	    // cday(:lend) // ' ' // ctime(:lent) 
     +              // cmin(:lenmn) // ' ' // ampm (:lena) // 
     +		    ' ' // fday (1:3)
                END DO
                CALL ST_LSTC  ( advdat, inner, '|', advstr6, ier )
                CALL ST_LSTC  ( tau,    inner, '|', taustr6, ier )
                CALL ST_LSTC  ( maxwnd, inner, '|', wndstr6, ier )
                CALL ST_LSTC  ( wgust,  inner, '|', gststr6, ier )
                CALL ST_LSTC  ( mslp,   inner, '|', prsstr6, ier )
                CALL ST_LSTC  ( tcdvlp, inner, '|', dvpstr6, ier )
                CALL ST_LSTC  ( dvlbl,  inner, '|', dvlstr6, ier )
                CALL ST_LSTC  ( tcdir,  inner, '|', dirstr6, ier )
                CALL ST_LSTC  ( tcspd,  inner, '|', spdstr6, ier )
                CALL ST_LSTC  ( dtlbl,  inner, '|', dtlstr6, ier )
                CALL ST_LSTC  ( stsrc,  inner, '|', srcstr6, ier )
                CALL ST_NULL ( advstr6, advstr6,lena, ier )
                CALL ST_NULL ( taustr6, taustr6,lena, ier )
                CALL ST_NULL ( wndstr6, wndstr6,lena, ier )
                CALL ST_NULL ( gststr6, gststr6,lena, ier )
                CALL ST_NULL ( prsstr6, prsstr6,lena, ier )
                CALL ST_NULL ( dvpstr6, dvpstr6,lena, ier )
                CALL ST_NULL ( dvlstr6, dvlstr6,lena, ier )
                CALL ST_NULL ( dirstr6, dirstr6,lena, ier )
                CALL ST_NULL ( spdstr6, spdstr6,lena, ier )
                CALL ST_NULL ( dtlstr6, dtlstr6,lena, ier )
                CALL ST_NULL ( srcstr6, srcstr6,lena, ier )
C
C*              3 day track.
C
                 fcstpd = '072'
                 iclr = iblk
                 iltyp = 1
                 CALL ST_NULL ( fcstpd, fcstpd, lena, ier )
		 CALL GH_WTCT ( tnam, zone2, wtyp, timstr, stnum,
     +                         wname, wbas, wadnm, 
     +                         fcstpd,
     +                         ilclr, iltyp, inner, 
     +                         slat, slon,
     +                         advstr6, taustr6, wndstr6, 
     +                         gststr6, prsstr6,
     +                         dvpstr6, dvlstr6, dirstr6, 
     +                         spdstr6, dtlstr6, srcstr6,
     +                         iret)	 
     
                 IF ( nout .gt. knumh2 .and. knumh2 .ne. 0 ) THEN
C
C*                  5day track.
C
                    fcstpd = '120'
                    iclr = iblk
                    iltyp = 3
                    CALL ST_LSTC  ( advdat, nknt, '|', advstr, ier )
                    CALL ST_LSTC  ( tau,    nknt, '|', taustr, ier )
                    CALL ST_LSTC  ( maxwnd, nknt, '|', wndstr, ier )
                    CALL ST_LSTC  ( wgust,  nknt, '|', gststr, ier )
                    CALL ST_LSTC  ( mslp,   nknt, '|', prsstr, ier )
                    CALL ST_LSTC  ( tcdvlp, nknt, '|', dvpstr, ier )
                    CALL ST_LSTC  ( dvlbl,  nknt, '|', dvlstr, ier )
                    CALL ST_LSTC  ( tcdir,  nknt, '|', dirstr, ier )
                    CALL ST_LSTC  ( tcspd,  nknt, '|', spdstr, ier )
                    CALL ST_LSTC  ( dtlbl,  nknt, '|', dtlstr, ier )
                    CALL ST_LSTC  ( stsrc,  nknt, '|', srcstr, ier )
                    CALL ST_NULL ( advstr, advstr,lena, ier )
                    CALL ST_NULL ( taustr, taustr,lena, ier )
                    CALL ST_NULL ( wndstr, wndstr,lena, ier )
                    CALL ST_NULL ( gststr, gststr,lena, ier )
                    CALL ST_NULL ( prsstr, prsstr,lena, ier )
                    CALL ST_NULL ( dvpstr, dvpstr,lena, ier )
                    CALL ST_NULL ( dvlstr, dvlstr,lena, ier )
                    CALL ST_NULL ( dirstr, dirstr,lena, ier )
                    CALL ST_NULL ( spdstr, spdstr,lena, ier )
                    CALL ST_NULL ( dtlstr, dtlstr,lena, ier )
                    CALL ST_NULL ( fcstpd, fcstpd, lena, ier )
                    CALL ST_NULL ( srcstr, srcstr, lena, ier )
                    CALL GH_WTCT ( tnam, zone2, wtyp, timstr, stnum,
     +                           wname, wbas, wadnm, 
     +                           fcstpd,
     +                           ilclr, iltyp, nknt, slat, slon,
     +                           advstr, taustr, wndstr, gststr, prsstr,
     +                           dvpstr, dvlstr, dirstr, spdstr, dtlstr,
     +                           srcstr, iret)	    
                END IF
C
           IF ( hvbkpt ) THEN
C
C*         Breakpoint output
C
                tnam = tnam (:8)//'TCb'//tnam(12:)
                CALL ST_NULL ( tnam, tnam, lena, ier )
                    DO ii = 1, 4
                      IF ( numb (ii) .gt. 0 ) THEN
                        IF ( ii .eq. 1 ) THEN
                           itcww = 0 
                           ilclr = 2
                           iltyp = 12 
                        END IF
                        IF ( ii .eq. 2 ) THEN 
                           itcww = 1 
                           ilclr = 11
                           iltyp = 5 
                        END IF
                        IF ( ii .eq. 3 ) THEN 
                           itcww = 2 
                           ilclr = 4
                           iltyp = 8 
                        END IF
                        IF ( ii .eq. 4 ) THEN 
                           itcww = 3 
                           ilclr = 5
                           iltyp = 2 
                        END IF
                         indbkc = 1
                         indbkp = 1
C
C*                       Start loop over the number of segments
C
                         DO ilines = 1, numb(ii)
                          inpseg = icnt (ii,indbkc)
C
C*                        Zero out the brakpoint and area arrays 
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
                          DO ipts = 1, icnt (ii,indbkc)
                             ibkcur(indb2) = ibkpts(ii, indbkp)
                             iarcur(indb2) = iarea(ii, indbkp)
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
     +				                nparts, nppart, 
     +				                lbklat, lbklon, 
     +					        iret)
                                 fcstpd = '120'
                                 CALL ST_NULL ( tnam, tnam, lena, ier )
                                 CALL ST_NULL ( wadnm, wadnm, lena,ier)
                                 CALL ST_NULL ( wtype, wtyp, lena, ier)
                                 CALL ST_NULL ( timstr,timstr,lena,ier)
                                 CALL ST_NULL ( stnum, stnum, lena,ier)
                                 CALL ST_NULL ( wname,wname, lena, ier)
                                 CALL ST_NULL ( wbas, wbas, lena, ier )
                                 CALL ST_NULL ( fcstpd,fcstpd,lena,ier)
                                 DO inparts = 1, nparts
                                   npline = nppart(inparts)
                                   DO ibk= 1, npline
                                     bkplat(ibk) = lbklat(inparts,ibk)
                                     bkplon(ibk) = lbklon(inparts,ibk)
                                   END DO
                                   CALL GH_WTCB ( tnam, wtyp, timstr, 
     +                                         stnum, wname,wbas,wadnm,
     +                                         fcstpd, ilclr, iltyp, 
     +                                         itcww,
     +                                         npline, 
     +                                         bkplat, bkplon,
     +                                         iret)
                                END DO
                             END IF
                          indb2 = indb2+1
                          indbkp = indbkp + 1
C
C*                        End of POINTS loop
C
                          END DO

                            IF  ( bklntp .eq. "ordr" .or. 
     +                            bklntp .eq. "pair") THEN
                                  CALL GH_GBKL ( ibkcur, iarcur, 
     +				                 inpseg, bklntp,
     +				                 nparts, nppart, 
     +				  		 lbklat, lbklon,
     +					         iret)
                              fcstpd = '120'
                              CALL ST_NULL ( tnam, tnam, lena, ier )
                              CALL ST_NULL ( wadnm, wadnm, lena, ier )
                              CALL ST_NULL ( wtype, wtyp, lena, ier )
                              CALL ST_NULL ( timstr,timstr, lena, ier )
                              CALL ST_NULL ( stnum, stnum, lena, ier )
                              CALL ST_NULL ( wname,wname, lena, ier )
                              CALL ST_NULL ( wbas, wbas, lena, ier )
                              CALL ST_NULL ( fcstpd, fcstpd, lena,ier)
                              npline = nppart(1)
                              DO ibk= 1, npline
                                     bkplat(ibk) = lbklat(1,ibk)
                                     bkplon(ibk) = lbklon(1,ibk)
                              END DO		      
                              CALL GH_WTCB ( tnam, wtyp, timstr,stnum,
     +                                     wname, wbas, wadnm,
     +                                     fcstpd, ilclr, iltyp,
     +                                     itcww,
     +		                           npline, bkplat, bkplon,
     +                                     iret)
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
                END IF
		plotit = .false.
		finish = .true.
                END IF
	      ELSE
		plotit = .false.
		hvbkpt = .false.
		finish = .true.
            END IF
C
C*	    Flush the graphics buffer.
C
	    CALL GEPLOT ( ier )
            CALL GENANM ( ier )
C
C*          Check for an acceptable display.
C
            IF ( .not. finish ) THEN
                IF ( device(:2) .eq. 'XW' ) THEN
                    finish = .true.
                ELSE
                    ddev = device
                    CALL ST_LSTR(tnam, lenm, ier )
                    CALL ST_LSTR(ddev, lend, ier )
                    device = ddev(:lend) // '|'
     +                    // tnam(:lenm)
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


