        SUBROUTINE GG_ENCY ( trfil, dattm2, icolor, mrktyp, ssize,
     +                       iwidth, lwidth, iflags, models, wlevs, 
     +                       ifcsth, iret )
C************************************************************************
C* GG_ENCY                                                              *
C*                                                                      *
C* This subroutine reads and plots ensemble cyclone data from a single	*
C* storm file for specified models and time(s).				*
C*                                                                      *
C* GG_ENCY ( TRFIL, DATTM2, ICOLOR, MRKTYP, SSIZE, IWIDTH, LWIDTH,     	*
C*           IFLAGS, MODELS, WLEVS, IFCSTH, IRET )                      *
C*                                                                      *
C* Input parameters:                                                    *
C*      TRFIL           CHAR*           Ensemble track file name	*
C*      DATTM2          CHAR*           Initial time for ensemble track	*
C*      ICOLOR (20)     INTEGER         Line and marker colors          *
C*      MRKTYP (20)     INTEGER         Marker symbol numbers           *
C*      SSIZE  (20)     REAL            Marker sizes                    *
C*      IWIDTH (20)     INTEGER         Marker line widths              *
C*      LWIDTH (20)     INTEGER         Line widths                     *
C*      IFLAGS (4)      INTEGER         Flags for labels                *
C*                                        0 = false                     *
C*                                        1 = true                      *
C*      MODELS (20)     CHAR*           Array of all valid model names  *
C*      WLEVS  (3)      REAL            Wind levels to color-code tracks*
C*      IFCSTH          INTEGER         Optional single fcst hour       *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/SAIC	04/05                                           *
C* D. Kidwell/NCEP	 5/05	Checked for no file, removed rounding   *
C* m.gamazaychikov/SAIC	06/05	Changed read/decode section		*
C* m.gamazaychikov/SAIC	02/06	Add code to color tracks based on wlevs *
C* m.gamazaychikov/SAIC	02/06	Fixed a bug to plot single point tracks *
C* m.gamazaychikov/SAIC	07/06	Increased NA to 25			*
C* H. Zeng/SAIC         07/06   changed wind speed cate. from 3 to 4    *
C* S. Jacobs/NCEP	 7/10	Changed to use comma delimiter instead	*
C*				of specific columns in the data record	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	PARAMETER       ( NA = 25, NPTS = 75, NS = 500 )
        PARAMETER       ( JTIM = 1, JPRS = 2, JMRK = 3, JCLR = 4 )
C*
        CHARACTER*(*)   trfil, dattm2, models (*)
        INTEGER         icolor (*), mrktyp (*), iwidth (*), lwidth (*),
     +                  iflags (*), itype, ifcsth
        REAL            ssize (*), wlevs (*)
C*
        CHARACTER       strmid (NS, NA)*8, presr (NPTS, NS, NA)*4,
     +                  windc(NPTS, NS, NA)*3,
     +                  modl*6, tau*3, timstr*8, cdttm*20,
     +                  lastim*20, tmpll*4, timest*20, tstamp*6,
     +                  buffer*195, strid*8,  wlbls(4)*27, 
     +                  leglbl(2)*15, carr(25)*12
        INTEGER         idtarr (5), numpts (NS, NA), iorder (NPTS),
     +                  ifhour (NPTS, NS, NA), idum (NPTS), jdtarr(5)
        REAL            alat (NPTS, NS, NA), alon (NPTS, NS, NA),
     +                  blat (NPTS), blon (NPTS), wind (NPTS, NS, NA),
     +                  lad(2), lod(2)
        LOGICAL         legnd (NA), strmch, modlch
        DATA            leglbl  / 'Model: ', 'Wind:  ' /
C-----------------------------------------------------------------------
        iret  = 0
C
C*	Set the wind levels, colors and labels for legend
C
	ictd = icolor (1)
        icgl = icolor (2)
	icts = icolor (3)
	ichr = icolor (4)
        wdtd = wlevs (1)
        wdgl = wlevs (2)
        wdts = wlevs (3)
        wdhr = wlevs (4)
        CALL ST_INLN ( int(wdtd), tmpll, ilen, ier)
        wlbls(1) = ' < '//tmpll(:ilen)//' knt '
        wlbls(2) = tmpll(:ilen)
        CALL ST_INLN ( int(wdgl), tmpll, ilen, ier)
        wlbls(2) = wlbls(2)(:ilen)//'-'//tmpll(:ilen)//' knt '
        wlbls(3) = tmpll(:ilen)
        CALL ST_INLN ( int(wdts), tmpll, ilen, ier)
        wlbls(3) = wlbls(3)(:ilen)//'-'//tmpll(:ilen)//' knt '
        CALL ST_INLN ( int(wdts), tmpll, ilen, ier)
        wlbls(4) = ' > '//tmpll(:ilen)//' knt '
        mstr = 5
C
C*      Open the specified data file.
C
        CALL FL_SOPN ( trfil, lund, ier )
	IF ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
C*      Check for special case of LAST, and get the time from
C*      the file.  Ignore the first two year digits in the ENCY file
C*      to be compatible with the gempak date string and subroutines.
C*
C
        IF ( dattm2 ( :4 ) .eq. 'LAST' ) THEN
            timstr = '00000000'
            iostat = 0
            READ ( lund, 3, IOSTAT = iostat ) buffer
            IF ( iostat .eq. 0 ) THEN
		CALL ST_CLST ( buffer, ',', ' ', 23, carr, num, ier )
		IF  ( carr(3) .gt. timstr ) timstr = carr(3)
            END IF
C
C*          Compare latest time to current time to see if this is an
C*          active storm.
C
            itype = 1
            CALL CSS_GTIM ( itype, cdttm, ier )
            lastim = timstr ( :6 ) // '/' // timstr ( 7:8 ) // '00'
            CALL TI_DIFF ( cdttm, lastim, nmin, ier )
C
C*          If the current time is more than 9 hours after the latest
C*          storm time, assume this is NOT a current storm.  This number
C*          may need to be adusted up or down, depending on timeliness
C*          of data receipt.
C
            IF ( nmin .gt. 540 ) THEN
                CALL FL_CLOS ( lund, ier )
                RETURN
              ELSE
                CALL FL_REWD ( lund, ier )
            END IF
          ELSE
C
C*          The input time is the time we are looking for.
C
            timstr = dattm2 ( :6 ) // dattm2 ( 8:9 )
        END IF
C
        timest = timstr (1:6) // '/' // timstr (7:8) // '00'
        CALL TI_CTOI ( timest(:11), idtarr, ier )
C
        iostat = 0
C
        DO ii  = 1, NA
           legnd ( ii )  = .false.
           DO nn  = 1, NS
              strmid ( nn, ii ) = ' '
              numpts ( nn, ii ) = 0
              DO jj = 1, NPTS
                 alat   ( jj, nn, ii ) = RMISSD
                 alon   ( jj, nn, ii ) = RMISSD
                 wind   ( jj, nn, ii ) = RMISSD
                 ifhour ( jj, nn, ii ) = IMISSD
                 presr  ( jj, nn, ii ) = ' '
                 windc  ( jj, nn, ii ) = ' '
              END DO
           END DO
        END DO
C
C*      Loop on all records in the file.
C
        nn = 0
        DO WHILE ( iostat .eq. 0 )
            READ ( lund, 3, IOSTAT = iostat ) buffer
3           FORMAT ( A )
            IF ( iostat .eq. 0 )  THEN
	       CALL ST_CLST ( buffer, ',', ' ', 23, carr, num, ier )
               IF ( num .eq. 23 .and. ier .eq. 0 )  THEN
		IF ( carr(3)(3:) .eq. timstr ) THEN
C
C*                A match was found on the time. Get model.
C
                  im = mstr
                  modlch = .false.
                  DO WHILE ( .not. modlch )
                     CALL ST_LSTR ( models ( im ), lmod, ier )
		     modl = carr(5)
                     IF ( modl (:lmod) .eq. models (im) (:lmod) .and. 
     +                    icolor ( im ) .gt. 0 )  THEN
C
C*                      A match was found on the model name. Get strm id
C
                        imodel = im
                        modlch = .true.
			CALL ST_LSTR ( carr(7), len7, ier )
                        CALL ST_CRND ( carr(7)(1:len7-1),
     +					chlat, nd, ier1 )
			CALL ST_LSTR ( carr(8), len8, ier )
                        CALL ST_CRND ( carr(8)(1:len8-1),
     +					chlon, nd, ier2 )
                        IF ( ( chlat .ne. 0.0 .or. chlon .ne. 0.0 )
     +				.and. ( ier1 .eq. 0 .and. ier2 .eq. 0 )
     +				 ) THEN
C
C*                          Construct the storm id name.
C
			    CALL ST_LSTR ( carr(1), len1, ier )
			    CALL ST_LSTR ( carr(2), len2, ier )
                            strid  = carr(1)(:len1) // carr(2)(:len2)
                            strmch = .false.
                            is     = 1
			    istorm = 0
                            DO WHILE ( .not. strmch )
                              IF ( strid .eq. strmid (is, imodel) )
     +				 THEN
                                 strmch = .true.
                                 IF ( numpts ( is, imodel) .lt. NPTS )
     +				      THEN
C
C*                                  This is an existing storm.
C
                                    numpts (is,imodel) = 
     +					       numpts (is,imodel) + 1
                                    istorm = is
				 END IF
                               ELSE IF ( numpts ( is, imodel ) .eq. 0 )
     +				      THEN
C
C*                               This is a new storm.
C
                                 strmid (is, imodel) = strid
                                 numpts (is, imodel) = 1
                                 istorm = is
                                 strmch = .true.
                              END IF
                              is = is + 1
                              IF ( is .gt. NS ) strmch = .true.
                            END DO
C
C*                          Decode the records into the arrays.
C
			    IF ( istorm .gt. 0 ) THEN
                               ip = numpts (istorm, imodel)
                               tau  = carr(6)
                               CALL ST_RMBL ( tau, tau, ln, ier )
                               CALL ST_INTG ( tau (1:ln), itau, ier)
                               ifhour ( ip, istorm, imodel ) = itau
                               presr  ( ip, istorm, imodel ) = 
     +					carr(10)
			       tmpll = carr(9)
                               windc(ip,istorm,imodel) = carr(9)
                               CALL ST_RMBL ( tmpll, tmpll, ln, ier )
                               CALL ST_CRNM ( tmpll (1:ln), wnd, ier )
                               wind ( ip, istorm, imodel ) = wnd
C
			       CALL ST_LSTR ( carr(7), len7, ier )
			       CALL ST_LSTR ( carr(8), len8, ier )
                               IF ( carr(7)(len7:len7) .eq. 'N' ) THEN
                                  dirns = 1.
                                ELSE
                                  dirns = -1.
                               END IF
                               IF ( carr(8)(len8:len8) .eq. 'E' ) THEN
                                  direw = 1.
                                ELSE
                                  direw = -1.
                               END IF
                               tmpll = carr(7)(1:len7-1)
                               CALL ST_RMBL ( tmpll, tmpll, ln, ier )
                               CALL ST_INTG ( tmpll (1:ln), ilat, ier )
                               tmpll = carr(8)(1:len8-1)
                               CALL ST_RMBL ( tmpll, tmpll, ln, ier )
                               CALL ST_INTG ( tmpll (1:ln), ilon, ier )
                               alat (ip, istorm, imodel) = ilat / 10.0 *
     +							   dirns
                               alon (ip, istorm, imodel) = ilon / 10.0 *
     +							   direw
			      ELSE
			       modlch = .true.
			    END IF
                        END IF
                     END IF
                     im = im + 1
                     IF ( im .gt. NA ) modlch = .true.
                  END DO
                 ELSE
C
C*                A match was NOT found on the time.
C
C		     print*,'match not found ',carr(3)(3:),' ? ',timstr
C		     print*,'num = ', num, '   ier = ', ier
		     print*,buffer
                  RETURN
                END IF
	       ELSE
C		   print*,'num = ', num, '   ier = ', ier
		   print*,buffer
               END IF
            END IF
        END DO
C
C*      Sort and plot the tracks.
C     
        CALL GSTEXT ( 22, 2, .75, 1, 111, 1, 1, ier )
        DO im = mstr, NA
           IF ( icolor (im) .ne. 0 ) THEN
              strmch = .false.
              is = 1
              CALL GSLINE ( 1, 0, lwidth ( im ), 0, ier )
              CALL GSMRKR ( mrktyp ( im ), 0, ssize ( im ),
     +                     iwidth ( im ), ier )
              DO WHILE ( .not. strmch  .and. strmid (is, im) .ne. ' ' )
                 npoints = numpts (is, im) 
C
C*               Perform sort of forecast hours array and store
C*               the number and order of unique entries.
C
                 DO ip = 1, npoints
                    idum ( ip ) = ifhour (ip, is, im)
                    iorder ( ip ) = ip
                 END DO
                 iswflg = 1
                 istop  = npoints - 1
                 DO WHILE  (( iswflg .ne. 0 ) .and. ( istop .ge. 1 ) )
                    iswflg = 0
                    DO ii = 1, istop
                       IF  ( idum (ii) .gt. idum (ii+1) )  THEN
                          iswflg        = 1
                          iswpbf        = idum (ii)
                          idum (ii)     = idum (ii+1)
                          idum (ii+1)   = iswpbf
                          isp           = iorder (ii)
                          iorder (ii)   = iorder (ii+1) 
                          iorder (ii+1) = isp
                       END IF
                    END DO
                    istop = istop - 1
                 END DO
                 jj = 1
                 DO  ii = 2, npoints
                    IF  ( idum (ii) .ne. idum (jj) )  THEN
                       jj = jj + 1
                       idum (jj) = idum (ii)
                       iorder (jj) = iorder (ii)
                    END IF
                 END DO
                 knt = jj
                 DO ip = 1, knt
                     ipp = iorder (ip)
                     blat (ip) = alat (ipp, is, im)
                     blon (ip) = alon (ipp, is, im)
                 END DO
C
C*               Plot single track and plot markers and pressures
C*               if requested.
C
		 IF ( knt .gt. 0 ) THEN
                     legnd ( im ) = .true.
C
C*                   Plot track. but skip this part if single
C*                   fcst hour display is required.
C
                     IF ( ifcsth .eq. -1 ) THEN
C
                     IF ( iflags ( JCLR ) .eq. 0 ) THEN
                        CALL GSCOLR ( icolor ( im ), ier )
                        CALL GLINE  ( 'M', knt, blat, blon, ier )
                      ELSE IF ( iflags ( JCLR ) .eq. 1 .and.
     +                          knt .gt. 1 ) THEN
                        DO ip = 2, knt
                           ipp = iorder (ip)
                           IF ( wind (ipp,is,im) .lt. wdtd ) icol = ictd
                           IF ( wind (ipp,is,im) .ge. wdtd .and.
     +                          wind (ipp,is,im) .lt. wdgl ) icol = icgl
                           IF ( wind (ipp,is,im) .ge. wdgl .and.
     +                          wind (ipp,is,im) .lt. wdts ) icol = icts
                           IF ( wind (ipp,is,im) .ge. wdts ) icol = ichr
                           CALL GSCOLR ( icol, ier )
                           lad (1) = blat (ip-1)
                           lad (2) = blat (ip)
                           lod (1) = blon (ip-1)
                           lod (2) = blon (ip)
                           CALL GLINE  ( 'M', 2, lad, lod, ier)
                        END DO
                     END IF
C
                     END IF
C
C*                   Plot markers
C
                     IF ( iflags ( JMRK ) .ne. 0 ) THEN
                        DO ip = 1, knt
                           ipp = iorder (ip)
                           IF ( iflags (JCLR) .eq. 0 ) icol=icolor(im)
                           IF ( iflags (JCLR) .eq. 1 ) THEN
                              IF ( wind (ipp,is,im) .lt. wdtd )
     +                           icol = ictd
                              IF ( wind (ipp,is,im) .ge. wdtd .and.
     +                             wind (ipp,is,im) .lt. wdgl ) 
     +                           icol = icgl
                              IF ( wind (ipp,is,im) .ge. wdgl .and.
     +                             wind (ipp,is,im) .lt. wdts ) 
     +                           icol = icts
                              IF ( wind (ipp,is,im) .ge. wdts ) 
     +                           icol = ichr
                           END IF
C
                           IF ( ifcsth .eq. -1 .or. 
     +                          ifcsth .eq. ifhour(ipp,is,im) ) THEN
                              CALL GSCOLR ( icol , ier )
                              CALL GMARK  ( 'M', 1, blat(ip), blon(ip), 
     +                                    ier)
                           END IF
                        END DO
                     END IF
C
C*                   Plot time stamp
C
                     IF ( iflags ( JTIM ) .ne. 0 ) THEN
                         IF ( iflags ( JCLR ) .eq. 0 ) icol=icolor(im)
                         IF ( iflags ( JCLR ) .eq. 1 ) icol=31
                         CALL GSCOLR ( icol , ier )
C
                         DO ip = 1, knt
                           ipp = iorder (ip)
                           imins = ifhour (ipp, is, im) * 60
                           CALL TI_ADDM ( idtarr, imins, jdtarr, ier )
                           CALL TI_ITOC ( jdtarr, timest, ier )
                           tstamp = timest (5:9)
C
                           IF ( (ifcsth .eq. -1 .and. ip .eq. 1) .or. 
     +                           ifcsth .eq. ifhour(ipp,is,im) ) THEN
                              CALL GTEXT ( 'M', blat (ip), blon (ip), 
     +                                     tstamp, 0.0, -6, 2, ier)
                           END IF
                         END DO
                     END IF
C
C*                   Plot pressure labels
C
                     IF ( iflags ( JPRS ) .ne. 0 ) THEN
                         DO ip = 1, knt
                            IF ( iflags (JCLR) .eq. 0 ) icol=icolor(im)
                            IF ( iflags (JCLR) .eq. 1 ) icol=31
                            CALL GSCOLR ( icol , ier )
                            ipp = iorder (ip)
                            IF ( ifcsth .eq. -1 .or. 
     +                           ifcsth .eq. ifhour(ipp,is,im) ) THEN
                              CALL GTEXT ( 'M', blat (ip), blon (ip),
     +                                   presr (ipp, is, im), 0.0, 0,
     +				         -3, ier )
                            END IF
                         END DO
                     END IF
		 END IF
                 is = is + 1
                 IF ( is .gt. NS ) strmch = .true.
              END DO
           END IF
        END DO
C
C*      Plot the legends.
C
        CALL GQBND ('P', xeleg, ysleg, xwleg, ynleg, ier)
        ylegoff = 0.04
        xlegoff = 0.05
        ysleg   = ysleg + ylegoff
        xeleg   = xeleg + xlegoff
        CALL GSTEXT ( 22, 0, .8, 0, 121, 0, 3, ier )
C
C*      First legend - model names.
C
        iys = 1
        DO  ii = NA, mstr, -1
            IF ( legnd ( ii ) ) THEN
                IF ( iflags (JCLR) .eq. 0 ) icol=icolor(ii)
                IF ( iflags (JCLR) .eq. 1 ) icol=31
                CALL GSCOLR ( icol, ier )
                CALL GTEXT  ( 'P', xeleg, ysleg, models ( ii ) (:4),
     +                        0.0, 0, 0, ier )
                ysleg = ysleg + .02
                iys = iys + 1
            END IF
        END DO
        CALL GSCOLR ( 31, ier )
        CALL GTEXT  ( 'P', xeleg, ysleg, leglbl (1) (:7),
     +                        0.0, 0, 0, ier )
C
C*      Next legend - wind criteria.
C
        IF ( iflags (JCLR) .eq. 1 ) THEN
           ysleg = ysleg - 0.02*iys
           CALL GSCOLR ( 31, ier )
           CALL GTEXT  ( 'P', xeleg, ysleg, leglbl (2) (:7),
     +                    0.0, 0, 0, ier )
           DO  jjjj = 1, 4
               CALL GSCOLR ( icolor(jjjj), ier )
               xeleg   = xeleg + 0.08
               CALL GTEXT  ( 'P', xeleg, ysleg, wlbls ( jjjj ),
     +                        0.0, 0, 0, ier )
           END DO
        END IF
C
        CALL FL_CLOS ( lund, ier )
C*
        RETURN
        END
