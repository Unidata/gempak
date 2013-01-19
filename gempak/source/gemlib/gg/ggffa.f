	SUBROUTINE GG_FFA ( dattim, icolor, mrktyp, ssize, iwidth,
     +			    lwidth, iflags, iret )
C************************************************************************
C* GG_FFA								*
C*									*
C* The subroutine plots the current flash flood watches			*
C*									*
C* GG_FFA ( DATTIM, ICOLOR, MRKTYP, SSIZE, IWIDTH, LWIDTH, IFLAGS, 	*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Ending time for flash flood	*
C*	ICOLOR (2)	INTEGER		Marker symbol colors		*
C*	MRKTYP (2)	INTEGER		Marker symbol types		*
C*	SSIZE  (2)	REAL		Marker symbol sizes		*
C*	IWIDTH (2)	INTEGER		Marker symbol widths		*
C*	LWIDTH (2)	INTEGER		Outline widths			*
C*	IFLAGS (4)	INTEGER		Flags for labels, outline	*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Logs:								*
C* C. Bailey/HPC	 4/06	Created					*
C* m.gamazaychikov/SAIC 05/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 1000, NC = 100 )
C*
	PARAMETER	( JFF = 1, JFA = 2 )
	PARAMETER	( JTIM = 1, JLAB = 2, JIC = 3, JFIL = 4 )
C*
	CHARACTER*(*)	dattim
	INTEGER		icolor(*), mrktyp(*), iwidth(*), iflags(*)
	INTEGER		lwidth(*)
	REAL		ssize(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20,
     +			buffer*128, tfile*128, wlabel*128, dattm4*20,
     +			tmstp4*20, tmstr4*20, carr(9)*20, ttype*3,
     +			tid*4, timlst*20, timtmp*20, carr1(3)*3,
     +			ttnum*4
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
	CHARACTER	timstr(NW)*20, timstp(NC,NW)*20, type(NW)*3,
     +			waid(NW)*4, tmcnl(NC)*20, tnum(NW)*4
	INTEGER		itest(NW), npt(NW), mtype(NW), itype, jtest
	REAL		rlat(NC,NW), rlon(NC,NW)
	CHARACTER	cnnam(NC,NW)*32, fipnam(NC,NW)*8, 
     +			st*2, cn*2, wfo*20, isstr(NW), tic(NW)*35,
     +			icauses(15)*35
C*
	CHARACTER	stime*20, flstrt*160, cfips*12, bndtyp*24
	INTEGER		itarr(5), jtarr(5), mrktst(17:21), nfips(NC,NW)
	LOGICAL		done, cancel(NW), ckcncl, cncld(NW)
	LOGICAL		match
C*
	DATA		mrktst / 2, 3, 4, 6, 14 /
	DATA		icauses / 'Excessive Rainfall',
     +				  'Snowmelt',
     +				  'Rain & Snowmelt',
     +				  'Dam & Levee Failure',
     +				  'Ice Jam',
     +				  'Glacier-Dammed Lake Outburst',
     +				  'Rain and/or Snow and/or Ice Jam',
     +				  'Upstream Flooding + Storm Surge',
     +				  'Upstream Flooding + Tidal Effects',
     +				  'Elev. Upstream Flow + Tidal Effects',
     +				  'Wind and/or Tidal Effects',
     +				  'Upstream Dam or Reservoir Release',
     +				  'Other Multiple Causes',
     +				  'Other Effects',
     +				  'Unknown' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Scan the directory for all of the flash flood data files.
C
	filnam = 'FFA'
	path   = ' '
	templ  = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +		ihb, mnb, iha, mna, mstrct, idtmch, ier)
	CALL ST_RNUL (path, path, lens, ier )
	CALL ST_RNUL (templ, templ, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )
	nexp   = MXNMFL
	iorder = 1
	CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*	Check for the last file requested by the user.
C
	CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1
	IF  ( ( dattim .eq. 'LAST' ) .or.
     +	      ( dattim .eq. 'ALL' ) )  THEN
	    CALL CSS_GTIM ( itype, dattm2, ier )
	    CALL FL_MNAM ( dattm2, templ, filnam, ier )
	ELSE
	    CALL CSS_GTIM ( itype, cdttm, ier )
	    CALL TI_STAN ( dattim, cdttm, dattm2, ier )
	    IF ( ier .ne. 0 ) THEN
		CALL ER_WMSG ( 'TI', ier, dattim, ierr )
		iret = ier
		RETURN
	    END IF
	    CALL FL_MNAM ( dattm2, templ, filnam, ier )
	END IF
C
C*	Find the earliest file to start searching. For ALL times
C*	go back 10 days, for any other entry for dattim subtract
C*	24 hours from the time given.
C
	IF  ( dattim .eq. 'ALL' )  THEN
	    minuts = 14400
	ELSE
	    minuts = 1440
	END IF
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
	CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*	Get 4-digit year to compare dates.
C
	CALL TI_DTM4 ( dattm2, dattm4, ier )
C
C*	Decode each file until the end time is reached.
C
	nwstm = 0
	done  = .false.
	ifl   = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	    IF  ( files(ifl) .gt. filnam )  THEN
		done = .true.
	    ELSE IF  ( nwstm .ge. NW )  THEN
		done = .true.
	    ELSE
		IF  ( files (ifl) .ge. flstrt )  THEN
		  tfile = path ( :lenp ) // '/' // files ( ifl )
		  CALL FL_SOPN ( tfile, lunf, ier )
C
		  iostat = 0
C
C*		  Loop on the records in the file.
C
		  DO WHILE  ( iostat .eq. 0 )
		    READ ( lunf, 2, IOSTAT = iostat ) buffer
 2		    FORMAT ( A )
		    IF  ( iostat .eq. 0 )  THEN
		      IF  ( buffer ( 1:1 ) .eq. '|' )  THEN
			  CALL ST_CLST ( buffer, '|', ' ', 9, carr,
     +			                 inum, ier )
			  ttype = carr ( 2 )
			  tid   = carr ( 7 )
			  ttnum = carr ( 8 )

			  CALL ST_CLST ( carr(9), ';', ' ', 3, carr1,
     +                                   inum1, ier ) 
C			  Check for cancelled action code
			  IF ( carr1(1)(1:3) .eq. 'CAN' ) THEN
			      jcancl = 1
			  ELSE
			      jcancl = 0
			  END IF
C			  Check if message is a test
			  IF (carr1(2)(1:1) .eq. 'T') THEN
			      jtest = 1
			  ELSE
			      jtest = 0
			  ENDIF
C			  Check of correction action code
			  IF ( carr1(1)(1:3) .eq. 'COR' ) THEN
                              jcorr = 1
                          ELSE
                              jcorr = 0
                          END IF
C
			  DO ii = 1, nwstm
			      cancel ( ii ) =.false.
			  END DO
			  ckcncl = .false.
C
                          IF ( jcancl .eq. 1 ) THEN
C
C*			      This is a cancellation.  Look for the
C*			      message being cancelled.  Match the
C*			      bulletin originator, message type, and
C*			      event code.  Look for a start time
C*			      before the cancel time, and an end time no
C*			      more than 4 hours before the cancel time
C*			      (a message can be cancelled after its
C*			      nominal end time), or an end time after
C*			      the cancel time.
C
			      DO ii = 1, nwstm
				IF ( ( tid .eq. waid (ii) ) .and.
     +				     ( ttype .eq. type (ii) ) .and.
     +				     ( ttnum .eq. tnum (ii) ) .and. 
     +				     ( carr (4) .gt. timstr(ii) ) ) THEN
C
C*				    Find the most recent time to compare
C*				    with the cancel time.
C
				    CALL TI_DTM4 ( timstp ( 1, ii ),
     +						   timlst, ier )
				    DO jj = 2, npt ( ii )
					CALL TI_DTM4 ( timstp ( jj,ii ),
     +						       timtmp, ier )
					IF ( timtmp .gt. timlst )
     +					          timlst = timtmp
				    END DO
				    timlst = timlst ( 3: )
				    CALL TI_DIFF ( carr ( 4 ), timlst,
     +					           nmin, ier )
				    IF ( nmin .le. 240 ) THEN
C
C*					A previous message matching the
C*					cancellation was found.
C
					cancel ( ii ) = .true.
					ckcncl = .true.
				    END IF
				END IF
                              END DO
                          END IF
C
C*                        Read the lat/lon coordinates from the file.
C
                          knt = 0
                          jostat = 0
			  jw = nwstm + 1
                          DO WHILE  ( jostat .eq. 0 )
                              READ ( lunf, 2, IOSTAT = jostat ) buffer
                              IF  ( jostat .eq. 0 )  THEN
                                IF  ( buffer(1:1) .eq. '|' )  THEN
                                    CALL FL_BKSP ( lunf, ier )
                                    jostat = -1
                                  ELSE
                                    knt = knt + 1
                                    IF ( knt .le. NC ) THEN
                                        READ (buffer, 1000)
     +					      fipnam(knt,jw),
     +                                        nfips(knt,jw),
     +					      cnnam(knt,jw),
     +                                        st, cn, rlat(knt,jw),
     +                                        rlon(knt,jw), elv,
     +                                        ipr, wfo
1000                                    FORMAT ( A, 1X, I6, 1X, A, 1X,
     +                                        A, 1X, A, 1X, F9.2, 1X,
     +                                        F9.2, 1X, F9.2, 1X, I2,
     +                                        1X, A )
                                    END IF
                                END IF
                              END IF
                          END DO
C
C*                        Check for correction to endtime or to test
C*                        status, if not a cancellation.
C
			  match = .false.
			  IF ( jcorr .eq. 1 ) THEN
			      DO ii = 1, nwstm
				IF ( ( ttype .eq. type(ii) )     .and.
     +				     ( carr(4) .ge. timstr(ii) ) .and.
     +				     ( tid .eq. waid (ii) )      .and.
     +				     ( ttnum .eq. tnum (ii) )    .and.
     +				     ( knt .eq. npt(ii) )  ) THEN
C
				     nn = 0     
				     DO jj = 1, knt
					IF ( fipnam (jj,nwstm+1) .ne.
     +                                       fipnam (jj,ii) )
     +					   nn = nn + 1
				     END DO
C
				     IF ( nn .eq. 0 ) THEN
					DO jj = 1, knt
					  IF ( carr( 6 ) .ne.
     +                                           timstp(jj,ii) ) THEN
					      timstp(jj,ii) = carr( 6 )
					      match = .true.
					  END IF
					END DO
C
					IF(jtest .ne. itest(ii) ) THEN
					    itest(ii) = 1
					    match = .true.
					ENDIF    
C
				     END IF
				END IF
			     END DO
			  END IF
C
C*                        Add this report if it is not a cancellation.
C
			  IF ( .not. match ) THEN
                            IF ( jcancl .eq. 0 ) THEN
                              nwstm            = nwstm + 1
                              npt ( nwstm )    = knt
                              type ( nwstm )   = ttype
                              timstr ( nwstm ) = carr ( 4 )
                              waid ( nwstm )   = tid
			      tnum ( nwstm )   = ttnum
			      isstr ( nwstm )  = carr ( 3 )
C
C*			      Decode Action Code.
C
			      IF (carr1 (3)(1:2) .eq. 'ER') THEN
			        tic(nwstm) = icauses(1)
			      ELSE IF (carr1 (3)(1:2) .eq. 'SM') THEN
				tic(nwstm) = icauses(2)
			      ELSE IF (carr1 (3)(1:2) .eq. 'RS') THEN
                                tic(nwstm) = icauses(3)
			      ELSE IF (carr1 (3)(1:2) .eq. 'DM') THEN
                                tic(nwstm) = icauses(4)
			      ELSE IF (carr1 (3)(1:2) .eq. 'IJ') THEN
                                tic(nwstm) = icauses(5)
			      ELSE IF (carr1 (3)(1:2) .eq. 'GO') THEN
                                tic(nwstm) = icauses(6)
			      ELSE IF (carr1 (3)(1:2) .eq. 'IC') THEN
                                tic(nwstm) = icauses(7)
			      ELSE IF (carr1 (3)(1:2) .eq. 'FS') THEN
                                tic(nwstm) = icauses(8)
			      ELSE IF (carr1 (3)(1:2) .eq. 'FT') THEN
                                tic(nwstm) = icauses(9)
			      ELSE IF (carr1 (3)(1:2) .eq. 'ET') THEN
                                tic(nwstm) = icauses(10)
			      ELSE IF (carr1 (3)(1:2) .eq. 'WT') THEN
                                tic(nwstm) = icauses(11)
			      ELSE IF (carr1 (3)(1:2) .eq. 'DR') THEN
                                tic(nwstm) = icauses(12)
			      ELSE IF (carr1 (3)(1:2) .eq. 'MC') THEN
                                tic(nwstm) = icauses(13)
			      ELSE IF (carr1 (3)(1:2) .eq. 'OT') THEN
                                tic(nwstm) = icauses(14)
			      ELSE IF (carr1 (3)(1:2) .eq. 'UU') THEN
                                tic(nwstm) = icauses(15)
			      ELSE
			        tic(nwstm) = " "
			      ENDIF
			      DO jj = 1, npt ( nwstm )
				  IF (carr (6) .eq. " ") THEN
				    timstp ( jj, nwstm ) = carr ( 5 )
				  ELSE
				    timstp ( jj, nwstm ) = carr ( 6 )
				  ENDIF
			      END DO
			      itest( nwstm ) = jtest
			      cncld ( nwstm )  = .false.
			    ELSE IF ( ckcncl ) THEN
C
C*                            This is a cancellation which appears to
C*                            match with a previous winter storm
C*                            message.  If a match on the zone name is
C*                            found, change the end time for the zone to
C*                            the cancellation time and set a flag.
C
			      DO ii = 1, nwstm
				IF ( cancel ( ii ) ) THEN
				    DO jj = 1, npt (  ii )
					DO kk = 1, knt
					    IF ( fipnam ( jj,ii ) .eq.
     +					         fipnam ( kk,jw ) ) THEN
					      timstp ( jj,ii ) = carr(4)
					      cncld ( ii ) = .true.
					    END IF
					END DO
				    END DO
				END IF
			      END DO
C
                            END IF
			  END IF
                      END IF
                    END IF
                  END DO
C
                  CALL FL_CLOS ( lunf, ier )
C
                END IF
C
            END IF
            ifl = ifl + 1
        END DO
C
C*      Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
C*      Set attributes and defaults.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  i = 1, 2
	    IF  ( ( mrktyp(i) .lt. 17 ) .or.
     +		  ( mrktyp(i) .gt. 21 ) ) THEN
		 mrktyp(i) = 20
	    END IF
C
	    IF  ( ( ssize(i) .le.  0.0 ) .or.
     +		  ( ssize(i) .gt. 10.0 ) )  ssize(i) = 1.0
C
	    IF  ( ( iwidth(i) .le.  0 ) .or.
     +		  ( iwidth(i) .gt. 10 ) )  iwidth(i) = 3
C
            IF  ( ( lwidth(i) .le.  0 ) .or.
     +            ( lwidth(i) .gt. 10 ) )  lwidth(i) = 3
	END DO
C
C*      Get message type to determine plotting hierarchy.
C
	DO ip = 1, nwstm
	    IF  ( type ( ip ) .eq. 'FF' )  THEN
	        mtype ( ip ) = JFF
	    ELSE IF  ( type ( ip ) .eq. 'FA' ) THEN 
		mtype ( ip ) = JFA
	    ELSE
		mtype ( ip ) = 0
	    ENDIF
	ENDDO
C
C*      Plot the graphic for each valid report.  Plot Flash Flood watches, 
C*      then ariel flood watches
C
	DO jtype = 1, 2, 1
	  DO ip = 1, nwstm
	    IF ( jtype .eq. mtype ( ip ) ) THEN
	      CALL TI_DTM4 ( timstr ( ip ), tmstr4, ier )
	      CALL TI_DTM4 ( timstp ( 1, ip ), tmstp4, ier )
	      IF ( cncld ( ip ) ) THEN
		tmcnl ( 1 ) = tmstp4
		DO jj = 2, npt ( ip )
		    CALL TI_DTM4 ( timstp ( jj, ip ), tmcnl (jj), ier )
		    IF ( tmcnl (jj) .gt. tmstp4 ) tmstp4 = tmcnl (jj)
		END DO
	      ELSE
		DO jj = 1, npt ( ip )
		    tmcnl ( jj ) = tmstp4
		END DO
	      END IF
	      IF  ( ( dattim .eq. 'ALL' ) .or.
     +		    ( ( tmstp4 .gt. dattm4 ) .and.
     +		      ( tmstr4 .le. dattm4 ) ) )  THEN
C
C*		  Set the color and marker based on the message type.
C
		  ic = icolor(jtype)
		  IF ( itest ( ip ) .eq. 0 ) THEN
		      mrkr = mrktyp ( jtype )
		  ELSE
		      mrkr = mrktst ( mrktyp ( jtype ) )
		  END IF
		  CALL GSMRKR ( mrkr, 0, ssize(jtype), iwidth(jtype),
     +				ier )
C
C*		  Plot each zone in the Flash Flood.
C
		  IF  ( ic .ne. 0 )  THEN
		    CALL GSCOLR ( ic, ier )
		    IF ( npt (ip) .gt. NC ) THEN
			npt (ip) = NC
			CALL ER_WMSG ( 'GG', 2, 'GGFFA', ierr )
		    END IF
		    IF ( npt (ip) .gt. 0 ) THEN
			DO  im = 1, npt (ip)
			  IF ( ( dattim .eq. 'ALL' ) .or.
     +			       ( tmcnl ( im ) .gt. dattm4 ) ) THEN
C
C*			    Draw the marker or outline the county.
C
			    CALL ST_INCH ( nfips(im,ip), cfips, ier )
			    IF ( fipnam(im,ip)(3:3) .eq. 'C' ) THEN
			      bndtyp = 'CNTY_BNDS|<FIPS>'//cfips
			    ELSE
			      bndtyp = 'PFZ|<FIPS>'//cfips
			    ENDIF
			    IF ( ( iflags (JFIL) .eq. 0 ) .or.
     +				 ( itest ( ip ) .ne. 0 ) ) THEN
C
				CALL GPLBND ( bndtyp,
     +                              0,0,0,0,0, 0,0,0,
     +                              'MARK', ic, mrkr, ssize(jtype),
     +                              iwidth(jtype), ier )
			    ELSE
C
			       CALL GPLBND ( bndtyp,
     +                              0,0,0,0,0, ic,1,lwidth(jtype),
     +                              ' ',0,0,0.,0, ier )
                            END IF
C
C*                          Plot the text.
C
			    IF ( ( iflags (JTIM) .eq. 0 ) .and.
     +				 ( iflags (JLAB) .eq. 0 ) .and.
     +				  (iflags (JIC) .eq. 0 ) ) THEN
				iyoff  = -1
				wlabel = ' '
C
			    ELSE IF  ( ( iflags (JTIM) .eq. 0 ) .and.
     +				       ( iflags (JLAB) .ne. 0 ) .and.
     +				       ( iflags (JIC) .eq. 0 ) ) THEN
			      iyoff  = -2
			      CALL ST_LSTR ( cnnam (im,ip), lencn,
     +                                       ier )
			       wlabel = cnnam(im,ip)(:lencn)
C
			    ELSE IF ( ( iflags (JTIM) .ne. 0 ) .and.
     +				      ( iflags (JLAB) .eq. 0 ) .and.
     +                                ( iflags (JIC) .eq. 0 ) ) THEN
			      iyoff  = -3
			      wlabel = timstr(ip) (5:11) // '-' //
     +				       tmcnl(im) (7:13)
C
			    ELSE IF ( ( iflags (JTIM) .ne. 0 ) .and.
     +				      ( iflags (JLAB) .ne. 0 ) .and.
     +                                ( iflags (JIC) .eq. 0 ) ) THEN
			      iyoff  = -3
			      CALL ST_LSTR ( cnnam (im,ip), lencn,
     +                                       ier )
			      wlabel = cnnam(im,ip) (:lencn) // CHCR
     +				       // timstr(ip) (5:11) // '-' //
     +				       tmcnl(im) (7:13)
			    ELSE IF ( ( iflags (JTIM) .eq. 0 ) .and.
     +                                ( iflags (JLAB) .eq. 0 ) .and.
     +                                ( iflags (JIC) .ne. 0 ) ) THEN
			      iyoff  = -2
			      wlabel = tic (ip)
			    ELSE IF ( ( iflags (JTIM) .ne. 0 ) .and.
     +                                ( iflags (JLAB) .eq. 0 ) .and.
     +                                ( iflags (JIC) .ne. 0 ) ) THEN
			      iyoff  = -3
			      wlabel = timstr(ip) (5:11) // '-' //
     +                                 tmcnl(im) (7:13) // CHCR // 
     +                                 tic (ip)
			    ELSE IF ( ( iflags (JTIM) .eq. 0 ) .and.
     +                                ( iflags (JLAB) .ne. 0 ) .and.
     +                                ( iflags (JIC) .ne. 0 ) ) THEN
			      iyoff  = -3
                              CALL ST_LSTR ( cnnam (im,ip), lencn,
     +                                       ier )
                              wlabel = cnnam(im,ip) (:lencn) // CHCR 
     +				       // tic (ip)
			    ELSE IF ( ( iflags (JTIM) .ne. 0 ) .and.
     +                                ( iflags (JLAB) .ne. 0 ) .and.
     +                                ( iflags (JIC) .ne. 0 ) ) THEN
                              iyoff  = -4
                              CALL ST_LSTR ( cnnam (im,ip), lencn,
     +                                       ier )
                              wlabel = cnnam(im,ip) (:lencn) // CHCR
     +                                 // timstr(ip) (5:11) // '-' //
     +                                 tmcnl(im) (7:13) // CHCR // 
     +				       tic (ip)
			    END IF
C
			    CALL GTEXT ( 'M', rlat(im,ip), rlon(im,ip),
     +					 wlabel, 0.0, 0, iyoff, ier )
			  END IF
			END DO
		    END IF
		  END IF
	      ENDIF
	    ENDIF
	  END DO
	ENDDO
C
C*      Reset the saved attributes.
C
        CALL GSCOLR ( jcolr, ier )
        CALL GSMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
        CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +                jrrotn, jjust, ier )
C*
        RETURN
        END

