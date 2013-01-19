	SUBROUTINE GG_CSIG ( dattim, icolor, lwidth, iflags, iret )
C************************************************************************
C* GG_CSIG								*
C*									*
C* This subroutine plots the current convective sigmets and outlooks    *
C* for the lower 48 states and the adjacent coastal waters.             *
C*									*
C* GG_CSIG ( DATTIM, ICOLOR, LWIDTH, IFLAGS, IRET )			*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Ending time for sigmet/outlook  *
C*	ICOLOR (4)	INTEGER		Line colors			*
C*	LWIDTH (4)	INTEGER		Line widths			*
C*	IFLAGS (5)	INTEGER		Flags for labels		*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02						*
C* A. Hardy/NCEP	11/02	Plot the latest hour of sigmets		*
C* A. Hardy/NCEP	 5/03	Change diameter to radius for 'IS'  	*
C* A. Hardy/NCEP	 1/04	Reworked plotting of conv. sigs.	*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM         	*
C* F. J. Yen/NCEP	 6/04   Added extrap. 1-hr & 2-hr forecasts	*
C* F. J. Yen/NCEP	 6/04   Added check for incrementing ncsig and	*
C*				increased NW from 1000 to 1200.		*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 1200 )
C
	PARAMETER	( JCOL = 4 )
	PARAMETER	( JSEQ = 1, JTIM = 2, JDSP = 3, JFLT = 4,
     +                    JINT = 5, JSEQ1 = 6, JSEQ2 = 7 )
C*
	CHARACTER*(*)	dattim
	INTEGER		icolor(*), lwidth(*), iflags(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*100, tfile*128, label*16, dattm4*20, 
     +			tmstp4*20, tmstr4*20, carr (12)*20, tid*4, 
     +			ttype*2 
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
	CHARACTER	type (NW)*2, timstr (NW)*20, timstp (NW)*20,
     +			waid (NW)*4, flevel (2,NW)*4,
     +                  cintsy (NW)*7, cdir (NW)*3, cspd (NW)*2,
     +                  cdst (NW)*3, tmptyp(NW)*2, tmpend (NW)*20,
     +			tmpid (NW)*4, tmpbeg (NW)*20
	INTEGER		npt (NW), itest (NW)
	REAL		rlat (50,NW), rlon (50,NW), tlat (50), tlon (50)
	REAL		rlat0 (50,NW), rlon0 (50,NW)
	REAL		dlat, dlon
C*
	CHARACTER	stime*20, flstrt*160
	INTEGER		itarr (5), jtarr (5), itype
	LOGICAL		done, match, extrap
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan the directory for all of the convective sigmet/outlook  
C*	data files.
C
	filnam = 'CSIG'
	path  = ' '
	templ = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, 
     +                   ic, is, if, ir, ii, ion, ihb, mnb, iha, mna,
     +			 mstrct, idtmch, ier)
        CALL ST_RNUL ( path, path, lens, ier )
        CALL ST_RNUL ( templ, templ, lens, ier )
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
		return
	    ENDIF        
	    CALL FL_MNAM ( dattm2, templ, filnam, ier )
	END IF
C
C*	Find the earliest file to start searching. For ALL times
C*	go back 10 days, for any other entry for dattim subtract
C*	1 day from the time given.
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
	ncsig = 0
	done  = .false.
	ifl   = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	    IF  ( files(ifl) .gt. filnam )  THEN
		done = .true.
	      ELSE IF  ( ncsig .ge. NW )  THEN
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
		  DO WHILE  ( iostat .eq. 0 .and. ( .not. done ) )
		    READ ( lunf, 2, IOSTAT = iostat ) buffer
2		    FORMAT ( A )
		    IF  ( iostat .eq. 0 )  THEN
		       IF  ( buffer ( 1:1 ) .eq. '|' )  THEN
			  CALL ST_CLST ( buffer, '|', ' ', 12, carr,
     +					 num, ier )
			  CALL ST_NUMB ( carr ( 12 ), jflag, ier )
			  jcorr = MOD ( jflag, 3 )
			  ttype = carr ( 2 )
			  tid   = carr ( 5 )
C
 			  IF ( jcorr .eq. 1 ) THEN
C
C*			    This is a correction.  Change the end time
C*			    of the original convective sigmet to the 
C*			    correction start time.
C
			    DO ii = 1, ncsig
				IF ( ( tid .eq. waid (ii) ) .and.
     +				     ( type(ii) .eq. ttype ) ) THEN
				      timstp ( ii ) = carr ( 3 )
				END IF
			    END DO
  			  END IF
C
C*			  Read the lat/lon coordinates from the file.
C
			  knt = 0
			  jostat = 0
			  DO WHILE  ( jostat .eq. 0 )
			    READ ( lunf, 2, IOSTAT = jostat ) buffer
			    IF  ( jostat .eq. 0 )  THEN
				IF  ( buffer ( 1:1 ) .eq. '|' )  THEN
				    CALL FL_BKSP ( lunf, ier )
				    jostat = -1
				  ELSE
				    knt = knt + 1
				    READ ( buffer, 1000 ) 
     +					   tlat ( knt ), tlon ( knt )
1000				    FORMAT ( 2F9.2 )
				END IF
			    END IF
			  END DO
			  IF ( ncsig .lt. NW ) THEN
C
C*			    Add this report.
C
			    ncsig = ncsig + 1    
			    type ( ncsig )      = ttype
			    timstr ( ncsig )    = carr ( 3 )
			    timstp ( ncsig )    = carr ( 4 )
			    waid ( ncsig )      = tid
			    cintsy ( ncsig )    = carr ( 6 )
			    cdir ( ncsig )      = carr ( 7 )
			    cspd ( ncsig )      = carr ( 8 )
			    flevel ( 1, ncsig ) = carr ( 9 )
			    flevel ( 2, ncsig ) = carr ( 10 ) 
			    cdst ( ncsig )      = carr ( 11 )
			    itest ( ncsig )     = jtest  		 
			    DO kk = 1, knt
			      rlat0 ( kk, ncsig ) = tlat ( kk )
			      rlon0 ( kk, ncsig ) = tlon ( kk )
			    END DO
			    npt ( ncsig ) = knt
			   ELSE
			    done = .true.
			  END IF   
		       END IF
		    END IF
		  END DO
C
		  CALL FL_CLOS ( lunf, ier )
		END IF
	    END IF
	    ifl = ifl + 1
	END DO
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQMRKR ( jmark, jmkhw, szmark, imkwid, ier )
	CALL GQLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
C*	Set attributes.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  i = 1, 4
	    IF  ( ( lwidth (i) .le.  0 ) .or.
     +		  ( lwidth (i) .gt. 10 ) )  lwidth (i) = 2
	END DO
C
C*      Initialize the temporary type and seq. id arrays.
C
        DO ik = 1, NW
           tmptyp(ik) = ' '
           tmpbeg(ik) =  ' '
           tmpend(ik) =  ' '
           tmpid (ik) = ' '
        END DO 
C
C*	Plot the graphic for each valid report.
C
        ik = 0
	DO ip =  ncsig, 1, -1
            match = .false.
	    CALL TI_DTM4 ( timstp ( ip ), tmstp4, ier )
	    CALL TI_DTM4 ( timstr ( ip ), tmstr4, ier )
	    IF  ( ( dattim .eq. 'ALL' ) .or.
     +		  ( ( tmstp4 .gt. dattm4 ) .and.
     +		    ( tmstr4 .le. dattm4 ) ) )  THEN
C
C*              Save a list of most current Convective/outlook sigmets.
C*              Don't plot those that have already been plotted.
C
                IF  ( ik .eq. 0 ) THEN
                    ik = ik + 1
                    tmptyp(ik) = type ( ip )
                    tmpid (ik) = waid( ip )
                    tmpbeg(ik) = tmstr4 
                    tmpend(ik) = tmstp4 
                  ELSE
C
C*                  Check for area, line or isolated.
C
                    IF ( ( type(ip) .eq. 'AR' ) .or. 
     +			 ( type(ip) .eq. 'LN' ) .or.
     +			 ( type(ip) .eq. 'IS' ) .or.
     +		         ( type(ip) .eq. 'CS' )  )THEN

                        CALL ST_LSTR ( waid(ip), il, ier )
                        DO ikl = 1, ik
                            CALL ST_LSTR ( tmpid(ikl), il2, ier )
                            IF ( tmptyp(ikl) .ne. 'OL' ) THEN
                              IF  ( waid(ip)(il:il) .eq. 
     +                             tmpid(ikl)(il2:il2) ) THEN
                                IF ( ( tmstr4 .lt. tmpbeg (ikl) ) .or.
     +                             ( tmstp4 .lt. tmpend (ikl) ) ) THEN
                                    match = .true.
                                END IF
                              END IF
                            END IF
                        END DO
                      ELSE
C
C*                      Check for outlook.
C
                        CALL ST_LSTR ( waid(ip), il, ier )
                        DO ikl = 1, ik
                            CALL ST_LSTR ( tmpid(ikl), il2, ier )
                            IF  ( waid(ip)(il:il) .eq. 
     +                                     tmpid(ikl) (il2:il2) ) THEN
                                IF ( ( tmstp4 .lt. tmpend (ikl) ) .and.
     +                              ( tmstr4 .lt. tmpbeg (ikl) ) ) THEN 
                                    match = .true.
                                  else IF  ( waid(ip) .eq. 
     +                                     tmpid(ikl)  ) THEN
                                    match = .true.
                                END IF
                            END IF
                        END DO 
                    END IF
C
                    IF ( .not. match )  THEN
                        ik = ik + 1
                        tmptyp(ik) = type ( ip )
                        tmpid (ik) = waid( ip )
                        tmpend(ik) = tmstp4 
                        tmpbeg(ik) = tmstr4 
                    END IF
                END IF
                IF ( .not. match ) THEN
C
C*		    Process for each hour ( 0-hr initial, 1-hr
C*		    extrapolated, and 2-hr extrapolated)
C
C
C*		    Find the earliest hour being plotted (to identify
C*		    which hour the four label flags apply)
C
		    ihre = -1
		    labhr = 0
		    DO ih = 3, 1, -1
			IF ( icolor (ih) .ne. 0 ) ihre = ih
		    END DO
		
		    ic = 0 
		    DO ihr = 1, 3
C
C*			Set the color and line type based on the
C*			convective type.  (icolor(1) is the color
C*			number for the initial hour; icolor(2) is
C*			the color number for the 1-hr extrapolated
C*			forecast; and icolor(3) is the color number
C*			for the 2-hr extrapolated forecast.
C
			lwid = lwidth (ihr)
			iltyp = 1
		        IF  ( type ( ip ) .eq. 'AR' .or.
     +			      type ( ip ) .eq. 'LN' )  THEN
		            ic = icolor (ihr)
		            CALL GSLINE ( iltyp, 0, lwid, 0, ier )
C
		          ELSE IF ( type ( ip ) .eq. 'IS' ) THEN
		            ic = icolor (ihr)
		            CALL GSLINE ( iltyp, 0, lwid, 0, ier )
                            CALL GSMRKR ( 1, 0, 0.7, lwid, ier )
C
		          ELSE IF ( ihr .eq. 1 .and.
     +				    type ( ip ) .eq. 'OL' ) THEN
		            ic = icolor (JCOL)
		            CALL GSLINE ( iltyp, 0, lwidth (JCOL), 0,
     +					  ier )
		        END IF
C
C*		        Draw the shape for a bounded type. 
C
		        IF ( ( ic .ne. 0 ) .and.
     +				( npt (ip) .ne. 0 ) ) THEN
C
C*		            Set lat/lon arrays
C
			    extrap = .false.
		            IF ( ihr .eq. 1 ) THEN
		    	        DO ll = 1, npt (ip)
			            rlat (ll,ip) = rlat0 (ll,ip)
			            rlon (ll,ip) = rlon0 (ll,ip)
			        END DO
		              ELSE
C
C*		   	        Calculate lat/lon for extrapolated hour
C*			        using subroutine clo_dltln. 
C
C*			        First determine the distance in meters
C*			        by getting the speed in knots.
C
		 	        CALL ST_CRNM ( cspd (ip), spdknt, ier )
				IF ( spdknt .ne. -9999 .and. 
     +					cdir (ip) .ne. '  ' ) THEN 
				    extrap = .true.
			            distm = spdknt * float(ihr-1)
     +						 * 1852.0
C
C*			            Wind direction dir is counter-
C*				    clockwise from N.  So, it needs to
C*				    be reversed to clockwise for call
C*				    to CLO_DLTLN.
C
		 	            CALL ST_CRNM ( cdir (ip), dir, ier )
			
		 	            IF ( dir .ge. 180. ) THEN
			                dir = dir - 180.
			              ELSE
			                dir = dir + 180.
			            END IF
		                    DO ll = 1, npt (ip)
			                CALL CLO_DLTLN ( rlat0 (ll,ip),
     +				            rlon0 (ll,ip), distm, dir,
     +				            rlat (ll,ip), rlon (ll,ip),
     +					    ier )
		                    END DO
				END IF
		            END IF

		            npts = npt ( ip )
		            CALL GSCOLR ( ic, ier )
                             IF ( ( type ( ip ) .eq. 'AR' ) .and. 
     +				    ( ihr .eq. 1 .or. extrap ) )  THEN
		                CALL GLINE ( 'M', npts , rlat ( 1, ip ),
     +			                 rlon ( 1, ip ), ier )
C
		              ELSE IF ( ( type ( ip ) .eq. 'LN' ) .and.
     +				    ( ihr .eq. 1 .or. extrap ) )  THEN
 		                CALL GLINE ( 'M', npts , rlat ( 1, ip ),
     +			                 rlon ( 1, ip ), ier )
C
		              ELSE IF ( ( type ( ip ) .eq. 'IS' ) .and.
     +				    ( ihr .eq. 1 .or. extrap ) )  THEN
                                CALL ST_CRNM ( cdst(ip), dist, ier )
C                          
C*                              Convert from NM to meters then divide
C*			        in half for radius in meters.
C 
                                dist = ( dist * 1852.0 ) / 2.0
		                CALL CLO_DLTLN ( rlat(1,ip),rlon (1,ip),
     +				         dist, 0., dlat, dlon, ier)
                                IF ( ier .eq. 0 ) THEN
		                    CALL GCIRCL ( 'M', rlat (1, ip), 
     +			                  rlon (1, ip), dlat, dlon, 
     +					  36, ier )
		                    CALL GMARK ( 'M', 1, rlat (1, ip), 
     +                                   rlon (1, ip), ier )
                                    alat = dlat
                                    alon = dlon
                                END IF
C
		              ELSE IF ( type ( ip ) .eq. 'OL' .and.
     +					ihr .eq. 1 ) THEN
C
C*				Outlook should be plotted only once
C
		                CALL GLINE ( 'M', npts , rlat ( 1, ip ),
     +			                 rlon ( 1, ip ), ier )
		            END IF
C
C*                          Find the label location and set up labels
C*			    for the earliest hour plotted
C
			    iyoff = 0 
		            IF ( ( ihr .eq. ihre) 	    .and. 
     +				 ( ihr .eq. 1 .or. extrap ) .and.
     +				 ( type (ip) .ne. 'OL' )         .and.
     +				 ( ( iflags ( JTIM ) .ne. 0 ) .or.
     +		                   ( iflags ( JDSP ) .ne. 0 ) .or.
     +		                   ( iflags ( JFLT ) .ne. 0 ) .or.
     +		                   ( iflags ( JINT ) .ne. 0 ) ) ) THEN
				labhr = ihr
                                IF ( type (ip) .ne. 'IS' ) THEN
		                    CALL GG_TLBL ( npts, rlat ( 1, ip ),
     +				      		rlon ( 1, ip ), alat,
     +						alon, ier )
                                END IF
C
C*		                Plot the intensity label.
C
		                IF ( iflags ( JINT ) .ne. 0 ) THEN
                                    IF (  cintsy(ip) .ne. ' ' ) THEN
		                        iyoff = 2 + iyoff
		                        label  = cintsy (ip)
 		                        CALL GTEXT ( 'M', alat, alon,
     +					     label, 0.0, 0, iyoff, ier )
                                    END IF
		                END IF
C
C*		                Plot the direction and speed label.
C
		                IF ( iflags ( JDSP ) .ne. 0 ) THEN
                                    IF (  cdir(ip) .ne. ' ' ) THEN
		                        iyoff = 2 + iyoff
		                        label  = cdir (ip) //' '//
     +					          cspd (ip) //  'kt'
 		                        CALL GTEXT ('M', alat, alon,
     +						label, 0.0, 0, iyoff, ier )
				     END IF
		                END IF
C
C*		                Plot the flight level label.
C
 		                IF ( iflags ( JFLT ) .ne. 0 ) THEN
 		                    IF ( flevel (1,ip) .ne. ' ' ) THEN
C
C*			                Plot the flight level.
C
		                        iyoff = 2 + iyoff
 		                        label  = 'FL' //
     +						    flevel ( 1, ip )
		                	ixoff = 0
 		                        CALL GTEXT ('M', alat, alon,
     +						label, 0., ixoff,
     +						iyoff, ier )
 			            END IF
 		                END IF
C
C*		                Plot the time label.
C
		                IF ( iflags ( JTIM ) .ne. 0 ) THEN
		                    iyoff = 2 + iyoff
		                    label  = timstp ( ip ) ( 5:11 )
 		                    CALL GTEXT ( 'M', alat, alon,
     +					    label, 0.0, 0, iyoff, ier )
		                END IF
		            END IF
C
C*		            Plot the Sequence number label.
C
			    IF ( ihr .eq. 1 ) THEN
				kseq = JSEQ
			      ELSE IF ( ihr .eq. 2 ) THEN
				kseq = JSEQ1
			      ELSE
				kseq = JSEQ2
			    END IF
			    IF ( type (ip) .eq. 'OL' ) THEN
				IF ( ihr .eq. 1 .and.
     +				        iflags (JSEQ) .ne. 0 ) THEN
		                    CALL GG_TLBL ( npts, rlat ( 1, ip ),
     +				      		rlon ( 1, ip ), alat,
     +						alon, ier )
		                    iyoff = 2 + iyoff
		                    label  = waid ( ip ) // ' OUTLOOK'
                                    CALL ST_RXBL (label, label, len,ier)
		                    CALL GTEXT ( 'M', alat, alon, label,
     +			    	                 0.0, 0, iyoff, ier )
				END IF
			      ELSE IF ( iflags ( kseq ) .ne. 0 ) THEN
				IF ( ihr .eq. 1 .or. EXTRAP ) THEN
				    IF (  labhr .eq. 0 .or.
     +					        labhr .ne. ihr ) THEN
                                        IF ( type (ip) .ne. 'IS' ) THEN
		                            CALL GG_TLBL ( npts,
     +					           rlat ( 1, ip ),
     +					           rlon ( 1, ip ),
     +					           alat, alon, ier )
                                        END IF
				    END IF
		                    iyoff = 2 + iyoff
		                    label  = waid ( ip ) 
		                    CALL GTEXT ( 'M', alat, alon, label,
     +			    	                 0.0, 0, iyoff, ier )
				END IF
			    END IF
		        END IF
		    END DO
	        END IF
	    END IF
	END DO
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSMRKR ( jmark, jmkhw, szmark, imkwid, ier )
	CALL GSLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C*
	RETURN
	END
