	SUBROUTINE GG_WWCN ( filtyp, dattim, num, icolor, icolr2,
     +			     mrktyp, ssize, iwidth, iflags, iret )
C************************************************************************
C* GG_WWCN								*
C*									*
C* This subroutine reads the current watch county notification (WCN) 	*
C* for each watch number.						*
C*									*
C* GG_WWCN ( FILTYP, DATTIM, NUM, ICOLOR, ICOLR2, MRKTYP, SSIZE, IWIDTH,*
C*           IFLAGS, IRET ) 						*
C*									*
C* Input parameters:							*
C*      FILTYP          CHAR*           File type 'WCN'			*
C*	DATTIM		CHAR*		Ending time for warning or watch*
C*	NUM		INTEGER		Number of markers		*
C*	ICOLOR (NUM)	INTEGER		Marker and outline colors	*
C*	ICOLR2 (NUM)	INTEGER		Fill colors                	*
C*	MRKTYP (NUM)	INTEGER		Marker symbol types		*
C*	SSIZE  (NUM)	REAL		Marker symbol sizes		*
C*	IWIDTH (NUM)	INTEGER		Marker symbol widths		*
C*	IFLAGS (5)	INTEGER		Flags for time, label, watch #, *
C*					outline, and color code		*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		03/03	                                       	*
C* A. Hardy/NCEP	 3/03	Changed call for GG_WWCR		*
C* A. Hardy/NCEP         4/03   Fixed watch number check		*
C* A. Hardy/NCEP         7/03   Added 'mndif' to GG_WWCR		*
C* A. Hardy/NCEP	 1/04	Added state string to GG_WWCR		*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM     	*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* T. Lee/SAIC		10/04	Increased CWCN size to append NULL	*
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* M. Li/SAIC		01/05	Added new input parameter ICOLR2	*
C* A. Hardy/NCEP	03/05   Added reading action code parameter	*
C* M. Li/SAIC		10/05	Fix display of cancelled counties	*
C* F. J. Yen/NCEP	11/05	Fixed CAN display for #855 (11/9)	* 
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP	03/07	Removed unused array cnties.		*
C* F. J. Yen/NCEP	01/08	Check for duplicate WCNs to fix counties*
C*				not displayed.  Fixed array size check. *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'

     	PARAMETER	( NW = 1000, NC = 500 )
C*
	CHARACTER*(*)	filtyp, dattim
	INTEGER		icolor(*), mrktyp(*), iwidth(*), iflags(*), 
     +			icolr2(*)
	REAL		ssize(*)
C*
        CHARACTER       wstn(NW)*7, timstr(NW)*20, timstp(NW)*20,
     +                  wtype(NW)*3, timisu(NW)*20, worig(NW)*4
        INTEGER         itest(NW), npt(NW), icancl(NW), nfips(NC,NW)
        REAL            rlat(NC,NW), rlon(NC,NW)
        CHARACTER       cnnam(NC,NW)*32


	INTEGER		iuwtch(NW)
	CHARACTER	tzone(NW)*3

        CHARACTER       path*25, templ*(MXTMPL), cdttm*20, dattm2*20,
     +                  buffer*128, tfile*128, dattm4*20, carr(10)*128,
     +                  stime*20, flstrt*160, flend*160
        CHARACTER*(MXFLSZ)      filnam, files (MXNMFL), cwcn
C*
        CHARACTER       arlin(NW)*80, timiss(100)*20,
     +                  sissue(20)*11, swnum(20)*4, sorig(20)*4,
     +                  tmstrt(100)*20, tmstp(100)*20
C
        INTEGER         arnum(NW), itarr(5), jtarr(5), inarr, itype
c       SAVE            sissue, swnum, sorig, inarr
        LOGICAL         done, good, gdtim, dup, found
C*
        DATA            sissue / 20*' ' /, swnum/ 20*' ' /,
     +                  sorig / 20*' ' /
C*
C-----------------------------------------------------------------------
	iret = 0
C
C*	Get array of active WCNs
C
	cwcn = 'WCN'
        mndif = 0
	CALL GG_WWCR ( dattim, cwcn, mndif, iuwtch, iunum, tzone, 
     +                 timiss, tmstrt, tmstp, ier )
        ip = 0 

        DO inm = 1, iunum       	
	   ipnm = 0
           icnt   = 0
           inarr  = 0
           done   = .false.
C
C*         Scan the WCN directory for all the WCN data files.
C
	   CALL ST_NULL ( cwcn, cwcn, nf, ier )
	   path  = ' '
	   templ = ' '
	   CALL CTB_DTGET ( cwcn, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	   CALL ST_RNUL ( path, path, lens, ier )
	   CALL ST_RNUL ( templ, templ, lens, ier )
           CALL ST_LSTR ( path, lenp, ier )
           nexp   = MXNMFL
           iorder = -1
           CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*         Check for the last file requested by the user.
C
           CALL ST_LCUC ( dattim, dattim, ier )
	   itype = 1
           IF  ( ( dattim .eq. 'LAST' ) .or.
     +        ( dattim .eq. 'ALL' ) )  THEN
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
C
              CALL FL_MNAM ( dattm2, templ, filnam, ier )
           END IF
C
C*         Find the earliest file to start searching. For ALL times
C*         go back 10 days, for any other entry for dattim subtract
C*         one day from the time given.
C
           IF  ( dattim .eq. 'ALL' )  THEN
              minuts = 14400
            ELSE
              minuts = 1440 
           END IF
C
C*	    Find end file name
C
	   CALL TI_CTOI ( dattm2, itarr, ier )
	   CALL TI_ITOC ( itarr, stime, ier )
	   CALL FL_MNAM ( stime, templ, flend, ier )
           CALL ST_LSTR ( flend, len, ier )
C
C*	   Find start file name
C
           CALL TI_CTOI ( dattm2, itarr, ier )
           CALL TI_SUBM ( itarr, minuts, jtarr, ier )
           CALL TI_ITOC ( jtarr, stime, ier )
           CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*         Get 4-digit year to compare dates.
C
           CALL TI_DTM4 ( dattm2, dattm4, ier )
C
C*         Decode each file until the end time is reached.
C
           nwtch = 0
           jj = 1
           done  = .false.
           good  = .false.
           ifl = 1
           DO WHILE  ( ( ifl .le. nfile) .and. ( .not. done ) )
            IF  ( ifl .gt. nfile )  THEN
                done = .true.
              ELSE
                IF  ( ( files(ifl) .ge. flstrt ) .and.
     +                  ( files(ifl) .le. flend ) ) THEN
                  tfile = path(:lenp) // '/' // files(ifl)
                  CALL FL_SOPN ( tfile, lunf, ier )
C
C*                Find header '|' lines  and store their locations and
C*                information lines.
C
                  iostat = 0
                  iln = 0
                  ii = 1
                  icnt = 0
                  DO WHILE  ( iostat .eq. 0 )
                    READ ( lunf, 2, IOSTAT = iostat ) buffer
2		    FORMAT ( A )
                    IF  ( iostat .eq. 0 )  THEN
                        IF  ( buffer(1:1) .eq. '|' )  THEN
                           iln = iln + 1
                           arnum(iln) = ii - icnt
                           arlin(iln) = buffer
                           ii = ii+1
                           icnt = icnt + 1
C
C*                         Read through county lines looking for next '|'.
C
                           jostat = 0
                           DO WHILE  ( jostat .eq. 0 )
                               READ ( lunf, 2, IOSTAT = jostat ) buffer
                               IF  ( jostat .eq. 0 )  THEN
                                   IF  ( buffer(1:1) .eq. '|' )  THEN
                                       CALL FL_BKSP ( lunf, ier )
                                       jostat = -1
                                   END IF
                               END IF
                               ii = ii+1
                            END DO
                        END IF
                    END IF
                  END DO
C
C*                Start from bottom of file and find last '|' line
C*                and set buffer to first county line.
C*
C*                inlf = number of lines in data file.
C*                inf  = number of lines of info. '|'
C
                  inlf = ii - iln - 1
                  inf = iln
                  itot = inlf
                  DO kk =  inf,1, -1
                      gdtim = .false.
                      itot = inlf
                      ii = arnum(kk)
                      DO WHILE ( itot .ge. ii )
                          CALL FL_BKSP ( lunf, ier )
                          itot = itot - 1
                      END DO
                      READ ( lunf, 2, IOSTAT = jostat ) buffer
C
C*                    Check time differences.
C
                      CALL TI_DIFF ( dattim, arlin(kk)(6:16),
     +                               nmis, ier )
                      CALL TI_DIFF ( dattim, arlin(kk)(18:28),
     +                               nmst, ier )
                      CALL TI_DIFF ( dattim, arlin(kk)(30:40),
     +                               nmsp, ier )
                      IF ( nmis .ge. 0 ) THEN 
                          IF ( ( nmst .ge. 0 ) .or. 
     +			       ( arlin(kk)(52:54). eq. 'CAN' ) ) THEN
                              IF ( nmsp .le. 0 )  THEN
                                  gdtim = .true.
                                ELSE 
                                  CALL TI_DIFF ( arlin(kk)(18:28),
     +                                 arlin(kk)(30:40), nm, ier )
                                  IF ( nm .eq. 0 ) THEN
                                      gdtim = .true.
                                    ELSE
                                      gdtim = .false.
                                  END IF
                              END IF
                            ELSE 
                              gdtim = .false.
                          END IF
                        ELSE 
                          gdtim = .false.
                      END IF
C
                   CALL ST_NUMB ( arlin(kk)(42:45), iarlin, ier )
                   IF ( ( iarlin .eq. iuwtch(inm) ) .and.
     +                    ( gdtim ) ) THEN
                      IF ( inarr .eq. 0 ) THEN
                          match = 0
                          sissue (1) = arlin(kk)(6:16)
                          swnum (1)  = arlin(kk)(42:45)
                          sorig (1)  = arlin(kk)(47:50)
                          inarr = inarr + 1
                        ELSE 
                          ij = 1
                          match = 0
                          DO WHILE ( ij .le. inarr ) 
                              IF ( arlin(kk)(42:45) .eq. swnum(ij) ) 
     +                                                          THEN
                                  IF ( arlin(kk)(47:50) .eq. sorig (ij)) 
     +                                                          THEN
                                      match = 1
                                  END IF 
                                  IF ((arlin(kk)(6:16) .gt. sissue(ij))
     +					.and. 
     +				     (arlin(kk)(47:50) .eq. sorig (ij))) 
     +                                                          THEN
                                      match = 0
                                    ELSE IF ( ( arlin(kk)(6:16) .eq. 
     +                                          sissue(ij) ) .and.
     +                                        ( arlin(kk)(47:50) .eq. 
     +                                          sorig (ij) ) )  THEN
                                      match = 2
                                  END IF 
                              END IF 
                              ij = ij + 1
                          END DO
 
                          IF ( match .eq. 0 ) THEN
                              inarr = inarr + 1
                              sissue (inarr) = arlin(kk)(6:16)
                              swnum (inarr)  = arlin(kk)(42:45)
                              sorig (inarr)  = arlin(kk)(47:50)
                          END IF
                      END IF
C
C*                    Check length of significance code.
C
                      ione = 1
                      CALL ST_NOCC ( arlin(kk), ';', ione,  npos, ier)
                      IF ( npos .lt. 1 ) THEN
                          ipos = 56
                        ELSE
                          ipos = npos + 5
                      END IF
C 
 		      IF ( ( ( match .eq. 0 ) .or.
     +                       ( match .eq. 2 ) ) .and.
     +                       ( arlin(kk)(ipos:ipos) .eq. '0' ) ) THEN
			 ip = ip + 1
			 wtype(ip)  = arlin(kk)(2:4)
			 timisu(ip) = arlin(kk)(6:16)
			 timstr(ip) = arlin(kk)(18:28) 
			 timstp(ip) = arlin(kk)(30:40)
			 wstn(ip)   = arlin(kk)(42:45)
			 worig(ip)  = arlin(kk)(47:50)
C
C*			 IF ipnm is 0, then it is first record saved for
C*			 that watch.
C
			 IF ( ipnm .eq. 0 ) ipnm = ip
			
			 CALL ST_NUMB(arlin(kk)(ipos:ipos), 
     +			 	      icancl(ip), ier)
		         CALL ST_NUMB(arlin(kk)(ipos+2:ipos+2), 
     +                                     itest(ip), ier)
                      END IF
C
C*                    Store active county lines.
C
                      jostat = 0
		      im = 0
                      DO WHILE  ( jostat .eq. 0 )
                          IF  ( jostat .eq. 0 )  THEN
                              IF  ( buffer(1:1) .eq. '|' )  THEN
                                  CALL FL_BKSP ( lunf, ier )
                                  jostat = -1
                                ELSE
                                  IF ( ( ( match .eq. 0 ) .or.
     +                                  ( match .eq. 2 ) ) .and. 
     +                            ( arlin(kk)(ipos:ipos) .eq. '0'))THEN

				    im = im + 1
                                    IF ( im .le. NC ) THEN
                                        CALL ST_CLST ( buffer, ' ', 
     +                                     ' ', 10, carr, ignore, ier )
					cnnam(im, ip) = carr(3)
    					CALL ST_NUMB( carr(2), 
     +					           nfips(im, ip), ier )	 
					CALL ST_CRNM( carr(6),
     +					           rlat(im, ip), ier ) 
					CALL ST_CRNM( carr(7),
     +                                             rlon(im, ip), ier )
                                    END IF
				    IF ( im .eq. NC + 1 ) 
     +					    CALL ER_WMSG ( 'GG', 9,
     +						    'GGWWCN', ier )
                                  END IF
                              END IF
                          END IF

                          READ ( lunf, 2, IOSTAT = jostat ) buffer
                          IF  ( buffer(1:1) .eq. '|' )  THEN
C
C*                            Read last county: read to end of file. 
C
                              DO WHILE  ( jostat .eq. 0 )
                                  READ ( lunf, 2, IOSTAT = jostat ) 
     +                                                          buffer
                              END DO
                              READ ( lunf, 2, IOSTAT = jostat ) buffer
                          END IF
                      END DO
C*
                      IF ( ( ( match .eq. 0 ) .or.
     +                       ( match .eq. 2 ) ) .and.
     +                     ( arlin(kk)(ipos:ipos) .eq. '0' ) ) THEN
		          npt(ip) = im
		      END IF
C
C*		      Check for duplicate record saved
C
		      found = .false.
		      IF ( ipnm .ne. 0 .and. ip .gt. ipnm  ) THEN
			  id = ipnm 
			  DO WHILE ( .not. found .and. id .lt. ip )
			      IF ( ( npt    (id) .eq. npt    (ip) ).and.
     +			      	   ( worig  (id) .eq. worig  (ip) ).and.
     +			           ( timisu (id) .eq. timisu (ip) ).and.
     +			           ( timstr (id) .eq. timstr (ip) ).and.
     +			           ( timstp (id) .eq. timstp (ip) ).and.
     +			           ( icancl (id) .eq. icancl (ip) ).and.
     +			           ( wtype  (id) .eq. wtype  (ip) ).and.
     +				   ( itest  (id) .eq. itest  (ip) ))THEN
				  jd = 1
				  dup = .true.
				  DO WHILE ( dup .and.
     +						 jd .le. npt (id) )
				      IF ( nfips (jd,id) .ne.
     +						nfips (jd,ip) )
     +							 dup = .false.
				      jd = jd + 1
				  END DO
				  IF ( dup ) THEN
C
C*				      Duplicate record found,
C*				      so remove last one entered.
C
				      found = .true.
				      ip = ip - 1
				  END IF
			      END IF
			      id = id + 1
			  END DO
		      END IF
C 
                    ELSE
                      jostat = 0
                      DO WHILE  ( jostat .eq. 0 )
                          IF  ( jostat .eq. 0 )  THEN
                              IF  ( buffer(1:1) .eq. '|' )  THEN
                                  CALL FL_BKSP ( lunf, ier )
                                  jostat = -1
                              END IF
                          END IF
                          READ ( lunf, 2, IOSTAT = jostat ) buffer
                          IF  ( buffer(1:1) .eq. '|' )  THEN
C
C*                            Read last county: read to end of file. 
C
                              DO WHILE  ( jostat .eq. 0 )
                                  READ ( lunf, 2, IOSTAT = jostat ) 
     +                                                          buffer
                              END DO
                              READ ( lunf, 2, IOSTAT = jostat ) buffer
                          END IF
                      END DO
                   END IF 
                  END DO
C
C*                Close file.
C
                  CALL FL_CLOS ( lunf, ier )
C
                END IF
            END IF
            ifl = ifl + 1
          END DO
C
	END DO
C
C*      Call the plotting program.
C
        CALL GG_WPLT(filtyp, timstr, timstp, dattim, dattm4, timisu,
     +                 wtype, mrktyp, ssize, iwidth, icolor, icolr2,
     +                 itest, icancl, iflags, num, ip, wstn, npt,
     +                 nfips, cnnam, rlat, rlon,
     +                 ier )

C*
        RETURN
        END
