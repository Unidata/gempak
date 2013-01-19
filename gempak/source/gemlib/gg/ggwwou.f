	SUBROUTINE GG_WWOU ( filtyp, dattim, num, icolor, icolr2, 
     +			      mrktyp, ssize, iwidth, iflags, iret )
C************************************************************************
C* GG_WWOU								*
C*									*
C* This subroutine reads the current weather watch outline update (WOU) *
C* for each watch number.						*
C*									*
C* GG_WWOU ( FILTYP, DATTIM, NUM, ICOLOR, ICOLR2, MRKTYP, SSIZE, IWIDTH,*
C*           IFLAGS, IRET ) 						*
C*									*
C* Input parameters:							*
C*      FILTYP          CHAR*           File type 'WOU'			*
C*	DATTIM		CHAR*		Ending time for warning or watch*
C*	NUM		INTEGER		Number of markers		*
C*      ICOLOR (NUM)    INTEGER         Marker and outline colors       *
C*      ICOLR2 (NUM)    INTEGER         Fill colors                     *
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
C* M. Li/SAIC		02/03						*
C* A. Hardy/NCEP	 3/03	Reworked file & report selection	*
C* A. Hardy/NCEP	 7/03	Added 'mndif' to GG_WWCR		*
C* A. Hardy/NCEP	 1/04	Added tmstp to GG_WWCR call		*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM     	*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* M. Li/SAIC           01/05   Added new input parameter ICOLR2        *
C* A. Hardy/NCEP	 3/05	Added check for next line beginning '|'	*
C* G. Grosshans/SPC	 4/05	Updated to change WOU plotting order	*
C* F. J. Yen/NCEP	 5/05	Improved performance--call GG_WPLT once	*
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 1000, NC = 500 )
C*
	CHARACTER*(*)	filtyp, dattim
	INTEGER		icolor(*), mrktyp(*), iwidth(*), iflags(*),
     +			icolr2(*)
	REAL		ssize(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*156, tfile*128, dattm4*20, carr(12)*20, 
     +			ttype*3, tstrt*20, tstn*7
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL), newfil
C*
	CHARACTER	wstn(NW)*7, timstr(NW)*20, timstp(NW)*20,
     +			wtype(NW)*3, timisu(NW)*20
	INTEGER		itest(NW), npt(NW), icancl(NW), itype 
	REAL		rlat(NC,NW), rlon(NC,NW)
	CHARACTER	cnnam(NC,NW)*32, fipnam(NC,NW)*8, 
     +			st*2, cn*2, wfo*20, fnull*(MXFLSZ) 
C*
	CHARACTER	stime*20, flstrt*160, cancl*3, timiss(NW)*20,
     +                  tmstrt(NW)*20, tzone(NW)*20, alias*3, 
     +                  flend*160, fname*160, tmstp(NW)*20
	INTEGER		itarr(5), jtarr(5), nfips(NC,NW), iuwtch(NW)
	LOGICAL		done, found, gdtim, exist
C*	
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan the WOU directory for all the WOU data files.
C
	CALL ST_NULL ( filtyp, fnull, nf, ier )
	path  = ' '
	templ = ' '
	CALL CTB_DTGET ( fnull, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( templ, templ, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )
        nexp   = MXNMFL
	iorder = -1
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
C*	12 hours from the time given.
C
	IF  ( dattim .eq. 'ALL' )  THEN
	    minuts = 14400
	  ELSE
	    minuts = 720
	END IF
C
C*      Find end file name
C
        CALL TI_CTOI ( dattm2, itarr, ier )
        CALL TI_ITOC ( itarr, stime, ier )
        CALL FL_MNAM ( stime, templ, flend, ier )
        CALL ST_LSTR ( flend, len, ier )
C
C*      Find start file name
C
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
	CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*	Get 4-digit year to compare dates.
C
	CALL TI_DTM4 ( dattm2, dattm4, ier )
C
C*	Check the total number of WOUs in the files.
C
	numw = 0
	done = .false.
	ifl  = nfile
	DO WHILE  ( ( ifl .gt. 0 ) .and. ( .not. done ) )
	    IF  ( files(ifl) .ge. flstrt )  THEN
	    	tfile = path(:lenp) // '/' // files(ifl)
		CALL FL_SOPN ( tfile, lunf, ier )
		iostat = 0
		DO WHILE  ( ( iostat .eq. 0 ) .and. ( .not. done ) )
		    READ ( lunf, 2, IOSTAT = iostat ) buffer
		    IF  ( iostat .eq. 0 )  THEN
		    	IF  ( buffer(1:1) .eq. '|' )  THEN
			    numw = numw + 1
			    IF  ( numw .gt. NW )  THEN
			    	done = .true.
				CALL ER_WMSG ( 'GG', 3, ' ', ierr )
			    END IF
			END IF
		    END IF
		END DO
		CALL FL_CLOS ( lunf, ier )
	      ELSE
	        done = .true.
	    END IF
	    ifl = ifl - 1
	END DO
C
	IF  ( ifl+1 .le. nfile )  THEN
	    flstrt = files (ifl+1)
	END IF
C
C*      Get the number of watches, watch numbers, issue and start times.
C
        mndif = 0
        alias = 'WOU'
        CALL GG_WWCR  ( dattim, alias, mndif, iuwtch, iunum, tzone,
     +			timiss, tmstrt, tmstp, ier ) 
C
C*	Decode each file until the end time is reached.
C*	Plot order is changed to display the most recent WOU on top.
C
	nwtch = 0
	done  = .false.
        DO ij = iunum, 1, -1 
            exist = .false.
	    CALL FL_MNAM ( timiss(ij), templ, fname, ier )
	    tfile = path(:lenp) // '/' // fname
            CALL  FL_INQR  ( tfile , exist, newfil, ier )
	    CALL FL_SOPN ( tfile, lunf, ier )
C
	    iostat = 0
	    DO WHILE  ( ( iostat .eq. 0 ) .and. ( exist ) )
                gdtim = .false.
		READ ( lunf, 2, IOSTAT = iostat ) buffer
2		FORMAT ( A )
		IF  ( iostat .eq. 0 ) THEN
		    IF ( buffer(1:1) .eq. '|' ) THEN
C
C*                      Check time differences and watch number.
C
                        CALL TI_DIFF ( buffer(6:16),timiss(ij),
     +                               nmis, ier )
                        CALL TI_DIFF ( buffer(18:28),tmstrt(ij),
     +                               nmst, ier )
                        CALL ST_NUMB ( buffer(42:45), iwnm, ier)
C
                        IF ( (nmis .eq. 0 ) .and. ( nmst .eq. 0 ) .and.
     +                        (iuwtch(ij) .eq. iwnm ) ) THEN
C
			    CALL ST_CLST ( buffer, '|', ' ', 12,
     +					   carr, ignore, ier )
C
			    CALL ST_NUMB ( carr(10), jflag, ier )
			    jcorr = MOD ( jflag, 2 )
			    jtest = jflag / 2
			    ttype = carr(2)
			    tstrt = carr(4)
			    tstn  = carr(6)
			    found = .false.
			    IF  ( jcorr .eq. 1 )  THEN
C 
C*				Allow for start time difference of 15
C*				minutes for warnings and 60 minutes
C*				for SLS watches for matching of 
C*				correction reports.
C
				IF ( INDEX ( tstn, ',' ) .eq. 0 ) THEN
				    minup = 15
				  ELSE
				    minup = 60
				END IF
				DO  ii = 1, nwtch
				    CALL TI_DIFF ( tstrt, timstr(ii),
     +					           mindif, iret)	
				    IF  ((wtype(ii) .eq. ttype) .and.
     +					 (wstn(ii)  .eq. tstn ) .and.
     +					 (mindif    .ge. 0)     .and.
     +					 (mindif    .le. minup)) THEN 
					found = .true.
					iw = ii
				    END IF
				END DO
                            END IF
C
			    IF  ( found )  THEN
				jw = iw
			      ELSE
				nwtch = nwtch + 1
				jw = nwtch
			    END IF
C
			    wtype(jw)  = carr(2)
			    timisu(jw) = carr(3)
			    timstr(jw) = carr(4)
			    timstp(jw) = carr(5)
			    wstn(jw)   = carr(6)
			    itest(jw)  = jtest
			    cancl = carr(11)
			    CALL ST_NUMB ( carr(11), icancl(jw), ier ) 
C
			    knt = 0
			    jostat = 0
			    DO WHILE  ( jostat .eq. 0 )
			        READ ( lunf, 2, IOSTAT = jostat ) buffer
			        IF  ( jostat .eq. 0 )  THEN
			    	    IF  ( buffer(1:1) .eq. '|' )  THEN
				        CALL FL_BKSP ( lunf, ier )
				        jostat = -1
				      ELSE 
                                        if ( (nmis .eq. 0 ) .and. 
     +					         ( nmst .eq. 0 ) )THEN
 				            knt = knt + 1
				            IF ( knt .le. NC ) THEN
				                READ (buffer, 1000) 
     +					      fipnam(knt,jw),
     +					      nfips(knt,jw), 
     +					      cnnam(knt,jw),
     +					      st, cn, rlat(knt,jw),
     +					      rlon(knt,jw), elv,
     +					      ipr, wfo
1000				            FORMAT ( A, 1X, I6, 1X, A, 
     +                                        1X, A, 1X, A, 1X, F9.2, 
     +                                        1X, F9.2, 1X, F9.2, 1X,
     +                                        I2, 1X, A )
				            END IF
			                end if
				    END IF
			        END IF
			    END DO
		            npt(jw) = knt
                        ELSE
C
C*                          Issue time and watch time dont match;
C*                          skip lines till next '|'. Check if next line
C*			    is a '|'.
C
                            jostat = 0
                            READ ( lunf, 2, IOSTAT = jostat ) buffer
			    IF  ( buffer(1:1) .eq. '|' )  THEN
			        CALL FL_BKSP ( lunf, ier )
                              ELSE
			       DO WHILE  ( jostat .eq. 0 )
                                  READ ( lunf, 2, IOSTAT = jostat)buffer
			           IF  ( jostat .eq. 0 )  THEN
			    	       IF  ( buffer(1:1) .eq. '|' )THEN
				           CALL FL_BKSP ( lunf, ier )
				           jostat = -1
                                       END IF
                                   END IF
                               END DO
                            END IF
                        END IF
		    END IF
		END IF
	    END DO
	    CALL FL_CLOS ( lunf, ier )
	END DO
C
C*      Call the plotting program.
C
        IF (nwtch.gt.0) THEN
            CALL GG_WPLT(filtyp, timstr, timstp, dattim, 
     +              dattm4, timisu, wtype, mrktyp, ssize, iwidth, 
     +	            icolor, icolr2, itest, icancl, iflags, num, 
     +	            nwtch, wstn, npt,  nfips, cnnam, rlat, rlon, ier )
	END IF
C*
	RETURN
	END
