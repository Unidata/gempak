	SUBROUTINE GG_WCUR ( date, iuwtch, iunum, iret )
C************************************************************************
C* GG_WCUR								*
C*									*
C* This subroutine returns the current thunderstorm and tornado watch   *
C* number information.							*
C*									*
C* GG_WCUR ( DATE, IUWTCH, IUNUM, IRET )				*
C*									*
C* Input parameters:							*
C*	DATE		CHAR*		Ending time for watches		*
C*									*
C* Output parameters:							*
C*	IUWTCH (IUNUM)	INTEGER		Unique watch numbers		*
C*	IUNUM		INTEGER		Number of Unique watchs 	*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 6/01	Extracted from GG_WTCH			*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* D. Kidwell/NCEP       5/02   Checked cancel time is before end time  *
C* A. Hardy/NCEP	 5/03   Removed 60 min. sub. from start time    *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ggcmn.cmn'
C*
	CHARACTER*(*)	date
        INTEGER		iuwtch(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*100, tfile*128, tmstr4*20,
     +			tmstp4*20, carr(7)*20, tnum*4, 
     +			temp4*20
C*
	CHARACTER	stime*20, flstrt*160
	INTEGER		itarr(5), jtarr(5), itype 
	LOGICAL		done, found, finis, rnew, PTDIFF
C*
	PTDIFF (a, b)	= ( ABS ( a - b ) .gt. .005 )
C-----------------------------------------------------------------------
	iret = 0
        dattim = date
C
C*	Scan the directory for all of the watch data files.
C
	filnam = 'WTCH'
	path   = ' '
	templ  = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
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
	nwtch = 0
	wnum ( 1 ) = ' '
	done  = .false.
	ifl = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	  IF  ( files(ifl) .gt. filnam )  THEN
	      done = .true.
	    ELSE
	      IF  ( files(ifl) .ge. flstrt )  THEN
		tfile = path(:lenp) // '/' // files(ifl)
		CALL FL_SOPN ( tfile, lunf, ier )
C
		iostat = 0
		DO WHILE  ( iostat .eq. 0 )
		  READ ( lunf, 2, IOSTAT = iostat ) buffer
2		  FORMAT ( A )
		  IF  ( iostat .eq. 0 )  THEN
		    IF  ( buffer(1:1) .eq. '|' )  THEN
			CALL ST_CLST ( buffer, '|', ' ', 7,
     +				       carr, num, ier )
			CALL ST_NUMB ( carr ( 5 ), nmwtch, ier )
			CALL ST_INCH ( nmwtch, tnum, ier )
			carr ( 5 ) = tnum
			CALL ST_NUMB ( carr ( 6 ), jflag, ier )
			icorr = MOD ( jflag, 2 )
			jtest = jflag / 2
			CALL ST_NUMB ( carr ( 7 ), icancl, ier )
			found = .false.
			finis = .false.
			rnew  = .false.
			istat = -1
			icnum = 0
			ii    = 1
			jw    = 0
C
			DO WHILE ( .not. finis ) 
			    IF  ( tnum .eq. wnum ( ii ) ) THEN
C
C*			        If this is a status report, increase
C*			        the number of watches and fill the
C*			        data arrays.
C
			        IF ( carr ( 2 ) .eq. 'ST' ) THEN
				    IF ( icorr .eq. 0 ) THEN
C
C*				        Add this status report.
C
				        rnew  = .true.
				        temp4 = timstp (ii)
				        finis = .true.
				      ELSE
C
C*				        This is a correction to a status
C*				        report.  Save the index.
C
					IF ( wtype (ii) .eq. 'ST' ) THEN
				            istat = ii
					  ELSE
					    istat = MAX ( istat, 0 )
					    temp4 = timstp ( ii )
					END IF
				    END IF
				  ELSE
C
C*				    Otherwise, if the watch number
C*				    was found in the array and this is a
C*				    cancellation, only update the end
C*				    time for the watch.  The cancel time
C*				    must be earlier than the end time.
C
				    IF ( icancl .eq. 1 ) THEN
					CALL TI_DIFF ( timstp ( ii ),
     +					     carr ( 4 ), nmin, ier )
					IF ( nmin .gt. 0 ) THEN
      				            timstp ( ii ) = carr ( 4 )
C
C*					  Check for correction to cancl.
C
					  ELSE IF ( icorr .eq. 1 ) THEN
					    CALL TI_DIFF ( carr ( 4 ),
     +						 timstr(ii), nmin, ier )
					    IF ( ( nmin .gt. 0 ) .and.
     +						 ( nmin .le. 390 ) )
     +				                 timstp (ii) = carr (4)
					END IF
				      ELSE
C
C*				        Check for a correction.
C
				        IF ( ( icorr .eq. 1 ) .and.
     +					     ( wtype (ii) .ne. 'ST' ) )
     +					     THEN
					    jw = ii
			  		    wtype (jw)  = carr (2)
			  		    timstr (jw) = carr (3)
			  		    timstp (jw) = carr (4)
			  		    npt (jw)    = 0
					    itest (jw)  = jtest
				        END IF
				    END IF
			        END IF
			        found = .true.
			    END IF
			    ii = ii + 1	
			    IF ( ii .gt. nwtch ) finis = .true.
			END DO
C
      			IF ( carr ( 2 ) .ne. 'ST' ) THEN
			    IF ( .not. found ) THEN
C
C*			        Add new watch to the data arrays.
C
			        rnew  = .true.
			        temp4 = carr ( 4 )
			    END IF
			    IF ( icorr .eq. 1 ) THEN
C
C*				See if this may actually be a correction
C*				to a watch number.
C
				DO ii = 1, nwtch
				    IF ( (carr(2) .eq. wtype (ii)) .and.
     +					 (carr(3) .eq. timstr(ii)) .and.
     +					 (carr(4) .eq. timstp(ii)) )
     +					 icnum = ii
				END DO
			    END IF
			  ELSE IF ( istat .gt. 0 ) THEN
C
C*			    Check the time for corrected status report.
C
			    CALL TI_DIFF ( carr ( 3 ), timstr ( istat ), 
     +					   nmin, ier )
			    IF ( ( nmin .ge. 0  ) .and.
     +				 ( nmin .le. 30 ) ) THEN
			        jw = istat
			        timstr ( jw ) = carr ( 3 )
			      ELSE
C
C*			        The time could not be matched, so treat
C*			        this as a new status report.
C
			        rnew  = .true.
				temp4 = timstp ( istat )
			    END IF
			  ELSE IF ( istat .eq. 0 ) THEN
			    rnew = .true.
		        END IF
C
			IF ( rnew ) THEN
C
C*			    Add a new watch or status report.
C			  
			    nwtch = nwtch + 1
			    jw    = nwtch
			    wtype (jw)  = carr (2)
			    timstr (jw) = carr (3)
			    timstp (jw) = temp4
			    wnum (jw)   = carr (5)
			    npt (jw)    = 0
			    itest (jw)  = jtest
			END IF
	  	    END IF
C
C*		    Read the lat/lon coordinates from the file.
C
		    knt    = 0
		    jostat = 0
		    DO WHILE  ( jostat .eq. 0 )
			READ ( lunf, 2, IOSTAT = jostat ) buffer
			IF  ( jostat .eq. 0 )  THEN
			    IF  ( buffer(1:1) .eq. '|' )  THEN
			        CALL FL_BKSP ( lunf, ier )
		       	        jostat = -1
			      ELSE
			        IF  ( jw .ne. 0 )  THEN
				    knt = knt + 1
				    READ (buffer, 1000)  rlat(knt,jw),
     +					        	 rlon(knt,jw)
1000				    FORMAT ( 2F9.2 )
			        END IF
			    END IF
			END IF
		    END DO
C
C*		    If this is not a status report, close the line.
C
		    IF  ( carr(2) .eq. 'ST' )  THEN
			IF  ( found )  npt(jw) = knt
		      ELSE
		        IF  ( knt .ne. 0 )  THEN
			    npt(jw) = knt + 1
			    rlat(npt(jw),jw) = rlat(1,jw)
			    rlon(npt(jw),jw) = rlon(1,jw)
			END IF
	            END IF
C
		    IF ( icnum .gt. 0 ) THEN
C
C*		        This report may be a watch number correction.
C*			Assume this is so only if all points are the
C*			same for both reports.
C
			IF ( npt ( jw ) .eq. npt ( icnum ) ) THEN
			    found = .true.
			    DO ii = 1, npt ( jw )
 			        IF ( ( PTDIFF ( rlat (ii,jw), 
     +						rlat (ii,icnum) ) ) .or.
     +				     ( PTDIFF ( rlon (ii,jw), 
     +					        rlon (ii,icnum) ) ) )
     +				   found = .false.
			    END DO 
			    IF ( found ) THEN
				IF ( rnew )  nwtch = nwtch - 1
				wnum ( icnum ) = wnum ( jw )
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
        ij = 1
        iunum = 0
	DO ii = 1, nwtch
C
           CALL TI_DTM4 ( timstp ( ii ), tmstp4, ier )
           CALL TI_DTM4 ( timstr ( ii ), tmstr4, ier )
C
            IF ( (dattim .eq. 'ALL' ) .or.
     +          ( ( tmstp4 .gt. dattm4 ) .and.
     +            ( tmstr4 .le. dattm4 ) ) ) THEN
               wnum(ij) = wnum(ii)
               timstr(ij) = tmstr4
               timstp(ij) = tmstp4
               wtype(ij) =  wtype(ii)
               npt(ij) =  npt(ii)
               itest(ij) =  itest(ii)
C
               DO jj = 1, npt(ii)
                   rlat(jj,ij) = rlat(jj,ii)
                   rlon(jj,ij) = rlon(jj,ii)
               END DO
C
C*	       Find unique watch numbers.
C
               IF ( ( wtype (ii) .eq. 'TS') .or.
     +              ( wtype (ii) .eq. 'TN') ) THEN
                   iunum = iunum + 1
                   CALL ST_NUMB (wnum (ii), iwnm, ier )
                   iuwtch ( iunum ) = iwnm 
               END IF
               ij = ij + 1
            END IF
        END DO
        nwtch = ij - 1 
C*
	RETURN
	END
