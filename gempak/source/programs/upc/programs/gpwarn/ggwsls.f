	SUBROUTINE GGWSLS ( filtyp, dattim, num, icolor, mrktyp, ssize,
     +			     iwidth, iflags, iret )
C************************************************************************
C* GGWWSLS								*
C*									*
C* This subroutine plots the current thunderstorm, tornado and flash	*
C* flood warnings.  It also plots the current SLS thunderstorm and	*
C* tornado watches.							*
C*									*
C* GGWSLS ( FILTYP, DATTIM, NUM, ICOLOR, MRKTYP, SSIZE, IWIDTH,	*
C*           IFLAGS, IRET )						*
C*									*
C* Input parameters:							*
C*	FILTYP		CHAR*		File type 'WARN' or 'SVRL'	*
C*	DATTIM		CHAR*		Ending time for warning or watch*
C*	NUM		INTEGER		Number of markers		*
C*	ICOLOR (NUM)	INTEGER		Marker symbol colors		*
C*	MRKTYP (NUM)	INTEGER		Marker symbol types		*
C*	SSIZE  (NUM)	REAL		Marker symbol sizes		*
C*	IWIDTH (NUM)	INTEGER		Marker symbol widths		*
C*	IFLAGS (2)	INTEGER		Flags for labels		*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/99	Copied from GG_WTCH			*
C* S. Jacobs/NCEP	 5/99	Increased file name array size to 500	*
C* A. Hardy/GSC		 6/99	Added label flag and marker type        *
C* S. Jacobs/NCEP	 6/99	Increased array sizes, NW, to 5000	*
C* S. Jacobs/NCEP	 8/99	Changed call to FL_SCND			*
C* S. Jacobs/NCEP	 8/99	Changed call to FL_TMPL			*
C* S. Jacobs/NCEP	11/99	Increased buffer from 100 to 128	*
C* S. Jacobs/NCEP	 3/00	Changed calling sequence		*
C* D. Kidwell/NCEP	 4/00	Added processing for TEST warnings      *
C* M. Li/GSC		 5/00	Added MXFLSZ and MXNMFL			*
C* J. Wu/GSC             7/00   Added checks for TI_STAN's return status*
C* F. J. Yen/NCEP	 1/01	Changed to handle SLS watches		*
C* F. J. Yen/NCEP	 1/01	Increased NC;called ER_WMSG when over NC*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* F. J. Yen/NCEP	 4/01	Fixed bug to handle corrections		*
C* S. Jacobs/NCEP	 4/01	Changed flstrt check from gt to ge	*
C* S. Jacobs/NCEP	 8/01	Added check for total num warn vs. NW	*
C* S. Jacobs/NCEP	 8/01	Changed NW to 1000 and NC to 100	*
C* A. Hardy/SAIC	12/01   Added check for missing counties	*
C* R. Tian/SAIC		02/02	Added new argument num			*
C* T. Piper/SAIC	02/02	Fixed #534P1.  num was already used	*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* S. Chiswell/Unidata	 9/02	Copied from GG_WARN			*
C* James/Unidata	 2/09   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 1000, NC = 100 )
C
	PARAMETER	( JSVR = 1, JTOR = 2, JFFW = 3 )
	PARAMETER	( JTIM = 1, JLAB = 2 )
C
C*	must be sync'd with gpwarn.h
	PARAMETER	( SLSTSTM = 9, SLSTORN = 10, SLSFFW = 11 )
C*
	CHARACTER*(*)	filtyp, dattim
	INTEGER		icolor(*), mrktyp(*), iwidth(*), iflags(*)
	REAL		ssize(*)
C*
	CHARACTER	path*25, templ*48, cdttm*20, dattm2*20, 
     +			buffer*128, tfile*128, wlabel*80, dattm4*20, 
     +			tmstp4*20, tmstr4*20, carr(6)*20, ttype*3, 
     +			tstrt*20, tstn*7
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
	CHARACTER	wstn(NW)*7, timstr(NW)*20, timstp(NW)*20,
     +			wtype(NW)*3
	INTEGER		itest(NW), npt(NW), nfips(NC,NW) 
	REAL		rlat(NC,NW), rlon(NC,NW)
	CHARACTER	cnnam(NC,NW)*32, fipnam*8, st*2, cn*2, wfo*20
C*
	CHARACTER	stime*20, flstrt*160, ctmpl*(LLMXLN)
	INTEGER		itarr(5), jtarr(5), mrktst(17:21)
	LOGICAL		done, tplflg, found
C*	
	DATA		mrktst / 2, 3, 4, 6, 14 /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan the directory for all of the warning & SLS watch data files.
C
	CALL ST_NULL ( filtyp, ctmpl, lens, ier )
	path  = ' '
        templ = ' '
        CALL CTB_DTGET ( ctmpl, path, templ, ic, is, if, ir, ii, ion, 
     +			ihb, mnb, iha, mna, mstruct, idtmch, ier )

	CALL ST_RNUL ( templ, templ, lens, ier )
	CALL ST_RNUL ( path, path, lenp, ier )
C
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
C*	12 hours from the time given.
C
	IF  ( dattim .eq. 'ALL' )  THEN
	    minuts = 14400
	  ELSE
	    minuts = 720
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
C*	Check the total number of warnings in the files.
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
C*	Decode each file until the end time is reached.
C
	nwrn = 0
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
2		    FORMAT ( A )
		    IF  ( iostat .eq. 0 )  THEN
			IF  ( buffer(1:1) .eq. '|' )  THEN
			    CALL ST_CLST ( buffer, '|', ' ', 6,
     +					   carr, ignore, ier )
C
			    CALL ST_NUMB ( carr(6), jflag, ier )
			    jcorr = MOD ( jflag, 2 )
			    jtest = jflag / 2
			    ttype = carr(2)
			    tstrt = carr(3)
			    tstn  = carr(5)
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
				DO  ii = 1, nwrn
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
				nwrn = nwrn + 1
				jw = nwrn
			    END IF
C
			    wtype(jw)  = carr(2)
			    timstr(jw) = carr(3)
			    timstp(jw) = carr(4)
			    wstn(jw)   = carr(5)
			    itest(jw)  = jtest
			END IF
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
				    knt = knt + 1
				    IF ( knt .le. NC ) THEN
				        READ (buffer, 1000) fipnam,
     +					      nfips(knt,jw), 
     +					      cnnam(knt,jw),
     +					      st, cn, rlat(knt,jw),
     +					      rlon(knt,jw), elv,
     +					      ipr, wfo
1000				        FORMAT ( A, 1X, I6, 1X, A, 1X,
     +                                        A, 1X, A, 1X, F9.2, 1X,
     +                                        F9.2, 1X, F9.2, 1X, I2,
     +                                        1X, A )
				    END IF
				END IF
			    END IF
			END DO
		        npt(jw) = knt
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
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
C*	Set attributes and defaults.
C
c	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  i = 1, num
	    IF  ( ( mrktyp(i) .lt. 17 ) .or.
     +		  ( mrktyp(i) .gt. 21 ) ) THEN
		IF ( filtyp .eq. 'WARN' ) THEN
		    mrktyp(i) = 20
		  ELSE
		    mrktyp(i) = 18
		END IF
	    END IF
C
	    IF  ( ( ssize(i) .le.  0.0 ) .or.
     +		  ( ssize(i) .gt. 10.0 ) )  ssize(i) = 1.0
C
	    IF  ( ( iwidth(i) .le.  0 ) .or.
     +		  ( iwidth(i) .gt. 10 ) )  iwidth(i) = 2
	END DO
C
C*	Plot the graphic for each valid report.
C
	DO ip = 1, nwrn
	    CALL TI_DTM4 ( timstp ( ip ), tmstp4, ier )
	    CALL TI_DTM4 ( timstr ( ip ), tmstr4, ier )
	    IF  ( ( dattim .eq. 'ALL' ) .or.
     +		  ( ( tmstp4 .gt. dattm4 ) .and.
     +		    ( tmstr4 .le. dattm4 ) ) )  THEN
C
C*		Set the color and marker based on the type of warning
C*		or SLS watch
C
		IF  ( wtype(ip) .eq. 'TOR' .or.
     +				wtype(ip) .eq. 'TN' )  THEN
		    jtype = JTOR
		    icolv = SLSTORN
		  ELSE IF  ( wtype(ip) .eq. 'SVR' .or.
     +				wtype(ip) .eq. 'TS' )  THEN
		    jtype = JSVR
		    icolv = SLSTSTM
		  ELSE IF  ( wtype(ip) .eq. 'FFW' )  THEN
		    jtype = JFFW
		    icolv = SLSFFW
		  ELSE
		    jtype = 0
		    icolv = -1
		END IF
C
		IF ( jtype .gt. 0 ) THEN
		    ic = icolor(jtype)
		    IF ( itest ( ip ) .eq. 0 ) THEN
		        mrkr = mrktyp ( jtype ) 
		      ELSE
		        mrkr = mrktst ( mrktyp ( jtype ) ) 
		    END IF
		    CALL GSMRKR ( mrkr, 0, ssize(jtype), iwidth(jtype), 
     +			          ier )
		  ELSE
		    ic = 0
		END IF
C
C*		Plot each county in the warnings or SLS watches.
C
		IF  ( ic .ne. 0 )  THEN
		  CALL GSCOLR ( ic, ier )
		  IF ( npt(ip) .gt. NC ) THEN
		      npt(ip) = NC
      		      CALL ER_WMSG ( 'GG', 2, 'GGWARN', ierr )
		  END IF
                  IF ( npt (ip) .gt. 0 ) THEN
		      DO  im = 1, npt(ip)
C
C*		          Draw the marker.
C
			  CALL plot_slsc ( icolv, nfips(im,ip) )
C		          CALL GMARK ( 'M', 1, rlat(im,ip), rlon(im,ip),
C    +				       ier )
C
C*		          Plot the text.
C
		            IF ( ( iflags(JTIM) .eq. 0 ) .and. 
     +			         ( iflags(JLAB) .eq. 0 ) ) THEN
			        iyoff  = -1
			        wlabel = ' '
C
		              ELSE IF  ( ( iflags(JTIM) .eq. 0 ) .and. 
     +				         ( iflags(JLAB) .ne. 0 ) ) THEN
			        iyoff  = -2
			        CALL ST_LSTR ( cnnam (im,ip), lencn, 
     +                                         ier )
			        wlabel = cnnam(im,ip)(:lencn)
C
		              ELSE IF ( ( iflags(JTIM) .ne. 0 ) .and. 
     +				        ( iflags(JLAB) .eq. 0 ) ) THEN
			        iyoff  = -3
			        CALL ST_LSTR ( timstr (ip), lenstr, ier)
			        CALL ST_LSTR ( timstp (ip), lenstp, ier)
			        wlabel = timstr(ip) (8:11) // '-' //
     +				 timstp(ip) (8:11)
C
		              ELSE IF ( ( iflags(JTIM) .ne. 0 ) .and. 
     +				        ( iflags(JLAB) .ne. 0 ) ) THEN
			        iyoff  = -3
			        CALL ST_LSTR ( cnnam (im,ip), lencn, 
     +                                         ier )
			        CALL ST_LSTR ( timstr (ip), lenstr, ier)
			        CALL ST_LSTR ( timstp (ip), lenstp, ier)
			        wlabel = cnnam(im,ip) (:lencn) // CHCR
     +				     // timstr(ip) (8:11) // '-' //
     +				     timstp(ip) (8:11)
C
		            END IF
C
		            CALL GTEXT ( 'M', rlat(im,ip), rlon(im,ip),
     +				          wlabel, 0.0, 0, iyoff, ier )
		        END DO
		    END IF
		END IF
	    END IF
	END DO
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C*
	RETURN
	END
