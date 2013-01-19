	SUBROUTINE GG_NCON ( dattim, icolor, ssize, iwidth, lwidth,
     +			     iflags, iret )
C************************************************************************
C* GG_NCON								*
C*									*
C* This subroutine plots the current non-convective sigmets for the     *
C* lower 48 states and the adjacent coastal waters.                     *
C*									*
C* GG_NCON ( DATTIM, ICOLOR, SSIZE, IWIDTH, LWIDTH, IFLAGS, IRET )	*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Ending time for non_conv sigmet *
C*	ICOLOR (4)	INTEGER		Line and symbol colors		*
C*	SSIZE  (4)	REAL		Symbol sizes			*
C*	IWIDTH (4)	INTEGER		Symbol widths			*
C*	LWIDTH (4)	INTEGER		Line widths			*
C*	IFLAGS (14)	INTEGER		Flags for labels & data filter	*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/00	                                        *
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* S. Jacobs/NCEP	 4/01	Changed flstrt check from gt to ge	*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* S. Jacobs/NCEP	 4/03	Changed the icing sym from mod to svr	*
C* M. Li/SAIC		 5/03	Added data filter                       *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NW = 500 )
C
	PARAMETER	( JICE = 1, JTRB = 2, JDUS = 3, JVOA = 4 )
	PARAMETER	( JSYM = 1, JTIM = 2, JMID = 3, JFLT = 4 )
	PARAMETER	( JNAM = 14 )
C*
	CHARACTER*(*)	dattim
	INTEGER		icolor(*), iwidth(*), lwidth(*), iflags(*)
	REAL		ssize(*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +			buffer*100, tfile*128, label*4, dattm4*20, 
     +			tmstp4*20, tmstr4*20, carr (8)*20, tid*20, 
     +			ttype*2 
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
	CHARACTER	type (NW)*2, timstr (NW)*20, timstp (NW)*20,
     +			msgid (NW)*20, flevel (2,NW)*4, name(10)*20
	INTEGER		npt (NW), itest (NW)
	REAL		rlat (20,NW), rlon (20,NW), tlat (20), tlon (20)
C*
	CHARACTER	stime*20, flstrt*160
	INTEGER		itarr (5), jtarr (5), itype
	LOGICAL		done, t1, t2, t3
C*
	DATA	name	/'NOVEMBER', 'OSCAR', 'PAPA', 'QUEBEC', 
     +			 'ROMEO', 'UNIFORM', 'VICTOR', 'WHISKEY', 
     +			 'XRAY', 'YANKEE'/ 
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan the directory for all of the non-convective sigmet data 
C*      files.
C
	filnam = 'NCON'
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
		RETURN
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
	nncon = 0
	done  = .false.
	ifl   = 1
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	    IF  ( files (ifl) .gt. filnam )  THEN
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
			  CALL ST_CLST ( buffer, '|', ' ', 8, carr,
     +					 num, ier )
			  CALL ST_NUMB ( carr ( 8 ), jflag, ier )
			  jcorr  = MOD ( jflag, 3 )
			  jtest  = jflag / 3
			  ttype  = carr ( 2 )
			  tid    = carr ( 5 )
			  jcancl = 0
			  IF ( ttype .eq. 'CN' ) THEN
C
C*			      This is an cancellation.  Look for a
C*			      matching name and change the end time to
C*			      the cancellation time.
C
			      DO ii = 1, nncon
				IF ( tid .eq. msgid (ii) ) THEN
				    timstp ( ii ) = carr ( 3 )
				END IF
				jcancl = 1
			      END DO
			
			    ELSE IF ( jcorr .gt. 0 ) THEN
C
C*			      This is an amendment or correction.
C*			      Look for a matching name and a match on
C*			      any two of the following: start time, 
C*			      end time, and type.  (An attempt to change
C*			      a name will not be recognized.)  If a 
C*			      match is found, change the end time to the
C			      correction time.
C
			      DO ii = 1, nncon
				IF ( tid .eq. msgid (ii) ) THEN
				    t1 = ( ttype .eq. type ( ii ) ) 
				    t2 = ( carr (3) .eq. timstr ( ii ) )
				    t3 = ( carr (4) .eq. timstp ( ii ) )
				    IF ( ( t1 .and. t2 ) .or.
     +					 ( t1 .and. t3 ) .or.
     +					 ( t2 .and. t3 ) ) THEN
					timstp ( ii ) = carr ( 3 )
				    END IF
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
C
C*			  Add this report if it is not a cancellation.
C
			  IF ( jcancl .eq. 0 ) THEN
			    nncon               = nncon + 1    
			    type ( nncon )      = ttype
			    timstr ( nncon )    = carr ( 3 )
			    timstp ( nncon )    = carr ( 4 )
			    msgid ( nncon )     = tid
			    flevel ( 1, nncon ) = carr ( 6 )
			    flevel ( 2, nncon ) = carr ( 7 ) 
			    itest ( nncon )     = jtest  		 
			    DO kk = 1, knt
				rlat ( kk, nncon ) = tlat ( kk )
				rlon ( kk, nncon ) = tlon ( kk )
			    END DO
			    npt ( nncon ) = knt
			  END IF
		      END IF
		    END IF
		  END DO
C
		  CALL FL_CLOS ( lunf, ier )
C
		END IF
	    END IF
	    ifl = ifl + 1
	END DO
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GQTURB ( szturb, jtuwid, ier )
	CALL GQICNG ( szicng, jicwid, ier )
	CALL GQWTHR ( szwthr, jwtwid, ier )
C
C*	Set attributes.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  i = 1, 4
	    IF  ( ( ssize (i) .le.  0.0 ) .or.
     +		  ( ssize (i) .gt. 10.0 ) ) THEN
		ssize (i) = 1.5
	    END IF
C
	    IF  ( ( iwidth (i) .le.  0 ) .or.
     +		  ( iwidth (i) .gt. 10 ) ) THEN
		iwidth (i) = 2
	    END IF
C
	    IF  ( ( lwidth (i) .le.  0 ) .or.
     +		  ( lwidth (i) .gt. 10 ) )  lwidth (i) = 2
	END DO
C
	sytic = 8.
	syttb = 4.
	sytdu = 31.
	sytva = 201.
C
C*	Plot the graphic for each valid report.
C
	DO ip = 1, nncon
	    CALL TI_DTM4 ( timstp ( ip ), tmstp4, ier )
	    CALL TI_DTM4 ( timstr ( ip ), tmstr4, ier )
	    IF  ( ( dattim .eq. 'ALL' ) .or.
     +		  ( ( tmstp4 .gt. dattm4 ) .and.
     +		    ( tmstr4 .le. dattm4 ) ) )  THEN
C
C*		Set the line type for a test or an actual report.
C
		IF ( itest ( ip ) .eq. 0 ) THEN
		    iltyp = 1
		  ELSE
		    iltyp = 2
		END IF
C
C*		Set the color and symbol based on the phenomenon type.
C
		IF  ( type ( ip ) .eq. 'IC' )  THEN
		    ic = icolor (JICE)
		    CALL GSICNG ( ssize (JICE), iwidth (JICE), ier )
		    CALL GSLINE ( iltyp, 0, lwidth (JICE), 0, ier )
C
		  ELSE IF  ( type ( ip ) .eq. 'TB' )  THEN
		    ic = icolor (JTRB)
		    CALL GSTURB ( ssize (JTRB), iwidth (JTRB), ier )
		    CALL GSLINE ( iltyp, 0, lwidth (JTRB), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'DU' ) THEN
		    ic = icolor (JDUS)
		    CALL GSWTHR ( ssize (JDUS), iwidth (JDUS), ier )
		    CALL GSLINE ( iltyp, 0, lwidth (JDUS), 0, ier )
C
		  ELSE IF ( type ( ip ) .eq. 'VA' ) THEN
		    ic = icolor (JVOA)
		    CALL GSWTHR ( ssize (JVOA), iwidth (JVOA), ier )
		    CALL GSLINE ( iltyp, 0, lwidth (JVOA), 0, ier )
		END IF
C
C*		Filter data by message ID
C
		DO jj = JFLT+1, JNAM
		    IF ( iflags(jj) .le. 0 ) THEN 
			CALL ST_LSTR ( name(jj-JFLT), lenn, ier )
			ipos = INDEX ( msgid(ip), name(jj-JFLT)(:lenn) )
			IF ( ipos .gt. 0 ) ic = 0
		    END IF
		END DO
C
C*		Draw the shape for a bounded phenomenon and get the
C*		label location.
C
		IF ( ( ic .ne. 0 ) .and. ( npt ( ip ) .ne. 0 ) ) THEN
		    npts = npt ( ip )
		    CALL GSCOLR ( ic, ier )
		    CALL GLINE ( 'M', npts , rlat ( 1, ip ),
     +			         rlon ( 1, ip ), ier )
C
		    IF ( ( iflags ( JSYM ) .ne. 0 ) .or.
     +			 ( iflags ( JFLT ) .ne. 0 ) .or.
     +			 ( iflags ( JMID ) .ne. 0 ) .or.
     +			 ( iflags ( JTIM ) .ne. 0 ) ) THEN
		        iyoff = -2
		        CALL GG_WLBL ( npts, rlat ( 1, ip ),
     +				       rlon ( 1, ip ), alat, alon, ier )
C
			IF ( iflags ( JMID ) .ne. 0 ) THEN
C
C*			  Plot the message id.
C
		          CALL GTEXT ( 'M', alat, alon, msgid ( ip),
     +				       0.0, 0, iyoff, ier )
			  iyoff = -4
			END IF
			  
		        IF ( iflags ( JSYM ) .ne. 0 ) THEN
C
C*		          Plot the symbol.
C
			  IF ( iflags ( JFLT ) .gt. 0 ) THEN
      			      iyoff = iyoff - 2
			    ELSE
			      iyoff = iyoff - 1
			  END IF
			  ixoff = 0
		          IF ( type ( ip ) .eq. 'IC' )  THEN
	                    CALL GICNG ( 'M', 1, sytic, alat, alon,
     +				         ixoff, iyoff, ier )
		           ELSE IF ( type ( ip ) .eq. 'TB' )  THEN
		            CALL GTURB ( 'M', 1, syttb, alat, alon,
     +				         ixoff, iyoff, ier )
		           ELSE IF ( type ( ip ) .eq. 'DU' )  THEN
			    CALL GWTHR ( 'M', 1, sytdu, alat, alon,
     +					 ixoff, iyoff, ier )
		           ELSE IF ( type ( ip ) .eq. 'VA' )  THEN
			    CALL GWTHR ( 'M', 1, sytva, alat, alon,
     +					 ixoff, iyoff, ier )
		          END IF
		        END IF
			IF ( ( iflags ( JSYM ) .ne. 0 ) .or.
     +			     ( iflags ( JFLT ) .ne. 0 ) ) 
     +			     iyoff = iyoff - 2
C
		        IF ( iflags ( JFLT ) .ne. 0 ) THEN
C
C*			    Plot the flight level(s).
C
			    ixoff = 4
			    IF ( iflags ( JSYM ) .eq. 0 ) 
     +				 iyoff = iyoff - 2
			    DO ii = 1, 2
		                label  = flevel ( ii, ip )
		                CALL GTEXT ( 'M', alat, alon, label, 0.,
     +				             ixoff, iyoff, ier )
				iyoff = iyoff + 4
			    END DO
			    iyoff = iyoff - 10
			  ELSE
			    iyoff = iyoff - 1
		        END IF
C
		        IF ( iflags ( JTIM ) .ne. 0 ) THEN
C
C*		          Plot the time.
C
		          label  = timstp ( ip ) ( 8:11 )
		          CALL GTEXT ( 'M', alat, alon, label, 0.0, 0, 
     +					iyoff, ier )
			END IF
		    END IF
		END IF
	    END IF
	END DO
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GSTURB ( szturb, jtuwid, ier )
	CALL GSICNG ( szicng, jicwid, ier )
	CALL GSWTHR ( szwthr, jwtwid, ier )
C*
	RETURN
	END
