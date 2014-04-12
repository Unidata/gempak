	SUBROUTINE TF_DCOD ( curtim, gemfil, stntbl, blktbl, prmfil,
     +			     iadstn, maxtim, nhours, cirflg, iret )
C************************************************************************
C* TF_DCOD								*
C*									*
C* This routine decodes TAF bulletins and writes the data to a GEMPAK   *
C* surface forecast file.  See WMO alphanumeric code type FM 51 for     *
C* documentation on the TAF format.			 		*
C*									*
C* TF_DCOD ( CURTIM, GEMFIL, STNTBL, BLKTBL, PRMFIL, IADSTN, MAXTIM,	*
C*	     NHOURS, IRET )					        *
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	BLKTBL		CHAR*		Black-list table		*
C*	PRMFIL		CHAR*		Parameter packing table		*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP     9/02	                                        *
C* D. Kidwell/NCEP     9/02	Retained line feed chars for text string*
C* D. Kidwell/NCEP    10/02	Err chk for TF_DECD; ST_LCUC; add stidnw*
C* D. Kidwell/NCEP     1/03	Added SF_WSTR w/ dattim for each hour   *
C* D. Kidwell/NCEP     5/03	Added parms VWNM, TVWN, LLWS, MOTV;cntry*
C* B. Yin/SAIC         3/04     Changed SS_GTIM to CSS_GTIM             *
C* D. Kidwell/NCEP    10/04	Added parms CTYL, TCTL for CB/TCU       *
C* F. J. Yen/NCEP      7/07	Stored files based on cycle time; added	*
C*				STIM; removed checking of parms multiple*
C*				times; added bbb to errstr.		*
C* L. Hinson/AWC       6/08     Added cirflg parameter to calling list; *
C*                              Revised logic to support circular files *
C*                              with time records validated against     *
C*                              current time via call to TF_VTIM        *
C* L. Lin/NCEP        11/08     Allow gempak file to span available slot*
C*                              up to 30 hour                           *
C* S. Jacobs/NCEP      3/14	Added black-list station table		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, blktbl, prmfil
C*
	CHARACTER	bull*(DCMXBF), rpt*(DCMXBF), bulsav*(DCMXBF),
     +			sysdt*12, dattmp*12, filnam*132, errstr*80,
     +			dattim*15, stid*8, strnew*80, cmdif*8,
     +			parms(MMPARM)*4, cprms(MMPARM)*4, string*10,
     +			bultim*12, bbb*8, wmohdr*8, oristn*8, seqnum*4,
     +			stidnw*8, origtm*25, cntry*2, stidmt (LLSTFL)*8,
     +			bvldtm*15
	INTEGER		istarr (5), iprms (MMPARM), irtarr (5), 
     +			jrtarr (5), jstarr (5), itype, nrtarr(5)
	INTEGER         iscale (MMPARM), iofset (MMPARM), ibits (MMPARM)
  	LOGICAL		more, good, addstn, cirflg, datflg, offtim,
     +			pkflg, vtimflg
	REAL		rdata (48,MMPARM), adata (MMPARM), 
     +			obscmt (LLSTFL)
C*
	CHARACTER	stidb(LLSTFL)*8, stnamb(LLSTFL)*32,
     +			statb(LLSTFL)*2, counb(LLSTFL)*2,
     +			tbchb(LLSTFL)*20, cpos*8
	INTEGER		istnmb(LLSTFL), isprb(LLSTFL)
	REAL		slatb(LLSTFL), slonb(LLSTFL), selvb(LLSTFL)
C*
	PARAMETER	( NUMPRM = 26 )
	PARAMETER	( NUMEXT = MMPARM - NUMPRM )
	DATA		cprms / 'SKNT', 'DRCT', 'GUST', 'WNUM', 
     +				'CHC1', 'CHC2', 'CHC3', 'VSBY',
     +				'TSKN', 'TDRC', 'TGST', 'TWNM',
     +				'TCH1', 'TCH2', 'TCH3', 'TVSB',
     +				'PPRB', 'CEIL', 'TCEL', 'VWNM',
     +				'TVWN', 'LLWS', 'MOTV', 'CTYL',
     +				'TCTL', 'STIM', NUMEXT * ' ' /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Write decoder version number to log.
C
	CALL DC_WLOG ( 2, 'DCTAF', 7, '1.2', ierr )
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file and file source.  Initialize
C*	off-time flag.
C
	maxfil = 3
	iftype = 1
	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
	addstn = .true.
	iflsrc = MFUNKN + MFTEXT 
	offtim = .false.
C
C*	Read the list of stations in the black list. These stations
C*	will not be processed. 
C
	CALL FL_TBOP ( blktbl, 'stns', lunb, ier )
	IF  ( ier .eq. 0 )  THEN
	    CALL TB_ASTN ( lunb, LLSTFL, nblk, stidb, stnamb, istnmb,
     +			statb, counb, slatb, slonb, selvb, isprb,
     +			tbchb, ierb )
	    CALL FL_CLOS ( lunb, ier )
	ELSE
	    nblk = 0
	END IF
C
C*	Get the mountain obscuration values.
C
	CALL TF_MOBV ( stntbl, stidmt, obscmt, nstn, ierr )
C
C*	Read the parameter-packing file
C
	CALL DP_FILE ( prmfil, nparm, parms, iscale,
     +		       iofset, ibits, pkflg, ierr)
C
C*	Check for successful reading of the parm
C*	packing file
C
        IF ( ierr .ne. 0 ) THEN
            CALL DC_WLOG ( 0, 'DP', ierr, ' ', ier )
	  ELSE
C
C*          Check for the parameters in the list.
C
            DO jj = 1, NUMPRM
		CALL ST_FIND ( cprms (jj), parms, nparm,
     +                        iprms (jj), ier )
                IF ( iprms ( jj ) .eq. 0 ) THEN
                    iprms ( jj ) = nparm + 1
                    CALL DC_WLOG ( 2, 'DCTAF', -3,
     +                             cprms ( jj ), ier )
                END IF
            END DO
        END IF
C
C*	Loop until a timeout occurs.
C
	iperr  = 0
C
	DO WHILE ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bull, lenbul, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
C
C*		Parse the header info from the bulletin.
C
		more = .true.
		CALL DC_GHDR ( bull ( :lenbul ), lenbul, seqnum, wmohdr, 
     +			       oristn, bultim, bbb, nchar, ierr )
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bull (:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCTAF', 2, errstr, ier )
		    more = .false.
		END IF
C
C*		Check for a correction or amendment as part of bulletin
C*		header.
C
		IF ( bbb ( :1 ) .eq. 'C' ) THEN
		    icorn = 1
		  ELSE IF ( bbb ( :1 ) .eq. 'A' ) THEN
		    icorn = 2
		  ELSE
		    icorn = 0
		END IF
C
C*		Save the bulletin to get the report with line feed
C*		characters.
C
		IF ( lenbul .gt. nchar ) THEN
		    bulsav = bull ( nchar + 1:lenbul )
		    lenbsv = lenbul - nchar
		    CALL ST_UNP1 ( bulsav, lenbsv, bulsav, lenout, ier )
		    ire    = 1
		END IF
C
C*		Set pointer iptr to end of header.
C
		CALL ST_UNPR ( bull (:nchar), nchar, strnew, iptr, ier )
		iptr = iptr + 2
C
C*		Remove control characters from entire bulletin and
C*		ensure upper case.
C
		CALL ST_UNPR ( bull ( :lenbul ), lenbul, 
     +                         bull ( :lenbul ), lenb, ier )
		CALL ST_LCUC ( bull ( :lenb ), bull ( :lenb ), ier )
C
C*		Look for 'TAF COR' or 'TAF AMD'.
C
		IF ( ( iptr + 9 ) .le. lenb ) THEN
		    string = bull ( iptr:iptr + 9 )
		    IF ( INDEX ( string, 'TAF COR' ) .ne. 0 ) THEN
			icorn = 1
		      ELSE IF ( INDEX ( string, 'TAF AMD' ) .ne. 0) THEN
			icorn = 2
		    END IF
		  ELSE
		    more = .false.
		END IF
C
C*		Get the system time, and make a standard GEMPAK time
C*		from the "current" time.
C
		itype = 1
		CALL CSS_GTIM ( itype, sysdt, ier )
		IF ( curtim .eq. 'SYSTEM' )  THEN
		    dattmp = sysdt
		  ELSE
		    CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		END IF
		CALL TI_CTOI ( dattmp, istarr, ier )
C
C*		Loop through the reports.
C
		DO WHILE ( more )
C
C*		    Get the next report and the station id.
C
		    ibptr = iptr
		    lpart = lenb - ibptr + 1
		    IF ( lpart .gt. 8 ) THEN
		        CALL TF_GRPT ( bull ( ibptr:lenb ), lpart, iptr,
     +		                       rpt ( :lenb ), lenr, stid, cntry,
     +				       iret )
C
			IF  ( iret .eq. 0 )  THEN
			    CALL ST_FIND ( stid, stidb, nblk,
     +					   ipos, ier )
			    IF  ( ipos .ne. 0 )  THEN
				iret = -1
			    END IF
			END IF
			iptr = iptr + ibptr
		      ELSE
			iret = -2
		    END IF
C
		    IF ( ( iret .ne. 0 ) .and. ( iret .ne. -3 ) ) THEN
			good = .false.
			IF ( iret .eq. -2 ) THEN
C
C*                          There are no more reports in this bulletin.
C
			    more = .false.
			  ELSE IF ( iret .eq. -1 ) THEN
			    CALL DC_WLOG ( 4, 'DCTAF', 5, 
     +					  bull( ibptr:ibptr + 9 ), ier )
			END IF
		      ELSE
		        good = .true.
C
C*			Get the report time.
C
			CALL TF_SHDR ( rpt ( :lenr ), lenr, iptrh,
     +			               irday, irhr, irmin, iret )
			IF ( iret .eq. 1 ) THEN
C
C*			    Use the bulletin hour and minute as the 
C*			    report time.
C
			    CALL ST_LSTR ( bultim, lenbtm, ier )
			    IF ( ( lenbtm .eq. 6 ) .or. 
     +				 ( lenbtm .eq. 7 ) ) THEN
			        CALL ST_NUMB ( bultim (3:4), irhr, ier )
			        IF ( ier .ne. 0 ) THEN
				    good = .false.
			          ELSE
				    IF ( ( irhr .lt. 0 ) .or.
     +				       ( irhr .gt. 23 ) ) good = .false.
			        END IF
			        CALL ST_NUMB ( bultim (5:6), irmin, ier)
			        IF ( ier .ne. 0 ) THEN
				    good = .false.
			          ELSE
				    IF ( ( irmin .lt. 0 ) .or.
     +				      ( irmin .gt. 59 ) ) good = .false.
			        END IF
			      ELSE
				good = .false.
			    END IF
			  ELSE IF ( iret .lt. 0 ) THEN
			    good = .false.
			END IF
			IF ( .not. good ) CALL DC_WLOG ( 4, 'DCTAF', 4, 
     +					  rpt( :lenr ), ier )
		    END IF
C
C*		    Get the base time to assign to this report.
C
		    IF ( good ) THEN
C
C*		    	Get observation time.
C
			CALL DC_GTIM  ( istarr, bultim, irhr, irmin,
     +					offtim, irtarr, dattim, ier1 )
			ihhmm = irhr * 100 + irmin
			IF ( ier1 .ne. 0 ) THEN
			    CALL DC_WLOG ( 2, 'DC', ier1, ' ', ier )
			END IF
C
C*		    	Compute difference between observation and
C*		    	system times.
C
			CALL TI_MDIF ( irtarr, istarr, imdif, ier2 )
C
C*		    	Check that the time is within NHOURS before
C*		    	the system time for GEMPAK.
C
 			IF ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +			      ( imdif .gt. 60 ) .or.
     +		  	      ( imdif .lt. ((-60)*nhours) ) ) THEN
			    good   = .false.
			    errstr = wmohdr // oristn // bultim // bbb
			    CALL DC_WLOG ( 2, 'DCTAF', 3, errstr, ier )
			    CALL ST_INCH ( imdif, cmdif, ier )
			    errstr = dattim // dattmp // cmdif
			    CALL DC_WLOG ( 2, 'DCTAF', 4, errstr, ier )
			END IF
		    END IF
C
C*		    Decode the report.
C
		    IF ( good ) THEN
			CALL TF_DECD ( rpt ( iptrh:lenr ), iprms,
     +                                irtarr, ihhmm, lunf, cntry, rdata,
     +				      jrtarr, ntimes, stidnw, jvehr,
     +				      iret )
			ier1 = 99
			ier3 = 99
C
			IF ( iret .eq. 0 ) THEN
C
C*			    TF_SY24 will return the 6-hour synoptic time
C*			    jstarr and if necessary derive it from the
C*			    ending valid time and a 24 hour valid time
C*			    period.  The new initial forecast time array
C*			    is returned in nrtarr.
C
			       CALL TF_SY24 ( jrtarr, ntimes, jvehr,      
     +			   	              jstarr, ndxstr, ndxend,
     +				              nrtarr, ier3 )
			END IF
C
C*			Convert initial synoptic fcst time in jstarr
C*			to GEMPAK time
C
			IF ( ier3 .eq. 0 ) THEN
			    CALL TI_ITOC ( jstarr, bvldtm, ier1 ) 
			    IF ( ier1 .ne. 0 ) THEN
			      CALL DC_WLOG ( 2, 'DC', ier1, ' ', ier )
			    END IF
			END IF
C
C*                      Make a file name from the template and the time.
C*                      Open the file as part of the open file list.
C
			IF ( ier1 .eq. 0 ) THEN
			    CAll FL_MNAM ( bvldtm, gemfil, filnam, ier )
			    IF ( ier .ne. 0 ) THEN
				CALL DC_WLOG ( 2, 'FL', -15, ' ', ier )
				good = .false.
			      ELSE
				CALL DC_FCYL ( filnam, iflsrc, stntbl,
     +					       iadstn, maxtim, lunf,
     +					       nparm, parms, ierr )
C*				Check that file was opened properly
				IF ( ierr .ne. 0 ) THEN
				    CAll DC_WLOG ( 0, 'SF', ierr, filnam,
     +						   ier )
				    good = .false.
				END IF
			    END IF
			END IF
			IF ( good .and. ier1 .eq. 0 ) THEN
			    isvday = irtarr ( 3 )
C
C*			    Get the mountain obscuration value for this
C*			    station.
C
			    CALL ST_FIND ( stid, stidmt, nstn, ipos, 
     +					   ier )
			    IF ( ipos .gt. 0 ) THEN
				obsc = obscmt ( ipos )
			      ELSE
				obsc = RMISSD
			    END IF
C
C*			    Loop to write out all forecast times.
C
			    DO ii = ndxstr, ndxend
                                good = .true.
                                IF ( cirflg ) THEN
                                  CALL TF_VALT( istarr, nrtarr, maxtim,
     +                                          vtimflg ); 
                                  IF ( .not. vtimflg ) THEN
                                    good = .false.
                                  END IF
                                ENDIF
                                IF ( good ) THEN
C
C*			          Make initial forecast time a GEMPAK 
C*                                time
C
			          CALL TI_ITOC ( nrtarr, dattim, ier )
C
C*    	                          Set the station and time in the 
C*                                output file.
C
			          CALL RA_TMST ( lunf, dattim, stid,
     +			                addstn, cirflg, datflg, iret )
C
C*			         Check for an error.
C
			          IF ( iret .ne. 0 ) THEN
			            errstr = ' '
			            IF ( iret .eq. -4 ) errstr = dattim
			            IF ( iret .eq. -5 ) errstr = stid
			            CALL DC_WLOG ( 2, 'RA', iret,
     +					               errstr, ier )
			          ELSE
C
C*			            Write report to GEMPAK file.
C
			            DO jj = 1, MMPARM
				        adata (jj) = rdata (ii, jj)
			            END DO
				    IF ( iprms ( 23 ) .le. MMPARM )
     +					     adata ( iprms (23) ) = obsc
			            CALL SF_WDAT ( lunf, ihhmm,
     +					           adata, ier )
				    IF ( ii .eq. 1 ) THEN
C
C*				        Extract report with line feed
C*					characters from bulletin.
C  
					irb = INDEX ( bulsav ( ire: ),
     +						        stid ( :5 ) )
					irb = ire + irb - 1
					IF ( irb .le. 0 ) irb = 1
C
					IF ( stidnw .eq. ' ' ) THEN
					  ire = INDEX ( bulsav (irb:),
     +						        '=')
					  ire = ire + irb - 2
					 ELSE
					  ire = INDEX (bulsav(irb+4:),
     +						  stidnw ( :4 ) )
					  ire = ire + irb + 1
					END IF
					IF (ire .lt. irb) ire = lenout
C
C*			                Write text to GEMPAK file and
C*					save original report time.
C
			    	        CALL SF_WSTR ( lunf, ihhmm, 
     +			                         bulsav (irb:ire), ier )
					origtm = 'REPORT AT '
     +						   // dattim
				      ELSE
			    	        CALL SF_WSTR ( lunf, ihhmm, 
     +			                                 origtm, ier )
				    END IF
				  END IF
                                END IF
				isvday = nrtarr ( 3 ) 
C
				CALL TI_ADDM ( nrtarr, 60, nrtarr, ier )
			    END DO
C
C*			    Check for a new station id (start of a new
C*			    report) embedded in the current report.
C
			    IF ( stidnw .ne. ' ' ) THEN
				jptr = INDEX ( bull ( ibptr:lenb ), 
     +					       stidnw ( :4 ) ) 
				IF ( jptr .gt. 0 ) THEN
C
C*				    Reset the pointer to the new report.
C*				    Check for the case of the new id
C*				    being the same as the current id.
C
				    iptr = jptr + ibptr - 1
				    IF ( stidnw .eq. stid )
     +					 iptr = iptr + 4
				    IF ( .not. more ) more = .true.
				END IF
    			    END IF
			  ELSE
			    errstr = dattim // rpt ( :iptrh + 6 )
			    CALL DC_WLOG ( 2, 'DCTAF', 4, errstr, ier )
			END IF
		    END IF
		END DO
	      ELSE
C
C*		Write an error to the decoder log file.
C
		CALL DC_WLOG ( 0, 'DC', iperr, ' ', ier )
	    END IF
	END DO
C
	CALL DC_FCLS ( ier )
C*
	RETURN
	END
