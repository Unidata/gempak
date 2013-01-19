	SUBROUTINE MT_DCOD ( curtim, gemfil, stntbl, prmfil, 
     +			     iadstn, maxtim, nhours, iret )
C************************************************************************
C* MT_DCOD								*
C*									*
C* This routine will decode METAR/SPECI or SAO bulletins and		*
C* write the data to a GEMPAK surface file.				*
C*									*
C* MT_DCOD ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,	*
C*	     IRET )					                *
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	PRMFIL		CHAR*		Parameter packing table		*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	RIVALS(IRCORN)	REAL		Report correction indicator     *
C*	RIVALS(IRSLAT)  REAL		Report latitude                 *
C*	RIVALS(IRSLON)  REAL		Report longitude                *
C*	RIVALS(IRSELV)  REAL		Report elevation                *
C*	CIVALS(ICSTID)  CHAR*		SAO station id                  *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP     1/96	                                        *
C* D. Kidwell/NCEP     2/96  Added METAR weather codes, hourly prec,    *
C*			     correction flag check and NCO flag         *
C* D. Kidwell/NCEP     3/96  Integrated DC_WLOG calls                   *
C* D. Kidwell/NCEP     4/96  Cleaned up code; renamed from DCMDCD       *
C* D. Kidwell/NCEP     5/96  Modified cloud processing; added BUFR      *
C* K. Tyle/GSC	      11/96  Added SAO decoding; write SPECI's   	*
C* K. Tyle/GSC	       1/97  Use new packing table vars; decode AFOS	*
C*			     bulletins; change calls to DC_WLOG; MT_RTIM*
C*			     -> RA_RTIM; remove BUFR-related conditional*
C*			     compilation; remove call to IN_BDTA;    	*
C*			     MT_TMST -> RA_TMST         		*
C* K. Tyle/GSC	       2/97  Changed error processing		        *
C* K. Tyle/GSC	       4/97  Cleaned up; changed logging and SPECI write*
C* D. Kidwell/NCEP     6/97  ST_LSTR -> INDEX; use explicit char length *
C* D. Kidwell/NCEP     4/98  Removed bufr/nco references; new interface;*
C*			     removed iasos flag; cleaned up; added vrsn *
C* D. Kidwell/NCEP     5/98  Version 3.0 --> 3.1                        *
C* D. Kidwell/NCEP    10/98  Added intf mnemonics to calling sequences  *
C* D. Kidwell/NCEP    10/98  Restructured for writing text, specials    *
C* A. Hardy/GSC        3/99  Added priority parameter to SG_QSTN        *
C* A. Hardy/GSC        3/99  Removed ispri = 0                          *
C* F. J. Yen/NCEP      6/99  Decoded off-time reports			*
C* D. Kidwell/NCEP     5/01  Fixed to handle identical US, CN stid*3    *
C* F. J. Yen/NCEP      4/02  Put specials in correct order		*
C* D. Kidwell/NCEP     9/02  Fixed bug in ordering specials             *
C* F. J. Yen/NCEP      2/04  Fixed so won't process if bad headers and	*
C*			     checked for value of nchar.  Fixed length 	*
C*			     of errstr in calls to DC_WLOG.		*
C* B. Yin/SAIC         3/04  Changed SS_GTIM to CSS_GTIM                *
C* D. Kidwell/NCEP     9/05  CSC for MT_GRPT to add corflg              *
C* D. Kidwell/NCEP    10/05  Changed log level on return from DC_GHDR   *
C* M. Li/SAIC	      06/07  Add a check for Candadian stations		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'mtcmn.cmn'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	CHARACTER	bultin*(DCMXBF), report*(DCMXBF),
     +			sysdt*12, dattmp*12, filnam*132, errstr*80,
     +			dattim*15, rpttyp*4, stid*8, bultyp*4,stnm*8,
     +			autop*4, stat*4, countr*4, sttmp*8, pilhdr*12,
     +			tmstr*4, coun*2, parms(MMPARM)*4, strnew*80,
     +			cmdif*8, cprms(MMPARM)*4, cnstn*4, dattm2*15,
     +			rimnem(NRIMN)*8, cimnem(NCIMN)*8, string*160
	INTEGER		istarr (5), imnem (MMPARM), itype
	LOGICAL		more, good, corflg, addstn, cirflg, datflg,
     +			corbul, saoflg, hourly, unique, offtim
C-----------------------------------------------------------------------
	iret = 0
C
C*	Write decoder version number to log.
C
	CALL DC_WLOG ( 2, 'DCMETR', 7, '3.3', ierr)
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file and file source.  Initialize
C*	off-time flag.
C
	IF ( maxtim .ne. 72 ) THEN
	    offtim = .false.
	  ELSE
	    offtim = .true.
	END IF
	maxfil = 2
	iftype = 1
	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
	addstn = .true.
	cirflg = .false.
	iflsrc = 2 + MFTEXT 
C
C*	Set the pointers for the interface arrays.
C
	CALL MT_IFSP ( rimnem, cimnem, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL DC_WLOG ( 0, 'DCMETR', -8, ' ', ierr)
	    RETURN
	END IF
C
C*	Initialize the GEMPAK parameter array and set up links to
C*	the interface mnemonics.
C
	CALL MT_INTF ( rimnem, cimnem, cprms, imnem, numprm, kret )
C
C*	Loop until a timeout occurs.
C
	iperr  = 0
	irpcnt = 0
	DO WHILE ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
C
C*		Parse the header info from the bulletin.
C
		more = .true.
		IF ( ifdtyp .eq. 0 ) THEN
		    CALL DC_GHDR ( bultin ( :lenbul ), lenbul, seqnum,
     +			    wmohdr, oristn, bultim, bbb, nchar, ierr )
		  ELSE
		    CALL DC_GPIL ( bultin ( :lenbul ), lenbul, pilhdr, 
     +			           wmohdr, oristn, bultim, nchar, ierr )
		    bbb = ' '
		END IF
		IF ( ierr .ne. 0 .or. nchar .gt. 80 ) THEN
		    CALL DC_WLOG ( 0, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 0, 'DCMETR', 2, errstr(1:len1), ier )
		    more = .false.
		END IF
		IF ( more ) THEN
		    coun = wmohdr (3:4)
C
C*		    Check for a correction as part of bulletin header.
C
		    IF ( bbb ( 1:1 ) .eq. 'C' ) THEN
		        corbul = .true.
		      ELSE
		        corbul = .false.
		    END IF
C
C*		    Set pointer to skip over header.
C
		    CALL ST_UNPR (bultin(:nchar), nchar, strnew, ibpnt, ier)
C
C*		    Get the system time, and make a standard GEMPAK time
C*		    from the "current" time.
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
C*		    Save receipt time in common.
C
		    DO i = 1, 5
		        irctm ( i ) = istarr ( i ) 
		    END DO
C 
C*		    Check for header of form METAR, SPECI, or MTR.
C
		    IF ( ( nchar + 6 ) .gt. lenbul ) more = .false.
		    IF ( more ) THEN
		        CALL MT_FIND ( bultin ( :lenbul ), nchar, ibpnt,
     +				       bultyp, mhr, mmin, iret )
C
C*		        Remove control characters from entire bulletin.
C
		        CALL ST_UNPR ( bultin ( :lenbul ), lenbul, 
     +				       bultin ( :lenbul ), lenb, iret )
		    END IF
		END IF
C
C*		Loop through the reports.
C
		DO WHILE ( more )
C
C*		    Get next report.
C
		    CALL MT_GRPT ( bultin ( :lenb), lenb, bultyp, ibpnt,
     +		                   rpttyp, report ( :lenb), lenr, 
     +				   corflg, iret )
		    IF ( iret .ne. -2 ) THEN
		    	irpcnt = irpcnt + 1
C
C*			Write every tenth report to the log.
C
			IF ( ( MOD ( irpcnt, 10 ) .eq. 0 ) ) 
     +			    CALL DC_WLOG ( 2, 'DCMETR', 2,
     +					    report( :lenr ), ier )	
		    END IF
		    irpntr = 1
		    good   = .true.
		    IF ( .not. corflg ) corflg = corbul 
		    IF ( corflg )  rivals ( ircorn ) = 1.
C
C*		    Check for SAO format.
C
		    saoflg = rpttyp .eq. 'SA' .or. rpttyp .eq. 'SX'
C
		    IF ( iret .ne. 0 ) THEN
			good = .false.
			IF ( iret .eq. -2 ) THEN
C
C*                          There are no more reports in this bulletin.
C
			    more = .false.
			  ELSE IF ( iret .eq. -1 ) THEN
			    CALL DC_WLOG ( 4, 'DCMETR', 5, 
     +					   report( :lenr ), ier )
			END IF
		      ELSE
C
C*			Check for SAO's.
C
			IF ( saoflg ) THEN
			    CALL RA_GFLD ( report ( :lenr), lenr, iret )
			    IF ( iret .eq. 0 ) THEN
				CALL RA_RHDR ( irpntr, stnm, rpttyp, 
     +				    corflg, autop, irhour, irmin, iret )
				stid = stnm
				civals ( icstid ) = stid
			    END IF
			    IF ( iret .ne. 0 ) good = .false.
			  ELSE
C
C*			    This is METAR format.  Get the report time.
C
			    CALL MT_SHDR ( report ( :lenr), irpntr,
     +			                   irday, ihour, iminit, iret )
			    stid   = civals ( icstid ) 
			    irhour = ihour
			    irmin  = iminit
			END IF
		    END IF
C
C*		    Get the time to assign to this bulletin.
C
		    IF ( good ) THEN
C
C*		    	Get observation time.
C
			IF ( .not. saoflg ) THEN 
C
C*		            Some METAR reports only have the
C*			    bulletin time, and no report time.
C
			    IF  ( irhour .lt. 0 )  THEN
				tmstr = bultim(3:4)
				CALL ST_NUMB ( tmstr, irhour, ier )
			    END IF
			    IF  ( irmin .lt. 0 )  THEN
				tmstr = bultim(5:6)
				CALL ST_NUMB ( tmstr, irmin, ier )
			    END IF
			END IF
			CALL DC_GTIM  ( istarr, bultim, irhour, irmin,
     +					offtim, irtarr, dattim, ier1 )
			ihhmm = irhour * 100 + irmin
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
			    errstr = wmohdr // oristn // bultim
			    CALL DC_WLOG ( 2, 'DCMETR', 3, errstr (:28),
     +					   ier )
			    CALL ST_INCH ( imdif, cmdif, ier )
			    errstr = dattim // dattmp // cmdif
			    CALL DC_WLOG ( 2, 'DCMETR', 4, errstr (:35),
     +					   ier )
			END IF
		    END IF
C
C*		    Open the output file.
C
		    IF ( good ) THEN
C
C*			Make a file name from the template and the time.
C*			Open the file as part of the open file list.
C
			CALL FL_MNAM ( dattim, gemfil, filnam, ier )
			CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, 
     +				     maxtim, lunf, nparm, parms, ierr )
C
C*			Check that the file was opened properly.
C
			IF ( ierr .ne. 0 ) THEN
C
C*			    If not, write an error to the decoder
C*			    log file.
C
			    CALL DC_WLOG ( 0, 'SF', ierr, filnam, iret )
			END IF
		    END IF
C
C*		    Check for correction flag within report.
C
		    IF ( good .and. ( .not. corflg ) ) THEN
			indxcr = 0
			IF ( coun .ne. 'CN' ) THEN
			    indxcr = INDEX ( report ( :lenr ), ' COR ' )
			  ELSE
			    indxcr = INDEX ( report ( :lenr ), ' CC' )
			END IF
			IF ( ( indxcr .gt. 10) .and. (indxcr .lt. 20 ) )
     +			    corflg = .true.
		    END IF
C
C*		    Set the station and time in the output file.
C
		    IF ( good ) THEN
C
C*			The following checks are made for US and 
C*			Canadian stations:
C*			If the station is Canadian and 3 characters
C*			long, a 'C' is prepended and the station table 
C*			is checked for the 4 character name; if found,
C*			the 4 character name is used; if not, the 3
C*			character name is used.
C*			If the station is initially 4 characters long 
C*			and begins with 'K' (US) or 'C' (Canadian), the
C*			station table is checked for the 4 character 
C*			name; if found, the 4 character name is used;
C*			if not, the first character is stripped off to
C*			make a 3 character name.
C*			This allows both US and Canadian stations to 
C*			have the same 3 character id, provided the
C*			Canadian station has a 'C' prepended to it in 
C*			the station table (whether or not the 'C' occurs
C*			in the METAR report.)
C
			lenid = INDEX ( stid, ' ' ) - 1
			IF ( lenid .eq. 3  .and. 
     +			     ( oristn (1:2) .eq. 'CW' )  ) THEN
			    cnstn = 'C' // stid ( :3 )
			    CALL SF_FSTN ( lunf, cnstn, ier )
			    IF ( ier .eq. 0 ) stid = cnstn
			  ELSE IF ( lenid .eq. 4 .and. 
     +				    ( stid ( :1 ) .eq. 'K' .or.
     +				      stid ( :1 ) .eq. 'C' ) ) THEN
			    CALL SF_FSTN ( lunf, stid, ier )
			    IF ( ier .eq. -10 ) stid = stid ( 2:4 )
			END IF
C
C*			For specials with a report time at or after 45
C*			minutes past the hour, if the station has not
C*			already reported, undo rounding up of hour by
C*			subtracting one hour before re-invoking DC_GTIM.
C
			IF ( rpttyp .eq. 'SP' .and. irmin .ge. 45 ) THEN
			    datflg = .false.
C
C*			    Find the time in the file.
C
			    CALL SF_FTIM  ( lunf, dattim, ier )
			    IF ( ier .eq. 0 ) THEN
C
C*				Set the station
C
				CALL SF_FSTN  ( lunf, stid, ier )
				IF ( ier .eq. 0 ) THEN
C
C*				    Check to see if station has already
C*				    reported.
C
				    CALL SF_QDAT  ( lunf, datflg, ier1 )				    
				END IF
			    END IF
			    IF ( ier .ne. 0 .or. .not. datflg ) THEN
				IF ( irhour .eq. 0 ) THEN
				   irhour = 23
				  ELSE
				   irhour = irhour - 1
				END IF
			        CALL DC_GTIM  ( istarr, bultim, irhour,
     +					irmin, offtim, irtarr, dattm2,
     +			 		ier1 )
				IF ( ier1 .ne. 0 ) THEN
			    	    CALL DC_WLOG ( 2, 'DC', ier1,
     +					           ' ', ier )
				END IF
C
C*		    		Compute difference between observation
C*				and system times.
C
				CALL TI_MDIF ( irtarr, istarr, imdif,
     +					       ier2 )
C
C*		    		Check that the time is within NHOURS 
C*				before the system time for GEMPAK.
C
 				IF ( ( ier1 .ne. 0 ) .or.
     +				     ( ier2 .ne. 0 ) .or.
     +			             ( imdif .gt. 60 ) .or.
     +		  	             ( imdif .lt. ((-60)*nhours) ) ) THEN
			    	    good   = .false.
			    	    errstr = wmohdr // oristn // bultim
			    	    CALL DC_WLOG ( 2, 'DCMETR', 3, 
     +						   errstr (:28), ier )
			    	    CALL ST_INCH ( imdif, cmdif, ier )
			    	    errstr = dattim // dattmp // cmdif
			    	    CALL DC_WLOG ( 2, 'DCMETR', 4, 
     +						   errstr (:35), ier )
				END IF
				IF ( good ) THEN
				    IF ( dattm2 .ne. dattim ) THEN
				        dattim = dattm2
C
C*				        Make a file name from the 
C*					template and the time.  Open the
C*					file as part of the open file 
C*					list.
C
				        CALL FL_MNAM ( dattim, gemfil,
     +					               filnam, ier )
				        CALL DC_FCYL ( filnam, iflsrc,
     +					      stntbl, iadstn, maxtim,
     +					      lunf, nparm, parms, ierr )
C
C*				        Check that the file was opened
C*				        properly.
C*
				        IF ( ierr .ne. 0 ) THEN
C
C*			    	            If not, write an error to
C*					    the decoder log file.
C
			    	            CALL DC_WLOG ( 0, 'SF',
     +					            ierr, filnam, iret )
				        END IF
				    END IF
				END IF
			    END IF
			END IF
			IF ( good ) THEN
			    CALL RA_TMST ( lunf, dattim, stid, addstn,
     +				           cirflg, datflg, iret )
			    sttmp = stid
			    CALL SF_QSTN  ( lunf, sttmp, istnm, slat,
     +				            slon, selv, ispri, stat,
     +					    countr, ier )
			    IF ( ier .eq. 0 ) THEN
			        rivals ( irslat ) = slat
			        rivals ( irslon ) = slon
			        rivals ( irselv ) = selv
			        contry            = countr
			    END IF
C
C*			    Check for an error.
C
			    IF ( iret .ne. 0 ) THEN
			        good   = .false.
			        IF ( iret .eq. -4 ) THEN
				    errstr = dattim
				    len1 = 15
				  ELSE IF ( iret .eq. -5 ) THEN
      				    errstr = stid
				    len1 = 8
				  ELSE
				    errstr = ' '
				    len1 = 1
				END IF
			        CALL DC_WLOG ( 2, 'RA', iret,
     +					       errstr (:len1), ier )
			    END IF
			END IF
		    END IF
C
C*		    Determine what type of report this is.
C
		    IF ( good ) THEN
		        hourly = rpttyp .eq. 'MT' .or. rpttyp .eq. 'SA'
		        IF ( hourly .and. ( .not. datflg .or. corflg ) )
     +			     THEN
C
C*			    This is an hourly METAR or SA report which
C*			    is either not already in the file or is a
C*			    correction.  Decode the data and write to 
C*			    the surface file.
C
			    IF ( saoflg ) THEN	
			        CALL MT_DSAO ( irpntr, countr, 
     +					       irtarr ( 4 ), iret )
			      ELSE
			        CALL MT_DECD ( report ( :lenr ), lenr, 
     +                                         irpntr, iret )
			    END IF
			    IF ( iret .eq. 0 ) THEN
C
C*			        Write decoded values to decoder log.
C
C 			        CALL MT_IFPT ( lenr, report, rimnem,
C    +					       cimnem, iret )
C
C*			        Write report and text to GEMPAK file.
C
			        CALL MT_GEMP ( lunf, ihhmm, nparm,
     +			                       parms, cprms, imnem, 
     +					       numprm, saoflg, iret )
			        CALL SF_WSTR ( lunf, ihhmm,
     +			                       report ( :lenr ), ier )
			    END IF
		          ELSE
C
C*			    This is a special or off-time report, or a
C*			    duplicate.  Discard duplicates of the hourly
C*			    report.
C
			    unique = .true.
			    IF ( hourly .and. datflg ) THEN
C
C*				Get the text of the hourly report.
C
				CALL SF_RSTR ( lunf, string, jhhmm, nch,
     +				               iret )
				IF ( ( iret .eq. 0 ) .and.
     +				     ( ihhmm .eq. jhhmm ) ) THEN
				  lnr = MIN ( lenr, 160 )
				  IF ( lnr .le. nch ) THEN
C
C*				    Check to see if current report is
C*				    identical to already-stored text.
C
				    IF ( string(:lnr) .eq. report(:lnr))
     +					 unique = .false.
				  END IF
				END IF
			    END IF
C
			    IF ( .not. hourly .or. unique ) THEN
C
C*			        Write the text of the SPECI, the SAO 
C*				special, or the unique off-time (e.g.,
C*				ASOS) report to the output file.
C
			        CALL SF_WSPC ( lunf, ihhmm, 
     +					       report ( :lenr ), iret )
			    END IF
			END IF
		    END IF
		END DO
	      ELSE
C
C*		Write an error to the decoder log file.
C
		CALL DC_WLOG ( 0, 'DC', iperr, ' ', iret )
	    END IF
	END DO
C
	CALL DC_FCLS ( ier )
C*
	RETURN
	END
