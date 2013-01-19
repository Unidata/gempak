	SUBROUTINE IS_DCOD  ( curtim, gemfil, stntbl, prmfil, iadstn,
     +			      maxtim, nhours, iret )
C************************************************************************
C* IS_DCOD								*
C*									*
C* This subroutine decodes WSNT_, WCNT_, WVNT_, WSPN_, WCPN_, and WVPN_ *
C* international sigmet reports from KKCI;				*
C* WSPA_, WCPA_ and WVPA_ reports from PHFO;				*
C* WSUK31, WSUK33, and WSNT21 reports from EGGY;			*
C* WSJP31 reports from RJAA;						*
C* WSCU reports from MUHA;						*
C* WSCN reports from CW__ and CYQX and writes the data to an ASCII	*
C* file.  Remaining ("other") international reports that follow the	*
C* standard format are decoded, but not fully.				*
C*									*
C* IS_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
C*	      IRET )    						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	PRMFIL		CHAR*		Parameter packing table         *
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP     10/99						*
C* D. Kidwell/NCEP     10/99	Added check for correction flag w/in rpt*
C* D. Kidwell/NCEP     11/99	Added KNHC				*
C* F. J. Yen/NCEP      11/99	Added PHNL and PGUM			*
C* F. J. Yen/NCEP       1/00	Added EGGY and KKCI			*
C* F. J. Yen/NCEP       2/00	Added RJAA				*
C* D. Kidwell/NCEP      3/00	Added TEST bull. chk; WW_RTIM -> DC_ITIM*
C* D. Kidwell/NCEP	4/00	Revised WMO header & origin checks	*
C* D. Kidwell/NCEP	6/00	Changed PGUM WMO header WSPQ2 -> WSPQ	*
C* F. J. Yen/NCEP	 7/00	Replaced PHNL with PHFO			*
C* F. J. Yen/NCEP	 5/01	Added NTAA				*
C* F. J. Yen/NCEP	10/01	Added MUHA				*
C* F. J. Yen/NCEP	11/01	Added generic (other) reports and	*
C*				eliminated some false correction flags.	*
C* F. J. Yen/NCEP	12/01	Eliminated more false correction flags.	*
C*				Changed log level for id not in table   *
C* A. Hardy/NCEP	 9/02	Added origin to IS_DECD & IS_EGGY calls	*
C* F. J. Yen/NCEP	 6/03	Checked for short error message string 	*
C*				and increase size of work array to 1000.*
C*				Expanded WMO header checks.		*
C* F. J. Yen/NCEP	 9/03	Revised to decode PANC reports fully.	*
C* F. J. Yen/NCEP	10/03	Revised to decode Canadian reports fully*
C* B. Yin/SAIC		 3/04	Changed SS_GTIM to CSS_GTIM		*
C* J. Lewis/AWC		 4/05	Revised WMO header and origin checks	*
C* T. Piper/SAIC	10/05	Removed code for outdated IDs		*
C* J. Lewis/AWC          8/07   Remove check for correction flag        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	CHARACTER	bultin*(DCMXBF),
     +			seqnum*4, wmohdr*8, oristn*8, bultim*12,
     +			bbb*8, errstr*80, work*2000, origin*4,
     +			sysdt*12, dattmp*12, tissue*20
	CHARACTER	astid(LLSTFL)*8, stnnam(LLSTFL)*32,
     +			stat(LLSTFL)*2, coun(LLSTFL)*2,
     +			tbchrs(LLSTFL)*20
	INTEGER		istarr (5), iotarr (5)
	INTEGER		istnm(LLSTFL), ispri(LLSTFL), itype
	REAL		slat(LLSTFL), slon(LLSTFL), selv(LLSTFL)
	LOGICAL		good, other
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize open file lists.  Set the max number of open files.
C*	Set the type of output file and file source.
C
	maxfil = 2
	iftype = 6
 	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
C
C*	Read the international sigmet MWO table.
C
	CALL DC_STNS  ( stntbl, astid, istnm, stnnam, stat, coun, slat,
     +          slon, selv, ispri, tbchrs, nstn, ier )
C
C*	Loop until a timeout occurs.
C
	iperr = 0
	DO WHILE ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
		good = .true.
		other = .false.
                icorr = 0
		CALL DC_TEST ( bultin, lenbul, itest, ier )
                icorr = icorr + itest
C
C*		Parse the header info from the bulletin.
C
		IF ( ifdtyp .eq. 0 ) THEN
		    CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +		                   oristn, bultim, bbb, nchar, ierr )
		  ELSE
		    ierr = -12
		END IF
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin (:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCISIG', 1, errstr, ier )
		    good = .false.
		END IF
C
C*		Check for bulletins from KKCI (AWC), PAWU (AAWU), and PHFO
C*		(WFO Honolulu) as well as RJAA, EGGY, NTAA, MUHA, CW__, and CYQX.
C
		IF ( good ) THEN
		    origin = oristn ( :4 )
		    ifeggy = 0
		    IF ( origin .eq. 'KKCI' ) THEN
		        IF ( ( wmohdr ( :4 ) .ne. 'WSNT' ) .and.
     +		             ( wmohdr ( :4 ) .ne. 'WCNT' ) .and.
     +			     ( wmohdr ( :4 ) .ne. 'WVNT' ) .and.
     +			     ( wmohdr ( :4 ) .ne. 'WSPN' ) .and.
     +			     ( wmohdr ( :4 ) .ne. 'WCPN' ) .and.
     +		             ( wmohdr ( :4 ) .ne. 'WVPN' ) )
     +		             good = .false.
		    ELSE IF ( origin .eq. 'PAWU' ) THEN
		        IF ( ( wmohdr ( :5 ) .ne. 'WSAK0' ) .and.
     +		             ( wmohdr ( :5 ) .ne. 'WCAK0' ) .and.
     +			     ( wmohdr ( :5 ) .ne. 'WVAK0' ) )
     +			     good = .false.
		    ELSE IF ( origin .eq. 'PHFO' ) THEN
		        IF ( ( wmohdr ( :4 ) .ne. 'WSPA' ) .and.
     +			     ( wmohdr ( :4 ) .ne. 'WCPA' ) .and.
     +			     ( wmohdr ( :4 ) .ne. 'WVPA' ) )
     +			     good = .false.
 		    ELSE IF ( origin .eq. 'RJAA' ) THEN
		        IF ( wmohdr ( :6 ) .ne. 'WSJP31' ) THEN
 			    good   = .false.
 		        ELSE
 			    ifeggy = 2
 		        END IF
		    ELSE IF ( origin .eq. 'EGGY' ) THEN
		        IF ( ( wmohdr ( :5 ) .ne. 'WSUK3' ) .and.
     +			     ( wmohdr ( :6 ) .ne. 'WSNT21' ) ) THEN
			    good   = .false.
		        ELSE
			    ifeggy = 1
		        END IF
		    ELSE IF ( origin .eq. 'NTAA' ) THEN
			IF ( wmohdr ( :4 ) .ne. 'WSPF' ) THEN
			    good = .false.
			ELSE
			    ifeggy = 3
			END IF
		    ELSE IF ( origin .eq. 'MUHA' ) THEN
			IF ( wmohdr ( :4 ) .ne. 'WSCU' ) THEN
			    good = .false.
			ELSE
			    ifeggy = 4
			END IF
		    ELSE IF ( origin (1:2) .eq. 'CW' .or.
     +				origin .eq. 'CYQX' ) THEN
			IF ( wmohdr ( :4 ) .ne. 'WSCN' ) THEN
			    good = .false.
			ELSE
			    ifeggy = 10
			END IF
		    ELSE
			other = .true.
		    END IF
	 	END IF
C
		IF ( good ) THEN
C
C*		    Set the bulletin pointer to one character past
C*		    the end of the WMO header.
C
		    ibpnt = nchar + 1
		    lenorg = lenbul
		    lenbul = lenbul - nchar
C
C*		    Remove unprintable characters.
C
		    CALL ST_UNPR ( bultin ( ibpnt:lenorg ), lenbul, 
     +				   work ( :lenbul ), lenb, ier )
C
C*		    Get the system time, and make a standard GEMPAK time
C*		    from the "current" time.
C
		    itype = 1
		    CALL CSS_GTIM ( itype, sysdt, ier )
		    IF ( curtim .eq. 'SYSTEM' ) THEN
			dattmp = sysdt
		      ELSE
			CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		    END IF
		    CALL TI_CTOI ( dattmp, istarr, ier )
C
C*		    Use the system time and the bulletin time to make a
C*		    Gempak time.
C
		    CALL ST_LSTR ( bultim, lens, ier )
		    IF ( lens .eq. 6 ) THEN
		        CALL ST_INTG ( bultim ( :6), issue, ierr )
			IF ( ierr .ne. 0 ) good = .false.
		      ELSE
			good = .false.
		    END IF
C
		    IF ( good ) THEN
	                irday  = issue / 10000
                        irhour = mod ( issue, 10000 ) / 100
                        irmin  = mod ( mod ( issue, 10000 ), 100 )
                        CALL DC_ITIM ( istarr, irday, irhour, irmin, 
     +				       iotarr, ier )
	                CALL TI_ITOC ( iotarr, tissue, ier )
		      ELSE
			CALL DC_WLOG ( 2, 'DC', -16, ' ', ier )
		    END IF
	        END IF
C
	        IF ( good ) THEN
C
C*		    Decode the international sigmet report.
C
		    IF ( ifeggy .eq. 0 ) THEN

			IF ( .not. other ) THEN

			    CALL IS_DECD ( work ( :lenb ), lenb, iotarr,
     +				    icorr, tissue, gemfil, stntbl,
     +				    iadstn, maxtim, origin, ier )
			  ELSE
			    CALL IS_OTHR ( work ( :lenb ), lenb, iotarr,
     +				    icorr, tissue, gemfil, stntbl,
     +				    iadstn, maxtim, origin, astid,
     +				    slat, slon, nstn, ier )
			END IF
		      ELSE IF ( ifeggy .eq. 10 ) THEN
			CALL IS_CNDA ( work ( :lenb ), lenb, iotarr,
     +				   icorr, tissue, gemfil, stntbl, iadstn,
     +				   maxtim, ifeggy, origin, ier )
		      ELSE
			CALL IS_EGGY ( work ( :lenb ), lenb, iotarr,
     +				   icorr, tissue, gemfil, stntbl, iadstn,
     +				   maxtim, ifeggy, origin, ier )
		    END IF
		    IF ( ier .ne. 0 ) THEN
			lenmn = MIN ( lenb, 80 )
		        CALL ST_UNPR ( work (:lenmn), lenmn, errstr,
     +				       len1, ierr )
			loglvl = 2
			IF ( ier .eq. -9 ) loglvl = 0
			CALL DC_WLOG ( loglvl, 'DCISIG', ier,
     +				       errstr (:len1), ierr )
 	            END IF
C
		END IF
	    END IF
	END DO
C*
	RETURN
	END

