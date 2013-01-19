	SUBROUTINE TA_DCOD ( cldt, bufrta_old, bufrta_new, 
     +						gemfil, nhours, iret)
C************************************************************************
C* TA_DCOD								*
C*									*
C* This routine decodes bulletins containing TAMDAR BUFR messages      	*
C* received directly from AirDat into GEMPAK sounding format and 	*
C* writes the data into the output sounding file.			*
C*									*
C* TA_DCOD ( CLDT, BUFRTA_OLD, BUFRTA_NEW, GEMFIL, NHOURS, IRET )	*
C*									*
C* Input parameters:							*
C*	CLDT		CHAR*		Date-time from command line	*
C*	BUFRTA_OLD	CHAR*		Old TAMDAR BUFR table file   	*
C*	BUFRTA_NEW	CHAR*		New TAMDAR BUFR table file	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	NHOURS		INTEGER		Max # of hours before run time	*
C*					for creating BUFR output	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP	07/07 						*
C* C. Caruso Magee/NCEP	10/07  	Add pressure (007004) to input descrpor	*
C*                             	list.					*
C* m.gamazaychikov/SAIC	02/08	Change output stream to sounding format	*
C* m.gamazaychikov/SAIC	07/08	Cleaned up				*
C* T. Piper/SAIC	08/08	Changed '.dummy/' to '.dummy.'		*
C* T. Piper/SAIC	09/08	Completely restructured logic		*
C* T. Piper/SAIC	10/08	Create unique "Observation ID"		*
C* T. Piper/SAIC	12/08	Added capability to process new format	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'tacmn.cmn'
C*
C*      Number of non-NCEP BUFR tables files.
C*
        PARAMETER       ( NBUFRT = 2 )
C
	PARAMETER	( NACRN=1000, NLV=50, NPARMS=6 )
C
	CHARACTER*(*)	cldt, bufrta_old, bufrta_new, gemfil
C
	CHARACTER	acrn(NACRN)*9, afdate(NACRN, NLV)*13, bbb*8,
     +			bfstyp*8, bufrtf(NBUFRT)*(DCMXLN), buhd*8,
     +			bull*(DCMXBF), bulldt*8, cbf,
     +			cborg*8, cbull*(DCMXBF), cdum*13,
     +                  cimnem(NCIMN)*8, coun(NACRN)*2, cracrn*9,
     +                  dattim*13, filnam*132, newfil*132,
     +			parms(NPARMS)*4, prmfil*72,
     +			rimnem(NRIMN)*8, rundt*12, seqnum*8,
     +			stat(NACRN)*2, sysdt*12
C
        REAL            corn
        REAL            alat(NACRN, NLV), alon(NACRN, NLV),
     +                  datai(NPARMS*NLV), datao(NPARMS*NLV),
     +			flat(NACRN), flon(NACRN),
     +                  poaf(NACRN, NLV),
     +                  wspd(NACRN, NLV), wdir(NACRN, NLV),
     +                  hmdt(NACRN, NLV), tmpk(NACRN, NLV),
     +			hght(NACRN, NLV), pres(NACRN, NLV),
     +                  selv(NACRN)
C
	INTEGER		iacrn(NACRN), iarcft(5), ibull(DCMXBF/4),
     +			irptdt(5), irundt(5), iubfmf(NBUFRT),
     +			iubftf(NBUFRT), nmlev(NACRN), nxdsc(NBUFRT)
C
	LOGICAL         addstn, amatch, bullok, cirflg, datflg,
     +			exist, mrgdat, pkflg, stmflg
C
	EQUIVALENCE	( cbull(1:4), ibull(1) )
C*
C*	Number of expected descriptors within Section 3 of each
C*	type of non-NCEP BUFR message.
C*
	PARAMETER	( NXDSC1 = 23 )
	PARAMETER	( NXDSC2 = 25 )
C*
C*	The following array will hold the list of expected descriptors
C*	within Section 3 of each type of non-NCEP BUFR message.
C*	The first dimension of this array must be at least as large
C*	as the largest of the above NXDSC values.
C*
	CHARACTER	cxdsc( NXDSC2, NBUFRT )*6
C*
C*	Expected descriptors within Section 3 of a TAMDAR BUFR message
C*	prior to October 2008.
C*
	DATA		( cxdsc ( ii, 1 ), ii = 1, NXDSC1 )
     +          / '301011', '301013', '301021', '007002',
     +            '007004', '007010', '010082', '008004', '001008',
     +		  '204002', '031021', '001013',
     +            '012001', '011001', '011002', '011037',
     +            '011038', '020041', '204000', '204007',
     +            '031021', '013009', '204000'/
C*
C*	Expected descriptors within Section 3 of a TAMDAR BUFR message
C*	after October 2008.
C*
	DATA		( cxdsc ( ii, 2 ), ii = 1, NXDSC2 )
     +          / '301011', '301013', '301021', '007002',
     +            '007004', '007010', '010082', '008004', '001008',
     +		  '001095', '001009',
     +		  '204002', '031021', '001013',
     +            '012001', '011001', '011002', '011037',
     +            '011038', '020041', '204000', '204007',
     +            '031021', '013009', '204000'/
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
        bufrtf(1) = bufrta_old
        nxdsc(1) = NXDSC1
C
        bufrtf(2) = bufrta_new
        nxdsc(2) = NXDSC2

C
C*  Set the pointers for the interface arrays.
C
	CALL TA_IFSP  ( rimnem, cimnem, ierfsp )
	IF  ( ierfsp .ne. 0 )  THEN
	    RETURN
	END IF

C
C++++	BUFR input setup
C
C*  Loop on each type of non-NCEP BUFR tables file.
C
	DO ii = 1, NBUFRT
C
C*  Open the tables file.
C
	    CALL FL_TBOP  ( bufrtf(ii), 'bufr', iubftf(ii), ierspn )
	    IF  ( ierspn .ne. 0 )  THEN
	        CALL DC_WLOG  ( 0, 'FL', ierspn, bufrtf(ii), ierwlg )
	        RETURN
	    END IF
C
C*  Open the messages file.
C
	    CALL FL_GLUN  ( iubfmf(ii), iergln )
	    IF  ( iergln .ne. 0 )  THEN
	        CALL DC_WLOG  ( 0, 'FL', iergln, ' ', ierwlg )
	        RETURN
	    END IF
	    WRITE ( UNIT = cbf, FMT = '(I1.1)')  ii
	    OPEN  ( UNIT = iubfmf(ii),
     +		    FILE = '.dummy.dctama_' // cbf,
     +		    FORM = 'UNFORMATTED' )
C
C*  Connect the tables and messages files.
C
	    CALL OPENBF  ( iubfmf(ii), 'IN', iubftf(ii) )
C
C*  Close the tables file.
C
            CALL FL_CLOS  ( iubftf(ii), iercls )
	    IF  ( iercls .ne. 0 )  THEN
		CALL DC_WLOG  ( 0, 'FL', iercls, ' ', ierwlg )
	    END IF
C
	END DO

C
C*  Constants for SN_CRFP
C
	iflsrc = 4
	maxstn = NACRN
	maxtim = 24
	stmflg = .true.
	addstn = .true.
	cirflg = .true.
	prmfil = 'sntdar.pack'

C
C======================================================================
C  Process bulltins...
C
	DO WHILE ( .true. )
C
C*  Get a new bulletin from the input pipe.
C
	  CALL DC_GBUL ( bull, lenb, ifdtyp, iergbl )
	  IF ( iergbl .ne. 0 ) THEN
C
C*  A time-out occurred while waiting for a new bulletin
C*  on the input pipe.  Shut down the decoder and exit.
C
	      CALL DC_WLOG  ( 0, 'DC', iergbl, ' ', ierwlg )
		DO ii = 1, NBUFRT 
		    CALL CLOSBF  ( iubfmf (ii) )
		END DO
		CALL FL_CLAL ( iercal ) 
	      RETURN
	  END IF
C
C*  Decode the header information from this bulletin.
C
	  IF  ( ifdtyp .ne. 0 )  THEN
C
C*  Do not decode AFOS products.
C
	    bullok = .false.
	  ELSE
C
C*  Decode WMO products.  Initialize the arrays.
C
	    nrept = 0
	    naircft = 0
	    bullok = .true.
	    DO na = 1, NACRN
	      iacrn( na ) = IMISSD
	      selv( na ) = RMISSD
	      acrn( na ) = ' ' 
	      stat( na ) = '--' 
	      coun( na ) = '--' 
	      nmlev( na ) = 0 
	      flat( na ) = RMISSD
	      flon( na ) = RMISSD
	      DO nl = 1, nlv
		poaf( na, nl ) = RMISSD
		alat( na, nl ) = RMISSD
		alon( na, nl ) = RMISSD
		wspd( na, nl ) = RMISSD
		wdir( na, nl ) = RMISSD
		hmdt( na, nl ) = RMISSD
		tmpk( na, nl ) = RMISSD
		hght( na, nl ) = RMISSD
		pres( na, nl ) = RMISSD
		afdate(na, nl) = ' '
	      END DO
	    END DO
C
C*  Decode WMO products.
C
	    CALL DC_GHDR  ( bull, lenb, seqnum, buhd, cborg,
     +                          bulldt, bbb, ibptr, ierghd )
	    IF  ( ierghd .ne. 0 )  THEN
	      CALL DC_WLOG  ( 2, 'DC', ierghd, ' ', ierwlg )
	      bullok = .false.
	    ELSE
C
C*  Start an entry for this bulletin in the decoder log.
C
	          logmsg = '####################' //
     +			   '####################'
	      CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )
	      logmsg = seqnum // buhd // cborg // bulldt // bbb
	      CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )
	    END IF
	  END IF
C
	  IF  ( bullok )  THEN
C
C*  Get the system time.
C
	    itype = 1
	    CALL CSS_GTIM  ( itype, sysdt, iergtm )
	    IF  ( iergtm .ne. 0 )  THEN
	      CALL DC_WLOG  ( 2, 'SS', iergtm, ' ', ierwlg )
	      bullok = .false.
	    END IF
	  END IF
C
	  IF  ( bullok )  THEN
C
C*  If a date-time was entered on the command line, then
C*  use it as the run date-time.  Otherwise, use the
C*  system time as the run date-time.
C
	    IF  ( cldt .eq. 'SYSTEM' )  THEN
	      rundt = sysdt
	    ELSE
	      CALL TI_STAN  ( cldt, sysdt, rundt, ierstn )
	      IF  ( ierstn .ne. 0 )  THEN
	        CALL DC_WLOG  ( 2, 'TI', ierstn, ' ', ierwlg )
	        bullok = .false.
	      END IF
	    END IF
	  END IF
C
	  IF  ( bullok )  THEN
C
C*  Convert the run date-time to integer.
C
	    rundt = rundt(1:9) // '00'
	    CALL TI_CTOI  ( rundt, irundt, ierctoi )
	    IF  ( ierctoi .ne. 0 )  THEN
	      CALL DC_WLOG  ( 2, 'TI', ierctoi, ' ', ierwlg )
	      bullok = .false.
	    END IF
	  END IF
C
	  IF ( bullok ) THEN
C
C*  Locate the next BUFR message within the bulletin,
C*  and store it within an equivalenced integer array.
C
	    ipt1 = INDEX ( bull ( ibptr : lenb ), 'BUFR' )
	    IF  ( ipt1 .eq. 0 )  THEN
	      bullok = .false.
	    ELSE
	      istart = ibptr + ipt1 - 1
	      cbull = bull ( istart : lenb )
	    END IF
	  END IF
C
          IF ( bullok ) THEN
C
C*  Determine which BUFR tables file this message
C*  corresponds to.  Retrieve the Section 3 descriptors
C*  from this TAMDAR BUFR message and compare it with
C*  the list of expected descriptors.
C
		ii = 1
		bullok = .false.
		DO WHILE  (  ( .not. bullok ) .and.
     +				( ii .le. NBUFRT )  )
		    CALL UT_CBS3  ( 4, ibull, cxdsc ( 1, ii ),
     +				    nxdsc ( ii ), iercs3 )
		    IF  ( iercs3 .eq. 0 )  THEN
			bullok = .true.
		    ELSE
			ii = ii + 1
		    END IF
		END DO
	  END IF
C
	  IF ( bullok ) THEN
		logmsg = 'message read using ' // bufrtf(ii)
		CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )
C
C*  Open the TAMDAR BUFR message.
C
		CALL READERME  ( ibull, iubfmf (ii),
     +				 bfstyp, ibfdt, ierrme )
		IF  ( ierrme .ne. 0 )  THEN
		    bullok = .false.
		ELSE
C
C*  Check update sequence number in Section 1 to see 
C*  if correction is indicated.  If this bulletin is 
C*  a correction, 'corn' will be set to a non-zero 
C*  value so CORN may be set within TA_BUFR.
C
		    corn = FLOAT ( IUPBS01 ( ibull, 'USN' ) )
		    IF ( corn  .gt. 0.0 ) corn = 1.0
		    nrept = 0
		END IF
	  ELSE
		logmsg = 'message has unknown format'
		CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )
	  END IF
C
	  DO WHILE  ( bullok )
C
C*  Get the next report from this BUFR message.
C
	    CALL READSB  ( iubfmf(ii), ierrsb )
	    IF  ( ierrsb .ne. 0 )  THEN
C
C*  There are no more reports in this message.
C
	      bullok = .false.
C
C*  Print a count of the number of reports processed.
C
	      WRITE  ( UNIT = logmsg, FMT = '( A, I4, A )' )
     +		'contained ', nrept, ' reports'
	      CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )

C
C*  READSB - ierrsb = 0...
C
	    ELSE
	      nrept = nrept + 1
C
C*  Initialize the interface arrays.
C
	      CALL TA_IFIV  ( ier_ifiv )
C
C*  Decode the report into the interface arrays.
C
	      CALL TA_BFIF  ( iubfmf (ii) , ierbif )
C
C*  Write data for this report to the decoder log.
C
	      CALL TA_IFPT  ( rimnem, cimnem, ier_ifpt )
C
C*  Do not create output for reports that are more than
C*  NHOURS before or more than 3 hours after the run time.
C
	      IF  (  ( ERMISS ( rivals( iryear ) ) ) .or.
     +			 ( ERMISS ( rivals( irmnth ) ) ) .or.
     +			 ( ERMISS ( rivals( irdays ) ) ) .or.
     +			 ( ERMISS ( rivals( irhour ) ) ) .or.
     +			 ( ERMISS ( rivals( irminu ) ) )  ) THEN
		    iertmk = -1
	      ELSE
		irptdt(1) = INT ( rivals( iryear ) )
		irptdt(2) = INT ( rivals( irmnth ) )
		irptdt(3) = INT ( rivals( irdays ) )
		irptdt(4) = INT ( rivals( irhour ) )
		irptdt(5) = INT ( rivals( irminu ) )
		CALL DC_TMCK  ( 2, irundt, irptdt, nhours,
     +			    180, iertmk )
	      END IF
	      IF  ( iertmk .eq. 0 )  THEN
C
C*  Construct the aircraft id from aircraft number
C*  by appending flight phase character:
C*  - 'A' for ascending plane
C*  - 'D' for descending plane
C
		CALL ST_NUMB (civals ( icacrn ), icurra, iret)
		IF ( rivals (irpoaf) .eq. 5 ) THEN
		  cracrn = civals ( icacrn )(2:) // 'A'
		ELSE IF ( rivals (irpoaf) .eq. 6 ) THEN
		  cracrn = civals ( icacrn )(2:) // 'D'
		ELSE 
		  cracrn = ' '
		ENDIF
		amatch = .false.
		ia    = 1
		iaircft = 0
C
C*  Start loop over the planes in the bulletin.
C
		DO WHILE ( .not. amatch )
		  IF ( ( cracrn .eq. acrn(ia) ) .and.
     +                 ( nmlev(ia) .ne. 0 ) ) THEN
C
C*  This is an existing plane - increment the number of levels. 
C
		    amatch = .true.
		    nmlev(ia) = nmlev(ia) + 1
		    iaircft = ia
		  ELSE IF ( ( cracrn .ne. acrn(ia) ) .and.
     +                           ( rivals(irpoaf) .ne. RMISSD ) .and. 
     +                           ( nmlev( ia ) .eq. 0 ) ) THEN
C
C*  This is a new plane - increment the total
C*  number of aircrafts and construct the aircraft number.
C
		    naircft = naircft + 1
		    acrn(naircft) = cracrn
		    IF ( cracrn(8:8) .eq. 'D') THEN
		      ipoaf = 6
		    ELSE IF (cracrn(8:8) .eq. 'A') THEN
		      ipoaf = 5
		    END IF
		    iacrn(naircft) = ipoaf*100000 + icurra
		    nmlev(naircft) = 1
		    iaircft = naircft
		    amatch = .true.
		  END IF
		  ia = ia + 1
		  IF ( ia .gt. NACRN ) amatch = .true.
		END DO
C
C*  Decode the records into the individual arrays.
C
		IF ( iaircft .gt. 0 ) THEN
		  il = nmlev(iaircft)
		  alat(iaircft, il) = rivals(irslat)
		  alon(iaircft, il) = rivals(irslon)
		  wspd(iaircft, il) = rivals(irwspd)
		  wdir(iaircft, il) = rivals(irwdir)
		  hght(iaircft, il) = rivals(irhmsl)
		  pres(iaircft, il) = rivals(irprlc)/100.
		  tmpk(iaircft, il) = rivals(irtmdb)
		  hmdt(iaircft, il) = rivals(irrehu)
		  CALL TI_ITOC ( irptdt, cdum, iret)
		  afdate(iaircft, il) = cdum
		END IF
	      END IF
C
		  logmsg = '-----------------------------------'
		  CALL DC_WLOG  ( 3, 'DC', 2, logmsg, ierwlg )
	    END IF

C
C*  END DO for DO WHILE ( bullok );
C*  looping over reports in a single BUFR message...
C
	  END DO

C
C======================================================================
C
C*  Write out the data for each aircraft for this BUFR message...
C
	  DO ia = 1, naircft
	    IF ( acrn(ia)(8:8) .eq. 'A' ) THEN
	      bot = 1
	      top = nmlev(ia)
	      inc = 1
	    ELSE
	      bot = nmlev(ia)
	      top = 1
	      inc = -1
	    END IF

C
C*  Create a "Nominal Time" of observation.
C
	    dattim = afdate(ia,bot)(1:9) // '00'

C
C*  Create a unique "observation ID" from time and aircraft ID.
C
	    acrn(ia) = dattim(8:9) // acrn(ia)(3:8)

C
C*  Make a file name from the template and the aircraft report time.
C
	    CALL FL_MNAM ( dattim(1:6), gemfil, filnam, ier )

C
C*  Check for the requested upper air data file.
C
	    CALL FL_INQR ( filnam, exist, newfil, ier )
	    IF  ( exist )  THEN

C
C*  Open the merged upper air data file.
C
	      CALL SN_OPNR ( filnam, isnfln, ifsc, nparm, parms, ivert,
     +				mrgdat, ieropnr )

C
C*  Check that the file was opened properly.
C
	      IF  ( ieropnr .ne. 0 )  THEN

C
C*  If not, write an error to the decoder log file.
C
	        CALL DC_WLOG ( 0, 'SN', ierr, filnam, iret )
	        RETURN
	      ELSE

C
C*  Check for existence of time/aircraft in file; if not present
C*  add the time/aircraft; if present, has data been added?
C
		CALL TA_TMST ( isnfln, dattim , acrn(ia),
     +				iacrn(ia), alat(ia,bot), alon(ia,bot),
     +				RMISSD, addstn, cirflg, datflg, ier_tmst)
		IF ( ier_tmst .eq. 0 ) THEN
		  IF ( datflg ) THEN

C
C*  Data already present in sounding file, read it in.
C
		    CALL SN_RDAT(isnfln, nlevi, datai, ihhmm, ier)
		    nlevo = nlevi + nmlev(ia)
		    IF ( acrn(ia)(8:8) .eq. 'A' ) THEN

C
C*  Ascending aircraft so can insert new data at the end of datai.
C
		      ii = nlevi * NPARMS
		      DO il = 1, nmlev(ia) 
			datai(ii+1) = pres(ia,il)
			datai(ii+2) = tmpk(ia,il)
			datai(ii+3) = hmdt(ia,il)

C
C*  It appears that wdir equal to 360.0 is reserved for missing...
C
			IF ( wdir(ia,il) .eq. 360.0 )  THEN
			  datai(ii+4) = RMISSD
			  datai(ii+5) = RMISSD
			ELSE
			  datai(ii+4) = wdir(ia,il)
			  datai(ii+5) = wspd(ia,il)
			END IF
			datai(ii+6) = hght(ia,il)
			ii = ii + 6
		      END DO
		      CALL SN_WDAT(isnfln, ihhmm, nlevo, datai, ier)
		    ELSE

C
C*  Descending aircraft so must put new data at head of datai.
C*  Use datao for the output data.
C
		      ii = 0
		      DO il = bot, top, inc
			datao(ii+1) = pres(ia,il)
			datao(ii+2) = tmpk(ia,il)
			datao(ii+3) = hmdt(ia,il)

C
C*  It appears that wdir equal to 360.0 is reserved for missing...
C
			IF ( wdir(ia,il) .eq. 360.0 )  THEN
			  datao(ii+4) = RMISSD
			  datao(ii+5) = RMISSD
			ELSE
			  datao(ii+4) = wdir(ia,il)
			  datao(ii+5) = wspd(ia,il)
			END IF
			datao(ii+6) = hght(ia,il)
			ii = ii + 6
		      END DO
		      kk = 1
		      DO jj = nmlev(ia)*NPARMS+1, nlevo*NPARMS
			datao(jj) = datai(kk)
			kk = kk +1
		      END DO
		    END IF

C
C*  ELSE for "IF ( datflg ) THEN"
C
		  ELSE

C
C*  New aircraft for this time period, just add it...
C
		    CALL TI_CTOI(afdate(ia,bot), iarcft, ierctoi )
		    ihhmm = 100*iarcft(4) + iarcft(5)
		    ii = 0
		    DO il = bot, top, inc 
		      datao(ii+1) = pres(ia,il)
		      datao(ii+2) = tmpk(ia,il)
		      datao(ii+3) = hmdt(ia,il)

C
C*  It appears that wdir equal to 360.0 is reserved for missing...
C
		      IF ( wdir(ia,il) .eq. 360.0 )  THEN
		        datao(ii+4) = RMISSD
		        datao(ii+5) = RMISSD
		      ELSE
		        datao(ii+4) = wdir(ia,il)
		        datao(ii+5) = wspd(ia,il)
		      END IF
		      datao(ii+6) = hght(ia,il)
		      ii = ii + 6
		    END DO
		    CALL SN_WDAT(isnfln, ihhmm, nmlev(ia), datao, ier)
		  END IF

C
C*  END IF for "IF ( iertmst .eq. 0 ) THEN"
C
		END IF
		CALL SN_CLOS(isnfln, ierclos)

C
C*  END IF for "IF  ( ieropnr .ne. 0 )  THEN"
C
	      END IF

C
C*  ELSE for "IF  ( exist )  THEN"
C
	    ELSE

C
C*  File does NOT exist, create the merged upper air data file.
C
	      CALL SN_CRFP ( filnam, prmfil, iflsrc, maxstn, maxtim,
     +               stmflg, isnfln, nparm, parms, pkflg, iercrfp )

C
C*  Check that the file was created properly.
C
              IF  ( iercrfp .ne. 0 )  THEN

C
C*  If not, write an error to the decoder log file.
C
                CALL DC_WLOG ( 0, 'SN', ierr, filnam, iret )
                RETURN
	      ELSE

C
C*  Add station to the data file.
C
		CALL TA_TMST ( isnfln, dattim, acrn(ia),
     +				iacrn(ia), alat(ia,bot), alon(ia,bot),
     +				RMISSD, addstn, cirflg, datflg, iertmst )
		IF ( iertmst .eq. 0 ) THEN
		  CALL TI_CTOI(afdate(ia,bot), iarcft, ierctoi )
		  ihhmm = 100*iarcft(4) + iarcft(5)
		  ii = 0
		  DO il = bot, top, inc 
		    datao(ii+1) = pres(ia,il)
		    datao(ii+2) = tmpk(ia,il)
		    datao(ii+3) = hmdt(ia,il)

C
C*  It appears that wdir equal to 360.0 is reserved for missing...
C
		    IF ( wdir(ia,il) .eq. 360.0 )  THEN
		      datao(ii+4) = RMISSD
		      datao(ii+5) = RMISSD
		    ELSE
		      datao(ii+4) = wdir(ia,il)
		      datao(ii+5) = wspd(ia,il)
		    END IF
		    datao(ii+6) = hght(ia,il)
		    ii = ii + 6
		  END DO
		  CALL SN_WDAT (isnfln, ihhmm, nmlev(ia), datao, ierwdat)
		END IF
		CALL SN_CLOS(isnfln, ierclos)
	      END IF

C
C*  END IF for "IF  ( exist )  THEN"
C
	    END IF

C
C*  END DO for "DO ia = 1, naircft" 
C
	  END DO

C
C*  END DO for "DO WHILE ( .true. )"; looping over BUFR bulltins...
C
	END DO
C
	RETURN
	END		
