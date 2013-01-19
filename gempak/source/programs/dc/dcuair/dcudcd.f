	SUBROUTINE DCUDCD ( curtim, gemfil, stntbl, iadstn, maxtim,
     +			    nhours, txtflg, iwndht, iret )
C************************************************************************
C* DCUDCD								*
C*									*
C* This routine will decode Upper Air Radiosonde bulletins and write	*
C* the data to a GEMPAK sounding file.					*
C*									*
C* DCUDCD ( CURTIM, GEMFIL, STNTBL, IADSTN, MAXTIM, NHOURS, TXTFLG,	*
C*	    IWNDHT, IRET )						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*					  to decode			*
C*	TXTFLG		LOGICAL		Flag to save undecoded text	*
C*	IWNDHT		INTEGER		Dropsonde wind cutoff value	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 9/95						*
C* S. Jacobs/NCEP	10/95	Updated call to DC_GHDR			*
C* S. Jacobs/NCEP	 1/96	Updated call to DC_GHDR			*
C* S. Jacobs/NCEP	 1/96	Added error message after DC_GHDR	*
C* S. Jacobs/NCEP	 4/96	Added decoding of AFOS products		*
C* D. Keiser/GSC	 4/96	Add iflsrc to call sequence of DC_FCYL	*
C* S. Jacobs/NCEP	 7/96	Changed calling sequence		*
C* K. Tyle/GSC		 1/97	Changed DC_WLOG calls;			*
C*				eliminated call to IN_BDTA		*
C* D. Kidwell/NCEP	10/98	Added check for bad observation time    *
C* D. Kidwell/NCEP	 2/01	Added dup. report check, text output    *
C* D. Kidwell/NCEP	 3/01	Changed RU_SHDR, RU_DECD call sequences;*
C*				checked for 1-hour interval             *
C* S. Chiswell/Unidata	 5/01	Added stid to RU_SHDR, RU_DECD, RU_DUPE	*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* m.gamazaychikov/SAIC	07/05	Added code for dropsonde wind cutoff hgt*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl
	LOGICAL		txtflg
C*
	CHARACTER	prmfil*72, parms(MMPARM)*4
C*
	CHARACTER	bultin*(DCMXBF), report*(DCMXBF),
     +			seqnum*4, wmorpt*8, oristn*8, btime*12, bbb*8,
     +			sysdt*12, dattmp*12, filnam*132, errstr*80,
     +			part*4, dattim*15, pilhdr*12, stid*8
	INTEGER		istarr (5), iotarr (5), itype
        REAL		data (3, LLMXLV)
	LOGICAL		more, good, dupchk, zwind
C------------------------------------------------------------------------
	IF ( .not. txtflg ) THEN
	    iflsrc = MFRAOB
	  ELSE
	    iflsrc = MFRAOB + MFTEXT
	END IF
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file to 3 for unmerged sounding.
C
	maxfil = 2
	iftype = 3
	prmfil = ' '
	CALL DC_FINT ( maxfil, iftype, prmfil, iperr )
C
C*	Loop until a timeout occurs.
C
	DO WHILE  ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    ibpnt = 1
	    IF  ( iperr .eq. 0 )  THEN
C
C*		Parse the header info from the bulletin.
C
		more   = .true.
		dupchk = .true.
		IF  ( ifdtyp .eq. 0 )  THEN
		    CALL DC_GHDR  ( bultin, lenbul, seqnum, wmorpt,
     +				    oristn, btime, bbb, nchar, ierr )
		    IF ( ( bbb ( :1 ) .eq. 'C' ) .or.
     +		         ( bbb ( :1 ) .eq. 'R' ) .or.
     +		         ( bbb ( :1 ) .eq. 'A' ) ) THEN
			dupchk = .false.
		    END IF
		  ELSE
		    CALL DC_GPIL  ( bultin, lenbul, pilhdr, wmorpt,
     +				    oristn, btime, nchar, ierr )
		END IF
		IF  ( ierr .ne. 0 )  THEN
		    CALL DC_WLOG ( 1, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 1, 'DCUAIR', 2, errstr, ier )
		    more = .false.
		END IF
C
C*		Get the system time, and make a standard GEMPAK time
C*		from the "current" time.
C
		itype = 1
		CALL CSS_GTIM  ( itype, sysdt, ier )
		IF  ( curtim .eq. 'SYSTEM' )  THEN
		    dattmp = sysdt
		  ELSE
		    CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		END IF
		CALL TI_CTOI ( dattmp, istarr, ier )
C
C*		Remove unprintable characters.
C
		CALL ST_UNPR ( bultin, lenbul, bultin, lennew, ier )
		lenbul = lennew
C
C*		Loop through reports.
C
		DO WHILE  ( more )
C
C*		    Get next report.
C
		    CALL RU_GRPT ( bultin, lenbul, ibpnt,
     +				   report, lenr, iret )
		    irpnt = 1
		    good  = .true.
		    IF  ( iret .ne. 0 )  THEN
			more = .false.
			good = .false.
		      ELSE
C
C*			Decode station header
C
			CALL RU_SHDR  ( report, lenr, irpnt, part,
     +					istnm,  stid, iday, ihour, 
     +					wnknot, itopwn, xlat, xlon, 
     +				        iret )
			IF  ( iret .ne. 0 )  good = .false.
		    END IF
C
C*		    Get the time to assign to this bulletin.
C
		    IF  ( good )  THEN
C
C*			Get observation time.
C
			CALL RU_RTIM  ( istarr, iday, ihour, iotarr,
     +					ier )
			IF ( ier .eq. 0 ) THEN
			    ihhmm = iotarr (4) * 100
C
C*			    Compute difference between observation and
C*			    system times.
C
			    CALL TI_MDIF ( iotarr, istarr, imdif, ier1 )
C
C*			    Adjust the time to the nearest three hours, 
C*			    unless hourly reports have been requested.
C
			    IF ( maxtim .ne. 24 ) THEN
			        CALL RU_ADJT  ( iotarr, dattim, ier2 )
			      ELSE
				CALL TI_ITOC  ( iotarr, dattim, ier2 )
			    END IF
C
C*			    Check that the time is within NHOURS before
C*			    the system time.
C
			    IF  ( ( ier1 .ne. 0 ) .or.
     +			          ( ier2 .ne. 0 ) .or.
     +			          ( imdif .gt. 90 ) .or.
     +			          ( imdif .lt. ((-60)*nhours) ) )
     +		                  good = .false.
			  ELSE
			    good   = .false.
			    dattim = ' '
			    imdif  = IMISSD
			END IF
			IF ( .not. good ) THEN
C
C*			    Write an error message if the time is
C*			    invalid.
C
			    errstr = 'INVALID BULLETIN: ' //
     +				     wmorpt // oristn // btime
			    CALL DC_WLOG ( 2, 'DCUAIR', 2, errstr, ier )
			    WRITE (errstr,1000) 'INVALID DATTIM: ',
     +						dattim, dattmp,
     +						iday, ihour, imdif,
     +						istnm, report (1:16)
1000			    FORMAT ( A, A, A, I3, I3, I6, I6, 1X, A )
			    CALL DC_WLOG ( 0, 'DCUAIR', 2, errstr, ier )
			END IF
		    END IF
C
C*		    Open the output file.
C
		    IF  ( good )  THEN
C
C*			Make a file name from the template and the time.
C*			Open the file as part of the open file list.
C
			CALL FL_MNAM ( dattim, gemfil, filnam, ier )
			CALL DC_FCYL  ( filnam, iflsrc, stntbl, iadstn,
     +				      maxtim, lunf, nparm, parms, ierr )
C
C*			Check that the file was opened properly.
C
			IF  ( ierr .ne. 0 )  THEN
C
C*			    If not, write an error to the decoder
C*			    log file.
C
			    CALL DC_WLOG ( 0, 'SN', ierr,
     +					   filnam, iret )
			  ELSE
C
C*			    If the report is not a correction, 
C*			    retransmission or amendment, check to see
C*			    if this station, time and part are already 
C*			    in the output file.  If so, skip the report.
C
			    IF ( dupchk ) THEN
				CALL RU_DUPE ( lunf, dattim, istnm,
     +					       stid, part, ihhmm, 
     +					       nlev, data, zwind, ier )
			        IF ( ier .ne. 0 ) good = .false.
			    END IF
			    IF ( good ) THEN
C
C*			        Decode the bulletin, and write the data
C*			        to the output file.
C
			        CALL RU_DECD  ( lunf, dattim, istnm,
     +					        stid, part, itopwn,
     +					        wnknot, ihhmm, report,
     +					        lenr, txtflg, xlat,xlon,
     +						iwndht, irpnt, iret )
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
