	SUBROUTINE DCNMDC ( curtim, gemfil, prmfil, stntbl, iadstn,
     +			    maxtim, nhours, txtflg, iret )
C************************************************************************
C* DCNMDC								*
C*									*
C* This routine will decode an NGM MOS bulletin and write the data to a *
C* GEMPAK surface file.							*
C*									*
C* DCNMDC ( CURTIM, GEMFIL, PRMFIL, STNTBL, IADSTN, MAXTIM,		*
C*	    NHOURS, TXTFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	PRMFIL		CHAR*		Parameter packing table		*
C*	STNTBL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*					  to decode			*
C*	TXTFLG		LOGICAL		Flag to save undecoded text	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Keiser/GSC	 2/96	Copied from DCADCD			*
C* D. Keiser/GSC	 4/96	Add error checks, allow decoded	and	*
C*				text data to be written			*
C* S. Jacobs/NCEP	 4/96	Added decoding of AFOS products		*
C* S. Jacobs/NCEP	 7/96	Changed calling sequence		*
C* K. Tyle/GSC		 1/97	Changed DC_WLOG calls; eliminated call	*
C*				to IN_BDTA				*
C* F. J. Yen/NCEP	11/98	Cleaned up and restructured from DCAVCD *
C* D. Kidwell/NCEP	11/98	Cleaned up; removed unprintable chars   *
C*                              from text data                          *
C* F. J. Yen/NCEP	 8/99	Replaced RA_RTIM with DC_GTIM		*
C* F. J. Yen/NCEP	11/99	Changed error code group 'RA' to 'DC'   *
C* S. Chiswell/Unidata	 5/00	Add generation of WNUM			*
C* A. Hardy/SAIC        10/01   Cleaned up datflg check			*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INTEGER		FCSTTM
	PARAMETER       ( FCSTTM = 19 )
	PARAMETER       ( NUMNAM = 22 )
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
	LOGICAL		txtflg
C*
	CHARACTER	parms(MMPARM)*4, gemftm(FCSTTM)*15, txttim*15
	CHARACTER	bultin*(DCMXBF), report*(DCMXBF),
     +			seqnum*4, wmorpt*8, oristn*8, btime*12, bbb*8,
     +			sysdt*12, dattmp*12, filnam*132, errstr*80,
     +			dattim*15, stid*8, date*8, pilhdr*12
	REAL		rdata (FCSTTM,MMPARM), adata (MMPARM)
	INTEGER		istarr (5), irtarr (5), istart (NUMNAM)
	LOGICAL		more, good, addstn, cirflg, datflg, gettim,
     +			offtim
C*
	PARAMETER	( NUMPRM = 24 )
	PARAMETER	( NUMEXT = MMPARM - NUMPRM )
	CHARACTER	cprms (MMPARM)*4
	INTEGER		iprms (MMPARM), itype
	DATA		cprms / 'TNTF', 'TDYF', 'TMPF', 'DWPF', 'CLCT',
     +				'SKNT', 'DRCT', 'PP06', 'PP12',
     +				'QP06', 'QP12', 'TS06', 'TS12',
     +				'TC06', 'TC12', 'PCPT', 'POZP',
     +				'POSN', 'SN06', 'SN12', 'FCIG',
     +				'FVIS', 'OVIS', 'WNUM', NUMEXT * ' ' /
C------------------------------------------------------------------------
	iret = 0
C
C*	If the save text flag is set to true, set the file source to
C*	save both the decoded data and the undecoded text.
C
	IF  ( txtflg )  THEN
	    iflsrc = MFUNKN + MFTEXT
	  ELSE
	    iflsrc = MFUNKN
	END IF
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file to 1 for surface.
C
	maxfil = 2
	iftype = 1
	CALL DC_FINT ( maxfil, iftype, prmfil, iperr )
	addstn = .true.
	cirflg = .false.
C
C*	Loop until a timeout occurs.
C
	DO WHILE  ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF  ( iperr .ne. 0 )  THEN
C
C*		Write an error to the decoder log file.
C
		CALL DC_WLOG ( 0, 'DC', iperr, ' ', ier )
	      ELSE
C
C*		Parse the header from the bulletin.
C
		good = .true.
		IF ( ifdtyp .eq. 0 ) THEN
		    CALL DC_GHDR  ( bultin, lenbul, seqnum, wmorpt,
     +				    oristn, btime, bbb, nchar, ierr )
		  ELSE
		    CALL DC_GPIL  ( bultin, lenbul, pilhdr, wmorpt,
     +				    oristn, btime, nchar, ierr )
		END IF
		ibpnt = nchar + 1
		IF  ( ierr .ne. 0 )  THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCNMOS', 2, errstr, ier )
		    good = .false.
		END IF
C
		IF ( good ) THEN
C
C*		    Get the system time, and make a standard GEMPAK time
C*		    from the "current" time.
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
C*		    Get the date from the bulletin title line.
C
		    CALL MN_TITL ( bultin, ibpnt, date, irhour,
     +				   irmin, ierr )
		    IF  ( ierr .eq. 0 )  THEN
C
C*			Decode the forecast times. 
C
		        CALL MN_FCTM  ( bultin, date, ibpnt, gemftm, 
     +                                  ierr )
		        ihhmm = irhour * 100 + irmin

		      ELSE
		        good = .false.
		        CALL DC_WLOG ( 2, 'MN', ierr, ' ', ier )
		    END IF
		END IF
		IF  ( good )  THEN
C
C*		    Set off-time flag to false.
C
		    offtim = .false.
C
C*	            Get the time to assign to this bulletin.
C*		    Get observation time.
C
		    CALL DC_GTIM  ( istarr, btime, irhour, irmin,
     +				    offtim, irtarr, dattim, ier1 )
		    IF  ( ier1 .ne. 0 )  THEN
		        CALL DC_WLOG ( 2, 'DC', ier1, ' ', ier )
		    END IF
C
C*		    Compute difference between observation and system 
C*                  times.
C
		    CALL TI_MDIF  ( irtarr, istarr, imdif, ier2 )
C
C*		    Check that the time is within NHOURS before
C*		    the system time.
C
		    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +			      ( imdif .gt. 60 ) .or.
     +			      ( imdif .lt. ((-60)*nhours) ) ) THEN
		        good = .false.
C
C*		        Write an error message if the time is invalid.
C
			errstr = 'INVALID BULLETIN: ' //
     +				     wmorpt // oristn // btime
			CALL DC_WLOG ( 2, 'DCNMOS', 2, errstr, ier )
			WRITE (errstr,1000) 'INVALID DATTIM: ',
     +				        dattim, dattmp, imdif
1000			FORMAT ( A, A, A, I6 )
			CALL DC_WLOG ( 2, 'DCNMOS', 2, errstr, ier )
		    END IF
	        END IF
		IF (good) THEN
C
C*		    Skip to start of first report
C
		    irs = INDEX ( bultin ( ibpnt: ), CHRS )
		    ibpnt = ibpnt + irs - 1
		END IF
		more = good
C
C*		Loop through the reports.
C
		DO WHILE  ( more )
C
C*		    Get next report.
C
		    CALL MN_GRPT  ( bultin, lenbul, ibpnt,
     +				    report, lenr, irerr )
		    IF  ( irerr .ne. 0 )  THEN
			good = .false.
			more = .false.
C
C*			Write a message to the decoder log file.
C
			CALL DC_WLOG ( 4, 'MN', irerr, ' ', ier )
		      ELSE
		    	good = .true.
C
C*			Find the starting point of each line in the 
C*			report.
C
			CALL MN_GLIN  ( report, irhour, istart,
     +				        stid, ierr )
		    END IF
C
C*		    Open the output file.
C
		    IF  ( good )  THEN
C
C*			Make a file name from the template and the time.
C*			Open the file as part of the open file list.
C
			CALL FL_MNAM  ( dattim, gemfil, filnam, ier )
			CALL DC_FCYL  ( filnam, iflsrc, stntbl, iadstn,
     +				      maxtim, lunf, nparm, parms, ierr )
C
C*			Check that the file was opened properly.
C
			IF  ( ierr .ne. 0 )  THEN
C
C*			    If not, write an error to decoder log file.
C
			    CALL DC_WLOG  ( 0, 'SF', ierr, filnam, ier )
			    good = .false.
			  ELSE
C
C*			    Check for the parameters in the list.
C
			    DO  j = 1, NUMPRM
				CALL ST_FIND  ( cprms(j), parms, nparm,
     +						iprms(j), ier )
				IF  ( iprms (j) .eq. 0 )  THEN
				    iprms (j) = nparm + 1
				    CALL DC_WLOG  ( 2, 'DCNMOS',
     +						    -3, cprms(j), ier )
				END IF
			    END DO
			END IF
		    END IF
		    IF  ( good )  THEN
C
C*		    	Decode the data and write to the surface file.
C
			Call MN_DECD ( report, istart, iprms,
     +				       rdata, iderr)
			IF  ( iderr .eq. 0 )  THEN
			    CALL DCWNUM ( rdata, inumerr)
			    gettim = .true.
			    DO i = 1, FCSTTM 
C
C*				Set the station and time in the
C*				output file.
C
				CALL RA_TMST  ( lunf, gemftm(i), stid, 
     +						addstn,	cirflg, datflg, 
     +						ierr )
				IF ( gettim ) THEN
C
C*				    Get time to assign to text string.
C
				    IF ( ierr .ne. -4 ) THEN
					txttim = gemftm ( i )
					gettim = .false.
				    END IF
				END IF
C
C*				Check for an error.
C
				IF  ( ierr .ne. 0 )  THEN
				    good = .false.
				    errstr = ' '
				    IF  ( ierr .eq. -4 ) 
     +                                          errstr = gemftm ( i )
				    IF  ( ierr .eq. -5 ) errstr = stid
				    CALL DC_WLOG ( 2, 'RA', ierr,
     +						   errstr, ier )
C
C*				    If the data has already been decoded
C*				    write data regardless.
C
				  ELSE
				    good = .true.
				END IF
C
C*				Write the data to the output file. First
C*				load rdata into a one-dimensional array
C*				so that it can be passed into SF_WDAT.
C
				IF ( good ) THEN
				    DO j = 1, MMPARM
				    	adata(j) = rdata(i,j)
				    END DO	
				    CALL SF_WDAT ( lunf, ihhmm, adata,
     +						   iwerr)
				    IF ( iwerr .ne. 0 ) THEN
				    	CALL DC_WLOG ( 2, 'SF', iwerr,
     +							' ', ier )
				    END IF
				END IF
			    END DO
			    good = .true.
C
C*			    Set the station and time in the output file.
C
			    IF ( gettim ) txttim = dattim
			    CALL RA_TMST  ( lunf, txttim, stid, addstn,
     +					    cirflg, datflg, ierr )
			    IF ( ierr .ne. 0 ) THEN
				good = .false.
				errstr = ' '
				IF ( ierr .eq. -4 )  errstr = txttim
				IF ( ierr .eq. -5 )  errstr = stid
				CALL DC_WLOG ( 2, 'RA', ierr, errstr,
     +						ier )
			    END IF
C
C*			    Write the text data to the output file.
C
			    IF ( good ) THEN
				CALL ST_UNP1 ( report, lenr, report,
     +					       lenrnw, ierr )
			    	CALL SF_WSTR  ( lunf, ihhmm, report,
     +						iwserr )
			    	IF ( iwserr .ne. 0 ) THEN
				    CALL DC_WLOG ( 2, 'SF', iwserr, ' ',
     +						   ier )
			    	END IF
			    END IF
			END IF
		    END IF
		END DO
	    END IF
	END DO
C
	CALL DC_FCLS ( ier )
C*
	RETURN
	END
