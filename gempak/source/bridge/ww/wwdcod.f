	SUBROUTINE WW_DCOD  ( curtim, gemfil, stntbl, prmfil, 
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* WW_DCOD								*
C*									*
C* This subroutine decodes WWUS40 or WWUS30 tornado and severe          *
C* thunderstorm watch reports and WWUS8 or WOUS20 status reports and    *
C* writes the data to an ASCII file.					*
C*									*
C* WW_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
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
C* D. Kidwell/NCEP      5/99   	                          	        *
C* D. Kidwell/NCEP      5/99	Added replacement check      	        *
C* D. Kidwell/NCEP      7/99	WWUS9 -> WWUS40, remove replacement     *
C*				type, add COR check for bulletin        *
C* D. Kidwell/NCEP      8/99	Added check for status report (WWUS8)   *
C* D. Kidwell/NCEP      9/99	Added processing for 2nd status line    *
C* D. Kidwell/NCEP      3/00	Added check for TEST bulletins          *
C* D. Kidwell/NCEP      9/00	Increased MAXPTS from 8 to 50           *
C* D. Kidwell/NCEP     10/01	Added WWUS30 watch and WOUS20 status    *
C* D. Kidwell/NCEP     10/01	Added check for originator KWNS         *
C* B. Yin/SAIC          3/04    Changed SS_GTIM to CSS_GTIM             *
C* D. Kidwell/NCEP      9/04	Checked return code from DC_TEST        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	PARAMETER	( MAXPTS = 50 )
	CHARACTER	bultin*(DCMXBF), parms (MMPARM)*4,
     +			seqnum*4, wmohdr*8, oristn*8, bultim*12,
     +			bbb*8, errstr*80,
     +			sysdt*12, dattmp*12, filnam*132, 
     + 			wnum*4, strtim*20, stptim*20, tissue*20,
     +			rwnum (10)*4
	INTEGER		istarr (5), itime
	LOGICAL		good, status
	REAL		rlat (MAXPTS), rlon (MAXPTS)
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file and file source.
C
	maxfil = 2
	iftype = 6
 	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
	iflsrc = 2
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
		CALL DC_TEST ( bultin, lenbul, itest, ier )
C
C*		Check for a single occurrence of the word 'TEST'.
C
		IF ( ier .eq. 1 ) itest = 2
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
		    CALL DC_WLOG ( 2, 'DCWTCH', -1, errstr, ier )
		    good = .false.
		END IF
C
C*		Determine the type of bulletin - watch or status report.
C
		IF ( wmohdr ( :6 ) .eq. 'WWUS40' ) THEN
		    status = .false.
      		  ELSE IF ( ( wmohdr ( :6 ) .eq. 'WWUS30' ) .and.
     +			    ( oristn ( :4 ) .eq. 'KWNS' ) ) THEN
		    status = .false.
		  ELSE IF ( ( wmohdr ( :6 ) .eq. 'WWUS8 ' ) .or.
     +			    ( wmohdr ( :6 ) .eq. 'WWUS08' ) ) THEN
		    status = .true.
      		  ELSE IF ( ( wmohdr ( :6 ) .eq. 'WOUS20' ) .and.
     +			    ( oristn ( :4 ) .eq. 'KWNS' ) ) THEN
		    status = .true.
		  ELSE
		    good = .false.
	 	END IF
C
		IF ( good ) THEN
C
C*		    Set the bulletin pointer to one character past
C*		    the end of the WMO header.
C
		    ibpnt = nchar + 1
		    lenbul = lenbul - nchar
C
C*		    Get the system time, and make a standard GEMPAK time
C*		    from the "current" time.
C
		    itime = 1
		    CALL CSS_GTIM ( itime, sysdt, ier )
		    IF ( curtim .eq. 'SYSTEM' ) THEN
			dattmp = sysdt
		      ELSE
			CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		    END IF
		    CALL TI_CTOI ( dattmp, istarr, ier )
C
C*		    Decode the watch box or status report.
C
		    CALL WW_DECD ( bultin ( ibpnt: ), lenbul, istarr,
     +				   status, MAXPTS, itype, wnum, strtim, 
     +				   stptim, tissue, icorr, icancl, rlat, 
     +				   rlon, npt, irepl, rwnum, ier )
		    IF ( ier .eq. (-2) ) THEN
			CALL DC_WLOG ( 2, 'DCWTCH', ier, ' ', ierr )
			good = .false.
		    END IF
C
		    IF ( bbb ( :3 ) .eq. 'COR' ) icorr = 1
		    icorr = icorr + itest
		END IF
C
		IF ( good ) THEN
C
C*		    Make a file name from the template and the time.
C*		    Open the file as part of the open file list.
C
 		    CALL FL_MNAM ( tissue, gemfil, filnam, ier )
 		    CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, 
     +		                   maxtim, lunf, nparm, parms, ier )
		    IF ( ier .eq. 0 ) THEN
			CALL WW_OUT ( lunf, itype, strtim, stptim, wnum,
     +				   icorr, icancl, rlat, rlon, npt, ier )
		        IF ( .not. status ) THEN
C
C*			    If any watches were replaced by this watch,  
C*			    treat them as watch cancellations.
C
			    DO i = 1, irepl
			        CALL WW_OUT ( lunf, 2, strtim, strtim, 
     +					      rwnum ( i ), icorr, 1,
     +					      rlat, rlon, 0, ier )
			    END DO
			  ELSE
C
C*			    Look for second status report watch number
C*			    or second status line definition.
C
			    IF ( irepl .lt. 0 ) THEN
			        CALL WW_OUT ( lunf, 3, strtim, stptim, 
     +					      rwnum ( 1 ), icorr, npt,
     +					      rlat, rlon, npt, ier )
			      ELSE IF ( irepl .gt. 0 ) THEN
				nn = npt + 1
			        CALL WW_OUT ( lunf, 3, strtim, stptim, 
     +					wnum, icorr, irepl, rlat ( nn ),
     +				        rlon ( nn ), irepl, ier )
			    END IF
			END IF
		      ELSE
			CALL DC_WLOG ( 0, 'DC', ier, filnam, iret )
		    END IF
C
 		    CALL DC_FCLS ( ier )
		END IF
C
	    END IF
	END DO
C*
	RETURN
	END
