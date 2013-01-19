	SUBROUTINE SI_DCOD  ( curtim, gemfil, stntbl, prmfil, 
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* SI_DCOD								*
C*									*
C* This subroutine decodes SCD bulletins and writes the data to a       *
C* GEMPAK surface file.							*
C*									*
C* SI_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
C*            IRET )		 					*
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
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* T. Lee/NCEP	 	 8/02	Based on SC_DCOD                        *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* D. Kidwell/NCEP	11/05	Improved error checks                   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	CHARACTER	bultin*(DCMXBF), report*(DCMXBF),
     +			sysdt*12, dattmp*12, filnam*132, errstr*80,
     +			dattim*15, stid*8, vtime*15, parms(2)*4, 
     +			bultim*12, bbb*8, wmohdr*8, oristn*8,
     +			fhr*8, vmdy*12, vhh*4, seqnum*4 
	INTEGER		istarr (5), itype 
        REAL		slat, slon
	LOGICAL		more, good, addstn
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file to 1 for surface, file source to
C*	1 for surface data.
C
	maxfil = 2
	iftype = 1 
	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
C
	addstn = .true.
	iflsrc = 1
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
C
C*		Parse the header info from the bulletin.
C
		more = .true.
		CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +		               oristn, bultim, bbb, nchar, ierr )
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCIDFT', 2, errstr, ier )
		    more = .false.
		END IF
C
C*	        Parse the header info specific for sea ice reports.
C
		IF  ( more )  THEN
		    CALL SI_GHDR  ( bultin, lenbul, nchar, 
     +				    fhr, vmdy, vhh, ierr )
		    IF  ( ierr .ne. 0 )  THEN
		        CALL DC_WLOG ( 2, 'SI', ierr, ' ', ier )
		        more = .false.
		    END IF
		END IF
		IF ( more ) THEN
C
C*		    Get the system time, and make a standard GEMPAK time
C*		    from the "current" time.  Array istarr will be used
C*		    as the receipt time.
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
C*		    Get the bulletin and verification (data) times. 
C
		    CALL SI_GTIM  ( istarr, bultim, vmdy, vhh, dattim,
     +				    vtime, ihhmm, ier1 )
C
C*		    Make a file name from the template and the time.
C*		    Open the file as part of the open file list.
C
		    CALL FL_MNAM ( dattim, gemfil, filnam, ier )
		    CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, 
     +			           maxtim, lunf, nparm, parms, ierr )
C
C*		    Check that the file was opened properly.
C
		    IF ( ierr .ne. 0 ) THEN
C
C*		        If not, write an error to the decoder log file.
C
			CALL DC_WLOG ( 0, 'DC', ierr, filnam, iret )
		        more = .false.
		    END IF
C
C*		    Point to the beginning of the data.
C
		    ibpnt = INDEX ( bultin, 'DIST(NM)' ) 
		    ibpnt = ibpnt + 15
		END IF
C
C*		Loop through the reports.
C
		DO WHILE ( more )
C
C*		    Get next report.
C
		    CALL SI_GRPT ( bultin, lenbul, ibpnt, stid, report,
     +				   lenr, iret )
		    good  = .true.
C
C*		    Check to see if a report was found.
C 
		    IF ( iret .ne. 0 ) THEN
			good = .false.
			IF ( iret .eq. -2 ) THEN
C
C*                          There are no more reports in this bulletin.
C
			    more = .false.
			END IF
		    END IF
C
C*		    Decode the data.
C
		    IF  ( good )  THEN
			CALL SI_DECD  ( report, lenr, stid, slat, slon,
     +					drct, dtnm, iret )
			IF  ( iret .ne. 0 )  THEN
			    good = .false.
			    errstr = report (:lenr)
			    CALL DC_WLOG ( 2, 'SI', iret, errstr, ier )
			END IF
		    END IF 		

C
C*		    Set the station and time in the output file.
C
		    IF ( good ) THEN
			CALL SI_TMST  ( lunf, vtime, stid, addstn, 
     +					slat, slon, iret )
C
C*			Check for an error.
C
			IF ( iret .ne. 0 ) THEN
			    good = .false.
			    errstr = ' '
			    IF ( iret .eq. -3 ) errstr = vtime
			    IF ( iret .eq. -4 ) errstr = stid
			    CALL DC_WLOG ( 2, 'SI', iret, errstr, ier )
			END IF
		    END IF
C
C*		    Write data to the surface file.
C
		    IF  ( good )  THEN
C
C*			Write report to GEMPAK file.
C
			IF ( good ) THEN 
			    CALL SI_GEMP ( lunf, ihhmm, parms, 
     +					   nparm, drct, dtnm, iret )
			    CALL SF_WSTR ( lunf, ihhmm, report, ier)
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
