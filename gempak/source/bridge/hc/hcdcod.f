	SUBROUTINE HC_DCOD  ( curtim, gemfil, stntbl, prmfil, 
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* HC_DCOD								*
C*									*
C* This subroutine decodes tropical depression, tropical storm and      *
C* hurricane forecast/advisory bulletins and writes the data to an ASCII*
C* file.								*
C*									*
C* HC_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
C*	      IRET )    						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	PRMFIL		CHAR*		Parameter table file name	*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC          9/99   					*
C* D. Kidwell/NCEP	 3/00	Added TEST bull. chk; HC_RTIM -> DC_ITIM*
C* A. Hardy/GSC		 5/00   Added ocean storm;wind radii and sea ft *
C* A. Hardy/GSC		 8/00   Added correction flag check to HC_DECD  *
C* D. Kidwell/NCEP	 7/01	Added fcst storm type and 34kt fcst     *
C*				radii at 24, 48, 72 hrs; cleaned up     *
C* A. Hardy/SAIC	 8/01   Added flag for extratropical storm; 	*
C* 				increased f34kt array 3 -> 5		*
C* D. Kidwell/NCEP 	 3/02	Added f50kt & f64kt for all fcst times  *
C* D. Kidwell/NCEP 	 2/03	Added isbflg to HC_GHDR and HC_OUT calls*
C* D. Kidwell/NCEP 	 2/03	Allowed for 7 fcst times; added NFCST   *
C* A. Hardy/NCEP	10/03	Increase posnm *2 -> *3			*
C* A. Hardy/NCEP	11/03	Added lenbul to HC_GHDR			*
C* A. Hardy/NCEP	12/03	Added 'curtim' to HC_GHDR		*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* D. Kidwell/NCEP 	 5/04	Fixed year for ocnstm in JTWC reports   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, prmfil, stntbl
C*
	PARAMETER	( NFCST = 7 )
	CHARACTER	bultin*(DCMXBF), parms(MMPARM)*4,
     +			filnam*132, errstr*80, dattim*11, sysdt*12, 
     +                  dattmp*12, sname*15, advnum*4, stype*10, 
     +                  posnm*3, direc*3, speed*3, strmtim*13,
     +                  temptim*7, time*6, minpres*5, ocnstm*6, 
     +                  sixty*50, fifty*50, thirty*50, seaft*40,
     +			f34kt(20)*50, datadv*11, fstype(20)*2,
     +			f50kt(20)*50, f64kt(20)*50
        INTEGER         istarr (5), irdtar (5), itype
        REAL            flat(20), flon(20)
	LOGICAL		more
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
		more = .true.
		CALL DC_TEST ( bultin, lenbul, itest, ier )
C
C*		Parse the header info from the bulletin.
C
 		CALL HC_GHDR ( bultin, lenbul, curtim, stype, sname, 
     +                         advnum, time, ocnstm, icor, datadv, 
     +                         isbflg, iterr )
                IF ( iterr .lt. 0 ) more = .false.
C
                IF ( more ) THEN
		    icor = icor + itest
C
C*                  Get the system time, and make a standard GEMPAK time
C*                  from the "current" time.
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
C*		    Set the GEMPAK date/time.
C
                    CALL ST_INTG ( time, issue, iret )
                    irday  = issue / 10000
                    irhour = mod ( issue, 10000 ) / 100
                    irmin  = mod ( mod ( issue, 10000 ), 100 )

                    CALL DC_ITIM ( istarr, irday, irhour, irmin,
     +                             irdtar, iret )

		    CALL TI_ITOC ( irdtar, dattim, iret )
                    IF ( iret .lt. 0 ) more = .false.
C
C*		    Remove control characters from entire bulletin.
C
		    IF ( more ) THEN 
		        CALL ST_UNPR ( bultin, lenbul, bultin, lenb, 
     +				       ierr )
		        IF ( ierr .ne. 0 ) THEN
		            CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		            CALL ST_UNPR ( bultin(:72), 72, errstr, 
     +					   len1, ier )
		            CALL DC_WLOG ( 2, 'DCWARN', 2, errstr, ier )
		            more = .false.
		        END IF
C
C*		        Make a file name from the template and the time.
C*		        Open the file as part of the open file list.
C
 		        CALL FL_MNAM ( dattim, gemfil, filnam, ier )
 		        CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, 
     +		                   maxtim, lunf, nparm, parms, ierr )
C
C*	                Check that the file was opened properly.
C
		        IF ( ierr .ne. 0 ) THEN
C
C*	    	            If not, write an error to the decoder 
C*			    log file.
C
	    	            CALL DC_WLOG ( 0, 'HC', ierr, filnam, iret )
                            more = .false.
		        END IF
		        IF ( more ) THEN
C
C*		        Sending the report to be decoded.
C
 	                    CALL HC_DECD ( bultin, lenbul, NFCST,
     +					   temptim, rlat, rlon, posnm,
     +				           direc, speed, minpres,
     +					   sixty, fifty, thirty, seaft,
     +                                     flat, flon, icind, fstype,
     +                                     f34kt, f50kt, f64kt, iexflg,
     +                                     iret )
			    IF ( iterr .eq. 0 ) THEN
				strmtim = datadv
			      ELSE
C
C*				Try to get the time from the report.
C
 	                        CALL HC_CTIM ( istarr, temptim, strmtim, 
     +					       iret )
C
C*				Get the year for JTWC reports.
C
				IF ( ( iret .eq. 0 ) .and.
     +				     ( ocnstm ( 2:2 ) .eq. 'P' ) .and.
     +				     ( ocnstm ( 5:6 ) .eq. ' ' ) )
     +				     ocnstm ( 5:6 ) = strmtim ( 1:2 )
			    END IF
                            IF ( icor .eq. itest ) icor = icind + itest
                          ELSE
			    more = .false.
                        END IF
C
C*                      Check for missing data.
C
                        IF ( (stype .eq. ' ') .or. (strmtim .eq. ' ') 
     +                     .or. ( rlat .eq. RMISSD ) .or. 
     +                          ( rlon .eq. RMISSD ) .or.
     +                          ( ( ocnstm .eq. ' ') .and. 
     +                          ( sname .eq. ' ') ) ) more = .false.
                        IF ( more ) THEN 
                            CALL HC_OUT (lunf, stype, sname, strmtim, 
     +                                   ocnstm, advnum, rlat, rlon, 
     +                                   posnm, direc, speed, minpres, 
     +					 icor, sixty, fifty, thirty, 
     +                                   seaft, flat, flon, fstype, 
     +					 f34kt, f50kt, f64kt, iexflg,
     +					 isbflg, NFCST, iret )
                        END IF
                    END IF
                END IF 
 	        CALL DC_FCLS ( ier )
	    END IF
	END DO
C*
	RETURN
	END
