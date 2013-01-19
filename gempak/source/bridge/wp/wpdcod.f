	SUBROUTINE WP_DCOD  ( curtim, gemfil, stntbl, prmfil, 
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* WP_DCOD								*
C*									*
C* This subroutine decodes the WCP text products (WWUS60 KWNS).  WW_DCOD*
C* writes the data to an ASCII GEMPAK file.				*
C*									*
C* WP_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
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
C*					  0 = normal return		*
C*					 -4 = Unable to get bultim	*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/05 	                          	        *
C* D. Kidwell/NCEP	11/05	Improved error checks      	        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	CHARACTER	bultin*(DCMXBF), parms (MMPARM)*4,
     +			seqnum*4, wmohdr*8, oristn*8, bultim*12,
     +			bbb*8, errstr*80,
     +			dattim*11, sysdt*12, dattmp*12, filnam*132 
	INTEGER		istarr (5), irdtar (5)
	LOGICAL		good
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
		    CALL DC_WLOG ( 2, 'DCWCP', -1, errstr(:len1), ier )
		    good = .false.
		END IF
C
C*		Check for proper headers.
C
		IF ( ( wmohdr ( :6 ) .ne. 'WWUS60' ) .or.
     +			    ( oristn ( :4 ) .ne. 'KWNS' ) ) THEN
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
C*		    Set the GEMPAK date/time.  Use bultim to set the
C*		    output file name.	
C
		    CALL ST_LSTR ( bultim, len, ier )
		    IF ( len .ne. 6 ) THEN
			IF ( curtim .eq. 'SYSTEM' ) THEN
                            bultim = dattmp ( 5:6 ) // dattmp ( 8:11 )
                          ELSE
                            iret = -4
                            CALL ST_UNPR ( bultin (:80), 80, errstr,
     +                                  len1, ierr )
                            CALL DC_WLOG ( 2, 'DCWCP', iret,
     +                                  errstr (:len1), ierr )
			    good = .false.
                        END IF
                    END IF
		    IF ( good ) THEN
                        CALL ST_NUMB ( bultim, issue, ier )
                        irday  = issue / 10000
                        irhour = mod ( issue, 10000 ) / 100
                        irmin  = mod ( mod ( issue, 10000 ), 100 )
C
                        CALL DC_ITIM ( istarr, irday, irhour, irmin,
     +                             irdtar, ier )
C
C*                      If invalid values in issue time, use system or
C*                      current time.
C
                        CALL TI_DAYM ( irdtar (1), irdtar (2), ndays,
     +                          ier )
                        IF ( irday  .lt. 1 .or. irday  .gt. ndays .or.
     +                       irhour .lt. 0 .or. irhour .gt. 23    .or.
     +                       irmin  .lt. 0 .or. irmin  .gt. 59 )  THEN
                            irday  = istarr (3)
                            irhour = istarr (4)
                            irmin  = istarr (5)
                        END IF
                        CALL DC_ITIM ( istarr, irday, irhour, irmin,
     +                             irdtar, ier )
                        CALL TI_ITOC ( irdtar, dattim, ier )
                        IF ( ier .eq. 0 ) THEN
C
C*                          Set bultim from dattim, in case original
C*                          value
C*                          was invalid.
C
                            bultim = dattim ( 5:6 ) // dattim ( 8:11 )
			  ELSE
			    good = .false.
			END IF
		    END IF
		END IF
C
		IF ( good ) THEN
C
C*		    Make a file name from the template and the time.
C*		    Open the file as part of the open file list.
C
 		    CALL FL_MNAM ( dattim, gemfil, filnam, ier )
 		    CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, 
     +		                   maxtim, lunf, nparm, parms, ier )
		    IF ( ier .eq. 0 ) THEN
C
C*		        Decode the WCP Watch Points report.
C
		        IF ( bbb ( :3 ) .eq. 'COR' ) icorr = 1
		        icorr = icorr + itest
C
		        CALL WP_DECD ( bultin ( ibpnt: ), lenbul,
     +				   dattim, lunf, ier )
		        IF ( ier .eq. (-2) ) THEN
			    CALL DC_WLOG ( 2, 'DCWCP', ier, ' ', ierr )
			    good = .false.
		        END IF
		      ELSE
			CALL DC_WLOG ( 0, 'DC', ier, filnam, ier )
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
