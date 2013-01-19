	SUBROUTINE SV_DCOD  ( curtim, gemfil, stntbl, prmfil, 
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* SV_DCOD								*
C*									*
C* This subroutine decodes SLS tornado and thunderstorm watch bulletins	*
C* and writes the data to an ASCII file.				*
C*									*
C* SV_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
C*		IRET )    						*
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
C*					 -5 = Unable to get bultim	*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	12/00	Created from WN_DCOD			*
C* F. J. Yen/NCEP	 1/01	Restructured to disregard '$$' delimeter*
C* F. J. Yen/NCEP	 1/01	Replaced SV_RDTB with DC_STNS		*
C* F. J. Yen/NCEP	 4/01	Added state abbreviation to header line.*
C* F. J. Yen/NCEP        3/02   Checked issue time & false delimeter -  * 
C* M. Li/SAIC		 08/02	WN_CNTY -> BR_CNTY			*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, prmfil, stntbl
C*
	CHARACTER	bultin*(DCMXBF), parms(MMPARM)*4,
     +			starry(5)*2800, filnam*132, errstr*80, wtype*2, 
     +			state*2, dattim*11, bultim*12,
     +			wmohdr*8, oristn*8, seqnum*4, cnties*700, 
     +			exptim*6, sysdt*12, dattmp*12, county(LLSTFL)*6,
     +			strtim*11, stptim*11, bbb*4
	CHARACTER	adstn(LLSTFL)*8, stnnam(LLSTFL)*32,
     +			stat(LLSTFL)*2, coun(LLSTFL)*2,
     +			tbchrs(LLSTFL)*20
        INTEGER         istarr (5), irdtar (5) 
	INTEGER		istnm(LLSTFL), ispri(LLSTFL), itype
	REAL		adlat(LLSTFL), adlon(LLSTFL), selv(LLSTFL)
	LOGICAL		more, morect, cntyol
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
C*	Read the county table.
C
	CALL DC_STNS  ( stntbl, adstn, istnm, stnnam, stat, coun, adlat,
     +		adlon, selv, ispri, tbchrs, nade, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Loop until a timeout occurs.
C
	iperr = 0
	DO WHILE ( iperr .eq. 0 )
	    icor = 0
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
		more = .true.
		CALL DC_TEST ( bultin, lenbul, itest, ier )
C
C*		Break bulletin into 5 strings.
C
		CALL ST_CLSL (bultin,'*',' ',5,starry, inumb, ierr)
C
C*		Parse the header info from the bulletin.
C
                CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +                         oristn, bultim, bbb, nchar, ierr )
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
                    CALL ST_UNPR ( bultin (:80), 80, errstr, len1, ier )
                    CALL DC_WLOG ( 2, 'DCSVRL', 1, errstr(:len1), ier )
                    more = .false.
                END IF
		IF ( more ) THEN
		    CALL SV_GHDR ( bultin, lenbul, icor, wtype,
     +			       iwtchn, state, iers )
                    IF (  bbb .eq. 'COR'  ) icor = 1
                    IF ( iers .lt. 0 ) THEN
			more = .false.
			CALL ST_UNPR ( bultin (:80), 80, errstr,
     +				       len1, ierr )
			CALL DC_WLOG ( 2, 'DCSVRL', iers,
     +				       errstr (:len1), ierr )
		    END IF
		END IF
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
C*		    Set the GEMPAK date/time. Use bultim to set
C*		    the output file name.
C
		    CALL ST_LSTR ( bultim, len, ier )
                    IF ( len .ne. 6 ) THEN
                        IF ( curtim .eq. 'SYSTEM' ) THEN
                            bultim = dattmp ( 5:6 ) // dattmp ( 8:11 )
                          ELSE
                            iret = -5
			    CALL ST_UNPR ( bultin (:80), 80, errstr,
     +					len1, ierr )
		            CALL DC_WLOG ( 2, 'DCSVRL', iret,
     +					errstr (:len1), ierr )
			    more = .false.
                        END IF
                    END IF
		    IF ( more ) THEN
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
     +				ier )
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
C*			    value
C*                          was invalid.
C
                            bultim = dattim ( 5:6 ) // dattim ( 8:11 )
C
C*		            Remove control characters from
C*			    entire bulletin.
C
		            CALL ST_UNPR ( bultin, lenbul, bultin, lenb,
     +					iret )
C
C*		            Make a file name from the template and the
C*			    time.  Open the file as part of the open
C*			    file list.
C
 		            CALL FL_MNAM ( dattim, gemfil, filnam, ier )
 		            CALL DC_FCYL ( filnam, iflsrc, stntbl,
     +					   iadstn, maxtim, lunf, nparm,
     +					   parms, ierr )
C
C*	                    Check that the file was opened properly.
C
		            IF ( ierr .ne. 0 ) THEN
C
C*	    	                If not, write an error to the decoder 
C*			        log file.
C
	    	                CALL DC_WLOG ( 0, 'SF', ierr, filnam,
     +						iret )
                                more = .false.
		            END IF
		            IF ( more ) THEN
C
			        cntyol = .false.
			        morect = .true.
			        npos = 50
			        DO WHILE ( npos .lt. lenb .and. morect )
				    nloc = npos + 1
				    nloce = lenb
				    irdash = INDEX ( bultin(nloc:nloce),
     +							'-' )
				    IF ( irdash .gt. 0 ) THEN
				        nlocmb = nloc + irdash
				        ictybeg = nlocmb - 7
				        CALL SV_CNTT ( bultin, ictybeg,
     +					   lenb, cnties, exptim,
     +					   jx, iers )
				        IF ( iers .eq. 0 ) THEN
				            IF ( .not. cntyol ) THEN
		            	                IF ( wtype .eq. 'TS'
     +						     .or. wtype .eq.
     +							'TN' ) THEN
 	                        	            CALL WN_STFS (
     +						      bultim, istarr,
     +						      exptim, cnties,
     +						      1440, 720,
     +						      bultin, 'DCSVRL',
     +						      strtim, stptim,
     +						      county, numcty,
     +						      iret )
			       		          ELSE
				  	            more = .false.
                            		        END IF
                            		        IF ( (wtype .eq. ' ')
     +							.or.
     +						     (strtim .eq. ' ')
     +							.or.
     +						     (stptim .eq. ' ') )
     +                            	              more = .false.
				              ELSE
					        CALL BR_CNTY ( cnties,
     +						    county, numcty,
     +						    iret )
				            END IF
C
C*				            Write out header (if first
C*					    time) and next county list
C
				            IF ( more ) THEN
					        CALL SV_OUT ( lunf,
     +						      wtype, strtim,
     +						      stptim, iwtchn,
     +						      state, icor,
     +						      county, numcty,
     +						      cntyol, adstn,
     +						      istnm, stnnam,
     +						      stat, coun, adlat,
     +						      adlon, selv,
     +						      ispri, tbchrs,
     +						      nade, iret )
					        cntyol = .true.
				            END IF
				          ELSE
				            CALL ST_UNPR ( bultin (:80),
     +				  	        80, errstr, len1, ierr )
				            CALL DC_WLOG ( 2, 'DCSVRL',
     +					        iers, errstr (:len1),
     +					        ierr )
				        END IF

				      ELSE
				        morect = .false.
			            END IF
				    IF ( morect ) THEN
				        npos = jx
				    END IF
			        END DO
		            END IF
			END IF
                    END IF
                END IF 
 	        CALL DC_FCLS ( ier )
	    END IF
	END DO
C*
	RETURN
	END
