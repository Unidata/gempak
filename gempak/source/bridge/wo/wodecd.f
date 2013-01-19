	SUBROUTINE WO_DECD ( report, lenr, iotarr, icorr, itest, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, origin, 
     +			     adstn, istnm, stnnam, stat, coun, adlat, 
     +			     adlon, selv, ispri, tbchrs, nade, iret )
C************************************************************************
C* WO_DECD 								*
C*									*
C* This subroutine processes a single watch outline update bulletin.	*
C*                                                                      *
C* WO_DECD ( REPORT, LENR, IOTARR, ICORR, ITEST, TISSUE, GEMFIL, STNTBL,*
C*	     IADSTN, MAXTIM, ORIGIN, ADSTN, ISTNM, STNNAM, STAT, COUN,	*
C*	     ADLAT, ADLON, SELV, ISPRI, TBCHRS, NADE, IRET ) 		*
C*									*
C*  Input parameters:							*
C*      REPORT          CHAR*           Winter storm bulletin 		*
C*      LENR            INTEGER         Length of bulletin		*
C*      IOTARR (5)      INTEGER         Bull. time - YYYY,MM,DD,HH,MM 	*
C*	ICORR		INTEGER		Correcting flag			*
C*      ITEST	   	INTEGER	   	Test flag		   	*
C*   	TISSUE	   	CHAR*	   	Bull. issue time, GEMPAK format *
C*    	GEMFIL	   	CHAR*	   	Output file name template	*
C*  	STNTBL	   	CHAR*	   	Station table		    	*
C* 	IADSTN	   	INTEGER	   	Number of additional stations   *
C* 	MAXTIM	   	INTEGER	   	Number of hours prior to CURTIM *
C* 	ORIGIN	   	CHAR*	   	Bulletin originator		*
C* 	ADSTN (*)	CHAR*	   	Station ID			*
C* 	ISTNM (*)	INTEGER	   	Station number		    	*
C* 	STNNAM (*)	CHAR*	   	Station name 		    	*
C* 	STAT (*)	CHAR*	   	State ID		   	*
C* 	COUN (*)	CHAR*	   	Country ID			*
C* 	ADLAT (*)	REAL 	   	Latitude			*
C* 	ADLON (*)	REAL 	   	Longitude			*
C* 	SELV (*)	REAL 	   	Elevation			*
C* 	ISPRI (*)	INTEGER	   	Priority number		    	*
C* 	TBCHRS (*)	CHAR*	   	Issuing station name 	    	*
C*	NADE 	   	INTEGER	   	Number of county table entries  *
C**									*
C*  Output parameters:                                                 	*
C*      IRET            INTEGER         Return code   			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 1/03	Increased stmid 3 -> 5			*
C* A. Hardy/NCEP	 2/03	Added 'tissue' to WO_OUT; added		*
C*                              test/correction flag logic		*
C* A. Hardy/NCEP	 2/03	Increased wfostns 32->120		*
C* A. Hardy/NCEP	 2/03	Added decoding of time zone;		*
C*				check for 'MIDNIGHT' & 'NOON'		*
C* A. Hardy/NCEP	 3/03	Changed time error code;added   	*
C*				check for start time > end time 	*
C* A. Hardy/NCEP	 4/03	Added check for empty stmid     	*
C* A. Hardy/NCEP	 5/03	Increase 'strbuf' 160 -> 220		*
C* A. Hardy/NCEP	 7/03	Added 'icancl' to wo_sgmt call  	*
C* A. Hardy/NCEP	 8/03   Added check for back up site		*
C* F. J. Yen/NCEP	10/06	Check WO_SGMT,WO_WFO,WO_OUT rtn codes, &*
C*				time zone. Fix subscript out of range in*
C*				calling WO_WFO.Stop proc if no ATTN line*
C* S. Jacobs/NCEP	 9/10	Added code to skip SCOTT AFB hdr line	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl, origin
        CHARACTER       adstn(LLSTFL)*8, stnnam(LLSTFL)*32,
     +                  stat(LLSTFL)*2, coun(LLSTFL)*2,
     +                  tbchrs(LLSTFL)*20
        INTEGER         istnm(LLSTFL), ispri(LLSTFL)
        REAL            adlat(LLSTFL), adlon(LLSTFL), selv(LLSTFL)
	INTEGER		iotarr (*)
C*
	CHARACTER	strbuf*230, carr(7)*4, dattim*40, stmid*5,
     +         		wtype*20, strtim*40, stptim*40, wfostns*120,
     +			cnties(LLSTFL)*6, filnam*132, parms (MMPARM)*4,
     +                  lcltim*30, zone*3, midnight*28, noon*28,
     +			tzones (13)*3, errstr*40
        
	LOGICAL		done, good, hdln
C*
C
        DATA            tzones / 'EDT', 'EST', 'CDT', 'CST',
     +                           'MDT', 'MST', 'PDT', 'PST',
     +                           'HST', 'AST', 'GMT', 'UTC', 'Z  ' /
C------------------------------------------------------------------------
	iret   = 0
        icancl = 0
        imid   = 0
        inoon  = 0
        lcltim = ' '
        strbuf = ' '
        hdln   = .false.
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( report, report, ier )
C
C*	Look for the time group.
C
	strbuf = report ( :220 )
	good = .true.
	inws = INDEX (strbuf, 'NWS STORM PREDICTION CENTER')
	IF ( inws .gt. 0 ) THEN
	    itime = inws + 27
	  ELSE
	    good = .false.
	    CALL DC_WLOG ( 2, 'DCWOU', -4, ' ', ier )
	END IF
C
C*	Skip the backup site header information, if present.
C
	ibkloc = INDEX ( strbuf,
     +			 'ISSUED BY 15TH OWS SCOTT AIR FORCE BASE IL' )
     	lenbk = 42
	IF ( ibkloc .gt. 0 ) THEN
	    itime = itime + lenbk
	ENDIF
C
C*      Find the local time string. Look for 'MIDNIGHT' and 'NOON'
C*      in the local time string.
C
	IF ( good ) THEN
            imid = INDEX ( strbuf(itime:itime+27), 'MIDNIGHT')
            inoon = INDEX ( strbuf(itime:itime+27), 'NOON')
            IF ( imid .gt. 0 ) THEN
                iadd = 27
                istart = itime + imid - 1
                lcltim = strbuf(istart : istart+iadd)
              ELSE IF ( inoon .gt. 0 ) THEN
                iadd = 23
                itime= itime + inoon - 1
                lcltim = strbuf(itime:itime+iadd)
              ELSE
	        done = .false.
	        DO WHILE ( .not. done )
	            CALL ST_ALNM ( strbuf(itime:itime), ityp, ier )
	            IF ( ityp .eq. 1 ) THEN
		        done = .true. 
	              ELSE
	    	        itime = itime + 1
		        IF ( itime .ge. 160 ) THEN
		    	    done = .true.
			    good = .false.
		        END IF
	            END IF
	        END DO
               
	        CALL ST_ALNM ( strbuf(itime+26:itime+26), ityp, ier )
                IF ( ityp .eq. 2) THEN
                    iadd = 25 
                  ELSE
                    iadd = 26 
                END IF 
                lcltim = strbuf(itime:itime+iadd)
            END IF
            ntime = iadd
C
C*	    Check for a backup station text line to skip over to 
C*	    find the time string.
C
            IF ( .not. good ) THEN
                good = .true.
 	        strbuf = report ( :230 )
                iampm = INDEX ( strbuf(itime:), ' AM ')
                imid = INDEX ( strbuf, 'MIDNIGHT')
                inoon = INDEX ( strbuf, 'NOON')
                IF ( iampm .eq. 0 ) THEN
                    iampm = INDEX ( strbuf(itime:), ' PM ')
                END IF
C
                IF ( iampm .gt. 0 ) THEN
                    jj = 2
                    done = .false.
                    DO WHILE ( .not. done ) 
                        CALL ST_ALNM (
     +                            strbuf(itime+iampm-jj:itime+iampm-jj), 
     +                                    ityp, ier )
                        IF ( ityp .ne. 1 ) THEN   
 	    	            done = .true.
                          ELSE
                            jj = jj + 1
                        END IF
                    END DO
                    iadd = 26
                    IF ( jj .ne. 5 ) iadd = 27
                    ibeg = itime + iampm -jj 
                    itime = ibeg 
                    lcltim = strbuf(itime:itime+iadd)
                  ELSE IF ( inoon .gt. 0 ) THEN
                    iadd = 23
                    itime = inoon 
                    lcltim = strbuf(itime:itime+iadd)
                  ELSE IF ( imid .gt. 0 ) THEN
                    iadd = 27
                    itime = imid
                    lcltim = strbuf(itime:itime+iadd)
                  ELSE
                    good = .false.
		    CALL DC_WLOG (2, 'DCWOU', -5, ' ', ier)
                END IF 
            END IF
	END IF
C
C*	Look for the headline and decode it.
C
        IF ( good ) THEN
  	    CALL WO_HDLN ( strbuf(:inws-1), wtype, stmid, ier )	
 	    IF ( wtype .eq. ' ' ) THEN
 	        good = .false.
 	        CALL DC_WLOG ( 2, 'DCWOU', -3, ' ', ier )
            END IF
C
 	    IF ( stmid .eq. ' ' ) THEN
 	        good = .false.
 	        CALL DC_WLOG ( 2, 'DCWOU', -8, ' ', ier )
            END IF
        END IF

C
C*      Put the local time string into GEMPAK time.
C
	IF ( good ) THEN
            IF ( ( imid .eq. 0 ) .and. ( inoon .eq. 0 ) ) THEN
	        CALL ST_CLST ( lcltim, ' ', ' ', 7,  carr, numb, ier )
C
	        CALL ST_LSTR ( carr (1), lens, ier )
	        IF ( lens .eq. 3 ) ntime = ntime - 1
	        CALL ST_LSTR ( carr (6), lens, ier )
	        IF ( lens .eq. 1 ) ntime = ntime - 1 
C
	        CALL TI_LOCL ( lcltim, dattim, ier ) 
              ELSE
                IF ( imid .gt. 0 ) THEN
                    midnight = '1200 AM' // lcltim(9:)
	            CALL TI_LOCL ( midnight, dattim, ier ) 
	            CALL ST_CLST ( midnight, ' ', ' ', 7,  carr, 
     +				   numb, ier )
                  ELSE IF ( inoon .gt. 0 ) THEN
                    noon = '1200 PM'// lcltim(5:) 
	            CALL TI_LOCL ( noon, dattim, ier ) 
	            CALL ST_CLST ( noon, ' ', ' ', 7,  carr, numb, ier )
                  ELSE
                    ier = -6
                END IF
            END IF
C       
	    IF ( ier .ne. 0 ) THEN
		good = .false.
	        CALL DC_WLOG ( 2, 'DCWOU', -6, ' ', ier )
	    END IF
C
C*          Find the time zone from the local time string.
C
            CALL ST_RMBL( carr(3), zone, lens, ier)
            zone = carr(3)
C
C*	    Check for valid time zone.
C
	    CALL ST_FIND ( carr(3), tzones, 13, ipos, ier )
	    IF ( ipos .eq. 0 ) THEN
		good = .false.
		CALL DC_WLOG ( 2, 'DCWOU', -11, ' ', ier )
	    END IF
	END IF
C
C*	Process watch outline update segment.
C
	IF ( good ) THEN
	    isgm = itime + ntime
	    done = .false.
	    DO WHILE ( .not. done )
C
C*	        Look for encoded county string.
C
	        itst1 = INDEX ( report(isgm:lenr), '-' ) + isgm - 1
	        itst2 = INDEX ( report(isgm:lenr), '>' ) + isgm - 1
C
	        IF ( report (itst1-4:itst1-4) .eq. 'C' ) THEN
	      	    ix = itst1
	          ELSE IF ( report (itst2-4:itst2-4) .eq. 'C' ) THEN
		    ix = itst2
		  ELSE
		    good = .false.
	        END IF
C				
		iseg0 = isgm
		IF ( good ) THEN
	            inum = 1
	            DO ii = 1, 3
		        CALL ST_ALNM ( report (ix-ii:ix-ii ), kk, ier )
		        inum = inum * kk 
	            END DO
	            IF ( inum .eq. 1 ) THEN
			iseg0 = ix - 6
		      ELSE
			good = .false.
	                CALL DC_WLOG ( 2, 'DCWOU', -1, ' ', ier )
		    END IF
		END IF
C
	    	iseg1 = INDEX ( report (iseg0:lenr), '$$' )
	    	IF ( iseg1 .gt. 0 ) THEN
		    lens = iseg1 - 1
		  ELSE
		    lens = lenr - iseg0 + 1
		END IF
C
	    	isgm = iseg0 + lens + 2
	    	IF ( isgm .ge. ( lenr - 50 ) ) done = .true.
C 
C*	    	    Decode watch outline update segment.
C
	    	    CALL WO_SGMT ( report(iseg0:iseg0+lens-1), lens,
     +		    		   dattim, lcltim, strtim, stptim, 
     +				   cnties, ncnty, icancl, ier )
                    IF ( ier .eq. 0 ) THEN
			good = .true.
		      ELSE
			IF ( ier .eq. 2 ) THEN
			    errstr = dattim
C
C*			    Although bad start time; used dattim
C
			    good = .true.
			  ELSE
			    errstr = ' '
			    good = .false.
			END IF
			CALL DC_WLOG (2, 'DCWOU', ier, errstr, ier2 )
		    END IF
C
C*	            Look for the wfo list line and decode it.
C
	            iwfo = INDEX ( report(:lenr), 'ATTN...WFO...')
	            IF ( iwfo .gt. 0 .and. good ) THEN
		        CALL ST_LSTR ( report(iwfo:lenr), len, ier )
                        CALL WO_WFO (report(iwfo:iwfo+len-1), len,
     +				wfostns, ier)
		        IF ( ier .ne. 0 ) THEN
C
C*			    ier could be -10, -13, or -14
C
	            	    CALL DC_WLOG ( 2, 'DCWOU', ier, ' ', ierr )
			    good = .false.
			END IF
		      ELSE
			ier = -15
	            	CALL DC_WLOG ( 2, 'DCWOU', ier, ' ', ierr )
			good = .false.
 	            END IF
		IF ( good ) THEN
C
		    IF ( ier .eq. 0 ) THEN
C
C*		        Update the test/correction flag.
C
                        IF ( ( icorr .eq. 1 ) .and. 
     +                                          ( itest .eq. 0 ) ) THEN
                            icorr = 1
                          ELSE IF ( ( icorr .eq. 0) .and. 
     +						( itest .eq. 3 ) ) THEN
                            icorr = 2
                          ELSE IF ( ( icorr .eq. 1) .and. 
     +						( itest .eq. 3 ) ) THEN
                            icorr = 3
                          ELSE 
                            icorr = 0
                        END IF
C
C*                      Check that the end time is after the start time. 
C
                	CALL TI_DIFF (  stptim, strtim, idiff, ier )
                	IF ( idiff .lt. 0 ) THEN
                    	    ier = -7
	            	    CALL DC_WLOG ( 2, 'DCWOU', ier, ' ', ierr )
                	END IF
C
                        IF ( ier .eq. 0 ) THEN
C
C*	    	            Make a file name from the template and the 
C*	    	            time.  Open the file as part of the open 
C*	    	            file list.
C
                            CALL FL_MNAM ( tissue, gemfil, filnam, ier )
 	    		    CALL DC_FCYL ( filnam, iflsrc, stntbl, 
     +			   	           iadstn, maxtim, lunf, nparm, 
     +					   parms, ier )
 	    		    CALL WO_OUT ( lunf, hdln, wtype, strtim, 
     +				        stptim, stmid, origin, wfostns, 
     +				        icorr, icancl, cnties, ncnty, 
     +				        adstn, istnm, stnnam, stat, 
     +				        coun, adlat, adlon, selv, ispri,
     +				        tbchrs, nade, tissue, zone, ier)
                            IF ( ier .eq. -12 ) THEN
				CALL DC_WLOG ( 2, 'DCWOU', ier, stmid,
     +						 ierr )
			      ELSE
			        hdln = .true.
			    END IF
                        END IF
		      ELSE
			CALL DC_WLOG ( 2, 'DCWOU', ier, ' ', ierr ) 
		    END IF
		  ELSE
		    good = .true.
		END IF
	    END DO
	END IF
C
 	CALL DC_FCLS ( ier )
C*
	RETURN
	END
