	SUBROUTINE WS_DECD ( report, lenr, iotarr, icorr, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, origin, 
     +			     adstn, istnm, stnnam, stat, coun, adlat, 
     +			     adlon, selv, ispri, tbchrs, nade, iret )
C************************************************************************
C* WS_DECD 								*
C*									*
C* This subroutine processes a single winter storm bulletin.            *
C*                                                                      *
C* WS_DECD ( REPORT, LENR, IOTARR, ICORR, TISSUE, GEMFIL, STNTBL,	*
C*	     IADSTN, MAXTIM, ORIGIN, ADSTN, ISTNM, STNNAM, STAT, COUN,	*
C*	     ADLAT, ADLON, SELV, ISPRI, TBCHRS, NADE, IRET ) 		*
C*									*
C*  Input parameters:							*
C*      REPORT          CHAR*           Winter storm bulletin 		*
C*      LENR            INTEGER         Length of bulletin		*
C*      IOTARR (5)      INTEGER         Bull. time - YYYY,MM,DD,HH,MM 	*
C*	ICORR		INTEGER		Correcting flag			*
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
C*	NADE 	   	INTEGER	   	Number of zone table entries    *
C**									*
C*  Output parameters:                                                 	*
C*      IRET            INTEGER         Return code   			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		07/02						*
C* D. Kidwell/NCEP	 9/02	Added error checks, fixed length check  *
C* D. Kidwell/NCEP	10/02	Improved zone string search             *
C* M. Li/SAIC		10/02	Initialized iflsrc			*
C* M. Li/SAIC		10/02	Improved method to get the time string	*
C* D. Kidwell/NCEP	11/02	Added argument timstr to WS_SGMT call   *
C* M. Li/SAIC 		02/03 	Increased the dimension of some var     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl, origin
        CHARACTER       adstn(LLSTFL)*8, stnnam(LLSTFL)*32,
     +                  stat(LLSTFL)*2, coun(LLSTFL)*2,
     +                  tbchrs(LLSTFL)*20
        INTEGER         istnm(LLSTFL), ispri(LLSTFL), icancl(2)
        REAL            adlat(LLSTFL), adlon(LLSTFL), selv(LLSTFL)
	INTEGER		iotarr (*)
C*
	CHARACTER	strbuf*160, dattim*40, timstr*50
	CHARACTER	wtype(2)*20, wthr(2)*20, strtim(2)*40, 
     +                  stptim(2)*40
	CHARACTER	zones (LLSTFL)*6, filnam*132, parms (MMPARM)*4 
	LOGICAL		done, good, found
C------------------------------------------------------------------------
	iret  = 0
	iflsrc = MFUNKN
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( report, report, ier )
C
C*	Look for the time group.
C
	strbuf = report ( :160 )
	good = .true.
	inws = INDEX (strbuf, 'NATIONAL WEATHER SERVICE')
	IF ( inws .gt. 0 ) THEN
	    itime = inws + 24
	  ELSE
	    good = .false.
	    CALL DC_WLOG ( 2, 'DCWSTM', -4, ' ', ier )
	END IF
C
C*	Search the beginning of time string.
C
	IF ( good ) THEN
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
	    		CALL DC_WLOG ( 2, 'DCWSTM', -5, ' ', ier )
		    END IF
	        END IF
	    END DO
	END IF
C 
C*	Look for the end of time string.
C
	ntime = 0
	IF ( good ) THEN
	    done = .false.
	    kk   = itime
	    myr  = kk + 50
	    DO WHILE ( .not. done )
	        iyr = INDEX ( strbuf(kk:myr), ' 20' )
	        nn1 = kk + iyr + 2
	        nn2 = nn1 + 1	
	        CALL ST_ALNM ( strbuf (nn1:nn1), ityp1, ier )
	        CALL ST_ALNM ( strbuf (nn2:nn2), ityp2, ier )
C
	        IF ( iyr .gt. 0 .and. ityp1 .eq. 1 .and. 
     +	            ityp2 .eq. 1 ) THEN
		    ntime = kk + iyr + 4
		    done  = .true.
	        ELSE
		    kk = kk + iyr
		    IF ( ( iyr .eq. 0 ) .or. ( kk .ge. myr ) ) THEN
	   	    	done = .true.
			good = .false.
		        CALL DC_WLOG ( 2, 'DCWSTM', -5, ' ', ier )
		    END IF
	        END IF
	    END DO
C
	    IF ( good ) THEN
	        CALL TI_LOCL (strbuf (itime:(ntime-1)), dattim, ier)
	        IF ( ier .ne. 0 ) THEN
		    good = .false.
	            CALL DC_WLOG ( 2, 'DCWSTM', -6, ' ', ier )
		  ELSE
C
C*		    Save the time string.
C
		    timstr = strbuf ( itime:( ntime - 1 ) )
	        END IF
	    END IF
	END IF
C
C*	Process winter storm segment.
C
	IF ( good ) THEN
	    isgm = ntime
	    done = .false.
	    DO WHILE ( .not. done )
C
C*	        Look for winter storm segment.
C
		found = .false.
		DO WHILE ( .not. found .and. .not. done )
C
C*		    Look for '-' or '>' which will follow a zone
C*		    designator.
C
	            itst1 = INDEX ( report(isgm:lenr), '-' )
	            itst2 = INDEX ( report(isgm:lenr), '>' )
		    IF ( itst1 .eq. itst2 ) THEN
			done = .true.
		      ELSE
			IF ( itst1 .eq. 0 ) THEN
			    ix = itst2
			  ELSE IF ( itst2 .eq. 0 ) THEN
			    ix = itst1
			  ELSE
			    ix = MIN ( itst1, itst2 )
			END IF
			ix = ix + isgm - 1
		    END IF
C
C*		    Check for a zone designator having the format
C*		    aaZnnn.
C				
		    iseg0 = isgm
		    IF ( .not. done ) THEN
	                inum = 1
	                DO ii = 1, 3
		            CALL ST_ALNM ( report ( ix-ii:ix-ii ), kk,
     +					   ier )
		            inum = inum * kk 
	                END DO
	                IF ( ( inum .eq. 1 ) .and. 
     +			     ( report (ix-4:ix-4 ) .eq. 'Z' ) ) THEN
			    CALL ST_WORD ( report (ix-6:ix-5), kk, ier )
			    IF ( kk .eq. 0 ) THEN
				kcor = INDEX ( report (ix-20:ix-7),
     +				               'CORRECTED' )
				IF ( kcor .eq. 0 ) THEN
			            iseg0 = ix - 6
				    found = .true.
				END IF
			    END IF
			END IF
		    END IF
		    IF ( .not. found ) THEN
			isgm = ix + 1
			IF ( isgm .gt. lenr ) done = .true.
		    END IF
		END DO
C
		IF ( done ) THEN
		    good = .false.
	            CALL DC_WLOG ( 2, 'DCWSTM', -1, ' ', ier )
		END IF
C
C*		Look for end of segment.
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
		IF ( good ) THEN
C 
C*	    	    Decode winter storm segment.
C
	    	    CALL WS_SGMT ( report(iseg0:iseg0+lens-1), lens,
     +		    	   dattim, timstr, wtype, wthr, strtim,
     +			   stptim, icancl, nhdln, zones, nzone, ier )
C
		    IF ( ( ier .eq. 0 ) .or.
     +		       ( ( ier .eq. -7 ) .and. ( nzone .gt. 0 ) ) ) THEN
C
C*	    	        Make a file name from the template and the 
C*	    	        time.  Open the file as part of the open 
C*	    	        file list.
C
 	    		CALL FL_MNAM ( tissue, gemfil, filnam, ier1 )
 	    		CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn,
     +			   	       maxtim, lunf, nparm, parms, ier1)
	    		CALL WS_OUT ( lunf, wtype, strtim, stptim,
     +				      origin, wthr, icorr, icancl, 
     +				      nhdln, zones, nzone, adstn, istnm, 
     +				      stnnam, stat, coun, adlat, adlon,
     +				      selv, ispri, tbchrs, nade, ier1 )
		    END IF
		    IF ( ier .lt. 0 ) THEN
			CALL DC_WLOG ( 2, 'DCWSTM', ier, ' ', ierr ) 
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
