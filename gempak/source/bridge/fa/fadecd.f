	SUBROUTINE FA_DECD ( bultin, dattim, oristn, adstn, adstn2, 
     +			     istnm, istnm2, stnnam, stnna2, stat, stat2,
     +			     coun, coun2, adlat, adlat2, adlon, adlon2,
     +			     selv, selv2, ispri, ispri2, tbchrs, tbchr2, 
     +			     nade, nade2, lunf, bultim, iret ) 
C************************************************************************
C* FA_DECD								*
C*									*
C* This subroutine decodes flash flood watch bulletins and		*
C* writes the data to an ASCII file.					*
C*									*
C* FA_DECD  ( BULTIN, DATTIM, ORISTN, ADSTN, ADSTN2, ISTNM, ISTNM2 	*
C*            STNNAM, STNNA2, STAT, STAT2, COUN, COUN2, ADLAT, ADLAT2   *
C*	      ADLON, ADLON2, SELV, SELV2, ISPRI, ISPRI2, TBCHRS, TBCHR2 *
C*            NADE, NADE2, LUNF, BULTIM, IRET )				*
C*									*
C* Input parameters:							*
C*	BULTIN          CHAR*		Bulletin			*
C*      DATTIM          CHAR*		Starting date/time DDHHMM	*
C*	ORISTN          CHAR*		Bulletin originator		*
C* 	ADSTN  (*)	CHAR*	   	Station ID			*
C* 	ADSTN2 (*)	CHAR*	   	Second Station ID		*
C* 	ISTNM  (*)	INTEGER	   	Station number		    	*
C* 	ISTNM2 (*)	INTEGER	   	Second Station number		*
C* 	STNNAM (*)	CHAR*	   	Station name 		    	*
C* 	STNNA2 (*)	CHAR*	   	Second Station name 		*
C* 	STAT   (*)	CHAR*	   	State ID		   	*
C* 	STAT2  (*)	CHAR*	   	Second State ID		   	*
C* 	COUN   (*)	CHAR*	   	Country ID			*
C* 	COUN2  (*)	CHAR*	   	Second Country ID		*
C* 	ADLAT  (*)	REAL 	   	Latitude			*
C* 	ADLAT2 (*)	REAL 	   	Second Latitude			*
C* 	ADLON  (*)	REAL 	   	Longitude			*
C* 	ADLON2 (*)	REAL 	   	Second Longitude		*
C* 	SELV   (*)	REAL 	   	Elevation			*
C* 	SELV2  (*)	REAL 	   	Second Elevation		*
C* 	ISPRI  (*)	INTEGER	   	Priority number		    	*
C* 	ISPRI2 (*)	INTEGER	   	Second Priority number		*
C* 	TBCHRS (*)	CHAR*	   	Issuing station name 	    	*
C* 	TBCHR2 (*)	CHAR*	   	Second Issuing station name 	*
C*      NADE 	   	INTEGER	   	Number of zone table entries    * 
C*      NADE2 	   	INTEGER	   	Number of county table entries  *
C*	LUNF            INTEGER		Open file number		*
C*      BULTIM          CHAR*		Issue date/time DDHHMM		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				          -2 = Error with end time	*
C*									*
C**									*
C* Log:									*
C* H. Zeng/SAIC		07/05	Initial coding				*
C* F. J. Yen/NCEP	04/06	Enhanced log messages for -1, -3, & -6.	*
C*				Added log msg for return from FA_VTCN.	*
C* S. Jacobs/NCEP	01/07	Changed check on start time to set	*
C*				cont flag instead of more flag		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	PARAMETER       ( NV = 10 )
C*
	CHARACTER*(*)	bultin, oristn, dattim, bultim
        CHARACTER*(*)   adstn (*), stnnam (*), stat (*), coun (*),
     +                  tbchrs (*)
        CHARACTER*(*)   adstn2 (*), stnna2 (*), stat2 (*), coun2 (*),
     +                  tbchr2 (*)
	INTEGER         istnm (*), ispri (*), nade
	INTEGER         istnm2 (*), ispri2 (*), nade2
        REAL            adlat (*), adlon (*), selv (*)
        REAL            adlat2 (*), adlon2 (*), selv2 (*)
C*
        CHARACTER       wtype(NV)*3, county(LLSTFL)*6, endtim(NV)*11,
     +                  strtim(NV)*11, stptim(NV)*11, signif(NV)*2, 
     +			wnum(NV)*4, pvtec(NV)*124, hvtec(NV)*124,
     +	                cnties*700, purge*6,
     +                  isstim*11, tcnty*700, loctim*40, pgetim*20
	CHARACTER	btin*(DCMXBF), c*1, sss*700, ttt*700,
     +                  ostn(NV)*8, wacd(NV)*4, strbuf*160,
     +			prdc(NV)*4, imcs(NV)*4
        INTEGER		idtarr (5), itar (5), icancl(NV), ifix(NV),
     +                  itst(NV)
	LOGICAL		more, good, done, cont
C------------------------------------------------------------------------
C
C*	Ensure that "bultin" is upper case.
C
	CALL ST_LCUC ( bultin, bultin, ier )
C
C*	Look for the time group.
C
	strbuf = bultin ( :160 )
	good = .true.
	inws = INDEX (strbuf, 'NATIONAL WEATHER SERVICE')
	IF ( inws .gt. 0 ) THEN
	    itime = inws + 24
	ELSE
	    good = .false.
	    CALL DC_WLOG ( 2, 'DCFFA', -11, ' ', ier )
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
	    		CALL DC_WLOG ( 2, 'DCFFA', -6, strbuf(:20), ier )
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
		        CALL DC_WLOG ( 2, 'DCFFA', -6, strbuf(:20), ier)
		    END IF
	        END IF
	    END DO
C
	    IF ( good ) THEN
	        CALL TI_LOCL (strbuf (itime:(ntime-1)), loctim, ier)
	        IF ( ier .ne. 0 ) THEN
		    good = .false.
	            CALL DC_WLOG ( 2, 'DCFFA', -12, ' ', ier )
	        END IF
	    END IF
	END IF
C
C
C 
        itop  = 0
        ibeg  = 1
        isgm  = 1
	icor  = 0
	itest = 0
        more  = .true. 
        done  = .false.
C
        btin = bultin (ntime:)
C
        CALL ST_LSTR ( btin, lenb, ier )
C
        DO WHILE ( .not. done )
C
C*          Look for the VTEC and county strings.
C*          But first initialize pvtec&hvtec strings.
C
	    DO ii = 1, NV
	       DO jj = 1, 124
		  pvtec(ii)(jj:jj) = ' '
		  hvtec(ii)(jj:jj) = ' '
	       END DO
	    END DO
C
            CALL FA_VTCN ( btin, lenb, pvtec, hvtec, nvt, cnties, 
     +			   ier )
C
            IF ( ier .eq. 0 ) THEN
C
C*	        Decode PVTEC string.
C
                CALL FA_VTEC ( pvtec, nvt, dattim, prdc, wacd, ostn, 
     +	                       wtype, signif, wnum, strtim, endtim, 
     +                         ifix, icancl, itst, ier )
C
C*	        Get immediate cause from HVTEC string.
C
	        DO ii = 1, nvt
		  IF ( hvtec(ii)(1:6) .eq. '00000.' ) THEN
		     imcs(ii) = hvtec(ii)(9:10)
		  ELSE
		     imcs(ii) = '99'
		  END IF
	        END DO
	      ELSE
                CALL DC_WLOG ( 2, 'DCFFA', ier, bultin(:20), ierrr )
C
            END IF
C
            IF ( ier .ne. 0 ) THEN
                more = .false.
                done = .true.
            END IF
C
C
C*	    Decode county strings.
C
            IF ( more) THEN
                CALL ST_LSTR ( cnties,  len, ier )
                CALL ST_UNPR ( cnties, len, cnties, len1, ier )
C
C*              Have the below in from ST_RMBL because of 
C*              160 char. string limit.
C
                ttt    = ' '
                length = 0
                sss = cnties
                DO  ii = 1, len1
                    c = sss (ii:ii)
                    IF (( c .ne. CHSPAC ) .and. ( c .ne. CHTAB )) THEN
                        length = length + 1
                        ttt ( length : length ) = c
                    END IF
                END DO
                tcnty = ttt
C
 	        CALL FA_CNTY (tcnty, length, county, ncoun, purge, ier)
C
                IF ( ier .ne. 0 ) more = .false.
C
                DO ij = 1, nvt
		    cont = .true.
C
C*                  Check if start time is '000000/000'
C
                    IF ( (strtim (ij) .eq. '000000/0000') .and. 
     +                    ( icancl (ij) .eq. 0 ) ) THEN
                        strtim (ij) = dattim
                      ELSE IF ( ( strtim (ij) .eq. '000000/0000') .and. 
     +                           ( icancl (ij) .gt. 0 ) ) THEN
                        strtim (ij) = endtim(ij)
                    END IF
C
C*                  Compute the end time.
C
 	            CALL TI_CTOI ( strtim(ij), idtarr, ier1 )
 	            CALL ST_INTG ( purge, ipurge, ier2 )
 	            IF ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) ) THEN
 	                iday = ipurge / 10000
 	                ihour = ipurge /100 - iday * 100
 	                imin = ipurge-iday*10000-ihour*100
 	                CALL DC_ITIM ( idtarr, iday, ihour, imin, itar, 
     +					ier) 
			CALL TI_ITOC ( itar, pgetim, ier )
C
                        stptim (ij) = endtim(ij)
                    ELSE
                        cont = .false.
	   	        iret = -2
                        CALL DC_WLOG ( 2, 'DCFFA', iret, ' ', ierrr )
	            END IF
C
C*	            Check if corrected report.
C
	            IF ( ifix(ij) .gt. 0 )  icor = 1
C
C*	            Check if test report.
C
	            IF ( itst(ij) .gt. 0 )  itest = 1
C
                    IF ( wtype (ij) .eq. ' ' ) cont = .false.
                    IF ( wnum (ij)  .eq. ' ' ) cont = .false.
                END DO
C
                IF ( cont ) THEN 
C
C*	            Convert bulletin issue time into GEMPAK format.
C
                    CALL ST_NUMB ( bultim, ibultim, ier )
	            iday = ibultim/ 10000
	            ihour = ibultim/100 - iday * 100
	            imin = ibultim-iday*10000-ihour*100
	            CALL DC_ITIM ( idtarr, iday, ihour, imin, itar,
     +				   ier ) 
	            CALL TI_ITOC ( itar, isstim, ier )
C
C*	            Write out flash flood watch to an ASCII file.
C
                    CALL FA_OUT (lunf, nvt, wtype, loctim, strtim, 
     +                       stptim, pgetim, wnum, wacd, ostn, prdc, 
     +			     imcs,  county, ncoun, adstn, adstn2,
     +			     istnm, istnm2, stnnam, stnna2, stat, 
     +			     stat2, coun, coun2, adlat, adlat2, 
     +			     adlon, adlon2, selv, selv2, ispri, 
     +			     ispri2, tbchrs, tbchr2, nade, nade2, 
     +			     iret )
	        END IF
C
C*              Look for end of segment.
C
 	        iseg1 = INDEX ( btin(:lenb), '$$' )
                CALL ST_LSTR ( btin(iseg1+2:), lenb, ier )
                btin = btin(iseg1+2:)
C
C*		Could remove the following block later.
C
                IF ( iseg1 .gt. 0 ) THEN
                    lens = iseg1 - 1
                ELSE
                    lens = lenb - iseg1 + 1
                END IF
C
                isgm = iseg1 
                CALL ST_LSTR ( btin, lenth, ier )
                IF ( isgm .le. 2 ) done = .true.
                IF ( lenth .le. 20 ) done = .true.
            END IF
        END DO 
C*
	RETURN
	END
