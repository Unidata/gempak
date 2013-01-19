	SUBROUTINE WC_DECD ( bultin, dattim, oristn, icor, itest, 
     +                       adstn, istnm, stnnam, stat, coun, adlat, 
     +			     adlon, selv, ispri, tbchrs, nade,
     +			     lunf, bultim, iret ) 
C************************************************************************
C* WC_DECD								*
C*									*
C* This subroutine decodes watch county notification bulletins and	*
C* writes tthe data to an ASCII file.					*
C*									*
C* WC_DECD  ( BULTIN, DATTIM, ORISTN, ICOR, ITEST, ADSTN, ISTNM, 	*
C*            STNNAM, STAT, COUN, ADLAT, ADLON, SELV, ISPRI, TBCHRS,    *
C*            NADE, LUNF, BULTIM, IRET ) 				*
C*									*
C* Input parameters:							*
C*	BULTIN          CHAR*		WCN bulletin			*
C*      DATTIM          CHAR*		Starting date/time DDHHMM	*
C*	ORISTN          CHAR*		Bulletin originator		*
C*	ICOR            INTEGER         Correction flag			*
C*	ITEST           INTEGER		Test flag			*
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
C*      NADE 	   	INTEGER	   	Number of county table entries  *
C*	LUNF            INTEGER		Open file number		*
C*      BULTIM          CHAR*		Issue date/time DDHHMM		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				          -2 = Error with end time	*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 1/03		Increased vtec  120->124	*
C* A. Hardy/NCEP	 2/03		Add bultim to WC_OUT call seq.; *
C*					remove bad chars from cnty line *
C* A. Hardy/NCEP	 2/03		Made VTEC variables arrays	*
C* A. Hardy/NCEP	 2/03		Removed decoding of missing VTEC*
C* A. Hardy/NCEP	 3/05		Added storing VTEC action code *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	PARAMETER       ( NV = 10 )
C*
	CHARACTER*(*)	bultin, oristn, dattim, bultim
        CHARACTER*(*)   adstn (*), stnnam (*), stat (*), coun (*),
     +                  tbchrs (*)
	INTEGER         istnm (*), ispri (*), nade
        REAL            adlat (*), adlon (*), selv (*)
C*
        CHARACTER       wtype(NV)*3, county(LLSTFL)*6, endtim(NV)*11,
     +                  strtim(NV)*11, stptim(NV)*11, signif(NV)*2, 
     +			wnum(NV)*4, vtec(NV)*124,
     +	                cnties*700, purge*6, temptim*11,
     +                  isstim*11, tcnty*700
	CHARACTER	btin*(DCMXBF), c*1, sss*700, ttt*700,
     +                  ostn(NV)*8, wacd(NV)*4
        INTEGER		idtarr (5), itar (5), icancl(NV), ifix(NV),
     +                  itst(NV)
	LOGICAL		more, done, novtec 
C------------------------------------------------------------------------
	iret = 0
        itop = 0
        ibeg = 1
        isgm = 1
        more = .true. 
        done = .false.
        novtec = .false.
C
        btin = bultin
        CALL ST_LSTR ( btin, lenb, ier )
C
        DO WHILE ( .not. done )
C
C*          Look for the VTEC and county strings.
C
            CALL WC_GHDR ( btin, lenb, vtec, nvt, cnties, nchar, ier )
C
            IF ( ier .eq. 0 ) THEN
C
C*	        Decode VTEC string.
C
                CALL WC_VTEC ( vtec, nvt, dattim, wacd, ostn, wtype, 
     +	                       signif, wnum, strtim, endtim, ifix, 
     +                         icancl, itst, ier ) 
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
 	        CALL WC_CNTY (tcnty, length, county, ncoun, purge, ier)
C
                IF ( ier .ne. 0 ) more = .false.
C
                DO ij = 1, nvt
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
C*                  If not a cancel report, use decoded local time.
C
                    IF ( novtec ) strtim (1) = temptim
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
C
                        stptim (ij) = endtim(ij)
                      ELSE
                        more = .false.
	   	        iret = -2
                        CALL DC_WLOG ( 2, 'DCWCN', iret, ' ', ierrr )
	            END IF
C
C*	            Check if corrected report.
C
	            IF (( ifix(ij) .gt. 0) .and. (icor .eq. 0 )) THEN
       	    	        icor = 1
                    END IF
C
C*	            Check if test report.
C
	            IF ( ( ( itst(ij) .gt. 0 ) .and. ( itest .eq. 0 ) ) 
     +				       .or. ( itest .eq. 2 ) ) THEN
       	    	        itest = 1
                    END IF
C
                    IF ( wtype (ij) .eq. ' ' ) more = .false.
                    IF ( wnum (ij)  .eq. ' ' ) more = .false.
                END DO
C
                IF ( more ) THEN 
C
C*	        Convert bulletin issue time into GEMPAK format.
C
                CALL ST_NUMB ( bultim, ibultim, ier )
	        iday = ibultim/ 10000
	        ihour = ibultim/100 - iday * 100
	        imin = ibultim-iday*10000-ihour*100
	        CALL DC_ITIM ( idtarr, iday, ihour, imin, itar, ier ) 
	        CALL TI_ITOC ( itar, isstim, ier )
C
C*	        Write out watch county notification to an ASCII file.
C
                    CALL WC_OUT (lunf, nvt, wtype, isstim, strtim, 
     +                          stptim, wnum, wacd, ostn, signif, icor, 
     +		                icancl, itest, county, ncoun, adstn, 
     +			        istnm, stnnam, stat, coun, adlat, adlon, 
     +				selv, ispri, tbchrs, nade, iret )
	        END IF
C
C*              Look for end of segment.
C
 	        iseg1 = INDEX ( btin(:lenb), '$$' )
                CALL ST_LSTR ( btin(iseg1+2:), lenb, ier )
                btin = btin(iseg1+2:)
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
