	SUBROUTINE FA_OUT  ( lunf, nvt, wtype, isstim, strtim, stptim, 
     +			     pgetim, wnum, wacd, ostn, prdc, imcs, 
     +			     county, ncnty, adstn, adstn2, istnm, 
     +			     istnm2, stnnam, stnna2, stat, stat2, coun,
     +			     coun2, adlat, adlat2, adlon, adlon2, selv, 
     +			     selv2, ispri, ispri2, tbchrs, tbchr2, nade, 
     +			     nade2, iret )
C************************************************************************
C* FA_OUT								*
C*									*
C* This subroutine writes the type, issue time, start time, stop time,  *
C* original station, event tracking number, action code, product class, *
C* immediate cause and watch counties/zones to an ASCII file.		*
C*									*
C* FA_OUT ( LUNF, NVT, WTYPE, ISSTIM, STRTIM, STPTIM, PGETIM, WNUM,	*
C*	    WACD, OSTN,	PRDC, IMCS, COUNTY, NCNTY, ADSTN, ADSTN2, ISTNM,* 
C*	    ISTNM2, STNNAM, STNNA2, STAT, STAT2, COUN, COUN2, ADLAT,    *
C*	    ADLAT2, ADLON, ADLON2, SELV, SELV2, ISPRI, ISPRI2, TBCHRS, 	*
C*	    TBCHR2, NADE, NADE2, IRET )					*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		File record number		*
C*	NVT		INTEGER		Number of decoded VTEC lines	*
C*	WTYPE		CHAR*		Watch type			*
C*	ISSTIM		CHAR*		Issue time			*
C*	STRTIM		CHAR*		Watch start time		*
C*	STPTIM		CHAR*		Watch stop time			*
C*	PGETIM		CHAR*		product end time from UGC string*
C*	WNUM		CHAR*		Watch number			* 
C*	WACD		CHAR*		Watch action code		* 
C*	OSTN		CHAR*		Issuing station name		*
C*	PRDC		CHAR*		Product class			*
C*	IMCS		CHAR*		Immediate cause from HVTEC line *
C*	COUNTY (NCNTY)	CHAR*6		County names in watch area      *
C*	NCNTY		INTEGER		Number of watch counties	*
C*	ADSTN (*)       CHAR*           Station ID                      *
C*	ADSTN2 (*)      CHAR*           Second Station ID               *
C*      ISTNM (*)       INTEGER         Station number                  *
C*      ISTNM2 (*)      INTEGER         Second Station number           *
C*      STNNAM (*)      CHAR*           Station name                    *
C*      STNNA2 (*)      CHAR*           Second Station name             *
C*      STAT (*)        CHAR*           State ID                        *
C*      STAT2 (*)       CHAR*           Second State ID                 *
C*      COUN (*)        CHAR*           Country ID                      *
C*      COUN2 (*)       CHAR*           Second Country ID               *
C*      ADLAT (*)       REAL            Latitude                        *
C*      ADLAT2 (*)      REAL            Second Latitude                 *
C*      ADLON (*)       REAL            Longitude                       *
C*      ADLON2 (*)      REAL            Second Longitude                *
C*      SELV (*)        REAL            Elevation                       *
C*      SELV2 (*)       REAL            Second Elevation                *
C*      ISPRI (*)       INTEGER         Priority number                 *
C*      ISPRI2 (*)      INTEGER         Second Priority number          *
C*      TBCHRS (*)      CHAR*           Issuing station name            *
C*      TBCHR2 (*)      CHAR*           Second Issuing station name     *
C*      NADE            INTEGER         Number of zone table entries    *
C*      NADE2           INTEGER         Number of cnty table entries    *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* H. Zeng/SAIC		07/05		Copied from wc_out		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wtype(*), strtim(*), stptim(*), ostn(*), 
     +                  wnum(*), county (*), prdc (*), isstim, pgetim
	CHARACTER*(*)   adstn (*), stnnam (*), stat (*), coun (*),
     +                  tbchrs (*), wacd(*), imcs (*)
	CHARACTER*(*)   adstn2 (*), stnna2 (*), stat2 (*), coun2 (*),
     +                  tbchr2 (*)
        INTEGER         istnm (*), ispri (*), nade
        INTEGER         istnm2 (*), ispri2 (*), nade2
        REAL            adlat (*), adlon (*), selv (*)
        REAL            adlat2 (*), adlon2 (*), selv2 (*)
C*
	CHARACTER       wloc*80
C-----------------------------------------------------------------------
	iret = 0
C
C*	Setting up the information string.
C
        DO ii = 1, nvt
	    CALL ST_LSTR ( wtype(ii), lenw, ier )
	    CALL ST_LSTR ( wnum(ii),  lenn, ier )
	    CALL ST_LSTR ( ostn(ii),  lens, ier )
	    CALL ST_LSTR ( prdc(ii),  lenp, ier )
	    CALL ST_LSTR ( wacd(ii),  lenc, ier )
	    CALL ST_LSTR ( imcs(ii),  leni, ier )
	    CALL ST_LSTR ( isstim,    leniss, ier )
	    CALL ST_LSTR ( strtim(ii),lenstr, ier )
	    CALL ST_LSTR ( pgetim,    lenpge, ier )
	    CALL ST_LSTR ( stptim(ii),lenstp, ier )
C
	    wloc ='|' // wtype(ii)(:lenw) // '|' // isstim(:leniss) 
     +		     // '|' // strtim(ii)(:lenstr) // '|' 
     +		     // pgetim(:lenpge) // '|' // stptim(ii)(:lenstp)
     +		     // '|' // ostn(ii)(:lens) // '|' // wnum(ii)(:lenn)
     +		     // '|' // wacd(ii)(:lenc) //';'//prdc(ii)(:lenp) 
     +		     // ';' // imcs(ii)(:leni)
C
C*	    Write the Flash Flood watch to the ASCII file. 
C
	    WRITE(lunf,20)wloc
 20         FORMAT (A)
C
C*	    Compare the county ids with the county table.
C
	    DO icnt = 1, ncnty
C
	       IF ( county(icnt)(3:3) .eq. 'Z' )  THEN
	          DO istn = 1, nade 
	             IF ( county(icnt) .eq. adstn(istn) ) THEN
	                WRITE(lunf,100) adstn(istn), istnm(istn), 
     +			      stnnam(istn), stat(istn), coun(istn), 
     +			      adlat (istn), adlon (istn), selv(istn), 
     +                        ispri(istn), tbchrs(istn)
	             END IF
	          END DO
C
               ELSE IF ( county(icnt)(3:3) .eq. 'C' )  THEN
	          DO istn = 1, nade2 
	             IF ( county(icnt) .eq. adstn2(istn) ) THEN
	                WRITE(lunf,200) adstn2(istn), istnm2(istn), 
     +			      stnna2(istn), stat2(istn), coun2(istn), 
     +			      adlat2(istn), adlon2(istn), selv2(istn), 
     +                        ispri2(istn), tbchr2(istn)
	             END IF
	          END DO
	       END IF
C	    
	    END DO
	END DO
C*
 100		        FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                    F9.2, 1X, F9.2, 1X, F9.2, 1X, I2, 1X, A )
 200		        FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                    F9.2, 1X, F9.2, 1X, F9.2, 1X, I2, 1X, A )
	RETURN
	END
