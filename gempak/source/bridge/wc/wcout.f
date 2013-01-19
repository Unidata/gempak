	SUBROUTINE WC_OUT  ( lunf, nvt, wtype, isstim, strtim, stptim, 
     +			     wnum, wacd, ostn, signif, icor, icancl, 
     +			     itest, county, ncnty, adstn, istnm, stnnam,
     +			     stat, coun, adlat, adlon, selv, ispri, 
     +			     tbchrs,  nade, iret )
C************************************************************************
C* WC_OUT								*
C*									*
C* This subroutine writes the warning type, start time, stop time,      *
C* issuing station id, correction indicator and warning counties        *
C* to an ASCII file.							*
C*									*
C* WC_OUT ( LUNF, NVT, WTYPE, ISSTIM, STRTIM, STPTIM, WNUM, WACD, OSTN,	* 
C*          SIGNIF, ICOR, ICANCL, ITEST, COUNTY, NCNTY, ADSTN, ISTNM,	* 
C*	    STNNAM, STAT, COUN, ADLAT, ADLON, SELV, ISPRI, TBCHRS, 	*
C*	    NADE, IRET )						*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		File record number		*
C*	NVT		INTEGER		Number of decoded VTEC lines	*
C*	WTYPE		CHAR*		Watch type			*
C*	ISSTIM		CHAR*		Issue time			*
C*	STRTIM		CHAR*		Watch start time		*
C*	STPTIM		CHAR*		Watch stop time			*
C*	WNUM		CHAR*		Watch number			* 
C*	WACD		CHAR*		Watch action code		* 
C*	OSTN		CHAR*		Issuing station name		*
C*	SIGNIF		CHAR*		Significance			*
C*	ICOR		INTEGER		Correction flag			*
C*	ICANCL		INTEGER		Cancelation flag		*
C*	ITEST		INTEGER		Test flag 			*
C*	COUNTY (NCNTY)	CHAR*6		County names in watch area      *
C*	NCNTY		INTEGER		Number of watch counties	*
C*	ADSTN (*)       CHAR*           Station ID                      *
C*      ISTNM (*)       INTEGER         Station number                  *
C*      STNNAM (*)      CHAR*           Station name                    *
C*      STAT (*)        CHAR*           State ID                        *
C*      COUN (*)        CHAR*           Country ID                      *
C*      ADLAT (*)       REAL            Latitude                        *
C*      ADLON (*)       REAL            Longitude                       *
C*      SELV (*)        REAL            Elevation                       *
C*      ISPRI (*)       INTEGER         Priority number                 *
C*      TBCHRS (*)      CHAR*           Issuing station name            *
C*      NADE            INTEGER         Number of table entries         *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 2/03		Added isstim to output string	*
C* A. Hardy/NCEP	 2/03		Made VTEC variables arrays	*
C* A. Hardy/NCEP	 3/05		Added storing VTEC action code  *
C* F. J. Yen/NCEp	 3/07		Checked for bogus WFO KWNS	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wtype(*), strtim(*), stptim(*), ostn(*), 
     +                  wnum(*), signif(*), county (*), isstim
	CHARACTER*(*)   adstn (*), stnnam (*), stat (*), coun (*),
     +                  tbchrs (*), wacd(*)
        INTEGER         istnm (*), ispri (*), nade, icancl(*)
        REAL            adlat (*), adlon (*), selv (*)
C*
	CHARACTER       wloc*80, corr, ccan, ctst
C-----------------------------------------------------------------------
	iret = 0
C
C*	Setting up the information string.
C
        DO ii = 1, nvt
C
C*	  Check for bogus WFOs: WCNs should not have 'KWNS' used by the
C*	  SPC for WOUs.
C
	  IF ( ostn(ii)(1:4) .eq. 'KWNS' ) THEN
	    CALL DC_WLOG ( 0, 'DCWCN', -11, ' ', ierr )
	   ELSE
	    CALL ST_INCH ( icor, corr, ier )
	    CALL ST_INCH ( icancl(ii), ccan, ier )
	    CALL ST_INCH ( itest, ctst, ier )
	    CALL ST_LSTR ( wtype(ii), lenw, ier )
	    CALL ST_LSTR ( wnum(ii), lenn, ier )
	    CALL ST_LSTR ( ostn(ii), lens, ier )
	    CALL ST_LSTR ( signif(ii), leng, ier )
	    CALL ST_LSTR ( wacd(ii), lenc, ier )
	    wloc ='|' // wtype(ii)(:lenw) // '|' // isstim // '|' 
     +	             // strtim(ii) // '|' 
     +	             // stptim(ii) // '|' // wnum(ii)(:lenn) // '|' 
     +               // ostn(ii)(:lens) // '|' 
     +		     // wacd(ii)(:lenc)//';'//signif(ii)(:leng) // '|'
     +               // corr // '|' // ccan // '|' // ctst
C
C*	    Write the WCN information to the ASCII file. 
C
	    WRITE(lunf,20)wloc
 20         FORMAT (A)
C
C*	    Compare the county ids with the county table.
C
	    DO istn = 1, nade 
	        DO icnt = 1, ncnty
	            IF ( county(icnt) .eq. adstn(istn) ) THEN
	                WRITE(lunf,100) adstn(istn), istnm(istn), 
     +			        stnnam(istn), stat(istn), coun(istn), 
     +			        adlat (istn), adlon (istn), selv(istn), 
     +                          ispri(istn), tbchrs(istn)
 100		        FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                    F9.2, 1X, F9.2, 1X, F9.2, 1X, I2, 1X, A )
		    END IF
	        END DO
	    END DO
	  END IF
	END DO
C*
	RETURN
	END
