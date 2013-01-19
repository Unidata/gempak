	SUBROUTINE SV_OUT  ( lunf, wtype, strtim, stptim, iwtchn, state,
     +			     icor, county, ncnty, cntyol, adstn, istnm,
     +			     stnnam, stat, coun, adlat, adlon, selv,
     +			     ispri, tbchrs, nade, iret )
C************************************************************************
C* SV_OUT								*
C*									*
C* This subroutine writes the SLS watch type, start time, stop time,    *
C* watch number, watch state abbreviation, correction indicator and	*
C* watch counties to an ASCII file.					*
C*									*
C* SV_OUT ( LUNF, WTYPE, STRTIM, STPTIM, IWTCHN, STATE, ICOR, COUNTY,	*
C*	    NCNTY, CNTYOL, ADSTN, ISTNM, STNNAM, STAT, COUN, ADLAT,	*
C*	    ADLON, SELV, ISPRI, TBCHRS, NADE, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		File record number		*
C*	WTYPE		CHAR*		Warning type			*
C*	STRTIM		CHAR*		Warning start time		*
C*	STPTIM		CHAR*		Warning stop time		*
C*	IWTCHN		INTEGER		Watch number			*
C*	STATE		CHAR*		Watch header state abbrev.	*
C*	ICOR		INTEGER		Correction indicator		*
C*	COUNTY (NCNTY)	CHAR*6		County names in warning area    *
C*	NCNTY		INTEGER		Number of warning counties	*
C*	CNTYOL		LOGICAL		Flag to output county line only	*
C*	ADSTN (*)	CHAR*		Station ID			*
C*	ISTNM (*)	INTEGER		Station number			*
C*	STNNAM (*)	CHAR*		Station name			*
C*	STAT (*)	CHAR*		State ID			*
C*	COUN (*)	CHAR*		Country ID			*
C*	ADLAT (*)	REAL		Latitude			*
C*	ADLON (*)	REAL		Longitude			*
C*	SELV (*)	REAL		Elevation			*
C*	ISPRI (*)	INTEGER		Priority number			*
C*	TBCHRS (*)	CHAR*		Issuing station name		*
C*	NADE		INTEGER		Number of table entries		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	12/00	Created from WN_OUT.			*
C* F. J. Yen/NCEP	 1/01	Added parameters to replace common block*
C* F. J. Yen/NCEP	 4/01	Added watch state abbreviation to header*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wtype, strtim, stptim, state, county (*)
	CHARACTER*(*)	adstn (*), stnnam (*), stat (*), coun (*),
     +			tbchrs (*)		        
	INTEGER		istnm (*), ispri (*), nade
	REAL		adlat (*), adlon (*), selv (*)
	LOGICAL		cntyol
C*
	CHARACTER       wloc*40, corr, watchn*5
C-----------------------------------------------------------------------
	iret = 0
C
C*	Setting up the information string.
C
	IF ( .not. cntyol ) THEN 

	    CALL ST_INCH ( icor, corr, ier )
	    CALL ST_INLN ( iwtchn, watchn, lens, ier )

	    wloc = '|'//wtype//'|'//strtim//'|'//stptim//'|'//
     +	           watchn(1:lens)//','//state//'|'//corr
C
C*	    Write the warning information to the ASCII file. 
C
	    WRITE(lunf,20)wloc
 20         FORMAT (A)
	END IF
C
C*	Compare the county ids with the county table.
C
	DO istn = 1, nade 
	    DO icnt = 1, ncnty
	        IF ( county(icnt) .eq. adstn(istn) ) THEN
	            WRITE(lunf,100) adstn(istn), istnm(istn), 
     +			    stnnam(istn), stat(istn), coun(istn), 
     +			    adlat (istn), adlon (istn), selv(istn), 
     +                      ispri(istn), tbchrs(istn)
 100		    FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                    F9.2, 1X, F9.2, 1X, F9.2, 1X, I2, 1X, A )
		END IF
	    END DO
	END DO
C*
	RETURN
	END
