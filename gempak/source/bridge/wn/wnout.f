	SUBROUTINE WN_OUT  ( lunf, wtype, strtim, stptim, oristn, icor,
     +			     etn, poly, county, ncnty, adstn, istnm,
     +			     stnnam, stat, coun, adlat, adlon, selv,
     +			     ispri, tbchrs, nade, iret )
C************************************************************************
C* WN_OUT								*
C*									*
C* This subroutine writes the warning type, start time, stop time,      *
C* issuing station id, correction indicator and warning counties        *
C* to an ASCII file.							*
C*									*
C* WN_OUT ( LUNF, WTYPE, STRTIM, STPTIM, ORISTN, ICOR, ETN, POLY, 	*
C*	    COUNTY, NCNTY, ADSTN, ISTNM, STNNAM, STAT, COUN, ADLAT,	*
C*	    ADLON, SELV, ISPRI, TBCHRS, NADE, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		File record number		*
C*	WTYPE		CHAR*		Warning type			*
C*	STRTIM		CHAR*		Warning start time		*
C*	STPTIM		CHAR*		Warning stop time		*
C*	ORISTN		CHAR*		Issuing station name		*
C*	ICOR		INTEGER		Correction indicator		*
C*	ETN   		CHAR*		Event Tracking Number		*
C*	POLY		CHAR*		Storm-based warning polygon	*
C*	COUNTY(NCNTY)	CHAR*6		County names in warning area    *
C*	NCNTY		INTEGER		Number of warning counties	*
C*	ADSTN(*)	CHAR*           Station ID                      *
C*      ISTNM(*)	INTEGER         Station number                  *
C*      STNNAM(*)	CHAR*           Station name                    *
C*      STAT(*)		CHAR*           State ID                        *
C*      COUN(*)		CHAR*           Country ID                      *
C*      ADLAT(*)	REAL            Latitude                        *
C*      ADLON(*)	REAL            Longitude                       *
C*      SELV(*)		REAL            Elevation                       *
C*      ISPRI(*)	INTEGER         Priority number                 *
C*      TBCHRS(*)	CHAR*           Issuing station name            *
C*      NADE            INTEGER         Number of table entries         *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 6/99						*
C* A. Hardy/GSC		12/00	Cleaned up iret values			*
C* F. J. Yen/NCEP	 1/01	Added parameters to replace common block*
C* T. Piper/SAIC	06/07	Added storm-based polygon parameter	*
C* F. J. Yen/NCEP	 3/08	Added ETN and modified corr (CSC)	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wtype, strtim, stptim, oristn, poly, county(*),
     +			etn
	CHARACTER*(*)   adstn(*), stnnam(*), stat(*), coun(*),
     +                  tbchrs(*)
        INTEGER         istnm(*), ispri(*), nade
        REAL            adlat(*), adlon(*), selv(*)
C*
	CHARACTER       wloc*50, corr*5
C-----------------------------------------------------------------------
	iret = 0
C
C*  Setting up the information string.
C
	CALL ST_INCH ( icor, corr, ier )
	CALL ST_LSTR ( corr, lencor, ier )
	CALL ST_LSTR ( oristn, lenstn, ier )
	wloc ='|'//wtype//'|'//strtim//'|'//stptim//'|'//
     +	       oristn(:lenstn)//'|'//corr(:lencor)//'|'//etn
C
C*  Write the warning information to the ASCII file. 
C
	WRITE (lunf, 20) wloc
 20     FORMAT (A)
C
C*  Write the warning polygon to the ASCII file.
C
	CALL ST_LSTR ( poly, lenpoly, ier )
	WRITE (lunf, 20) poly(:lenpoly)
C
C*  Compare the county ids with the county table.
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
C
	RETURN
	END
