	SUBROUTINE WP_OUT ( lunf, wtype, isutim, strtim, stptim, wnum,
     +			    nmpt, alat, alon, iret )
C************************************************************************
C* WP_OUT								*
C*									*
C* This subroutine writes the decoded watch corner point information	*
C* (watch type, start time, ending time, watch number, number of lat/lon*
C* pairs, and the lat/lon pairs) to an ASCII file.			*
C*									*
C* WO_OUT ( LUNF, ISUTIM, STRTIM, STPTIM, WNUM, NUMPT, ALAT, ALON, IRET)*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	WTYPE		CHAR*		Watch type (TN, TS, NA)		*
C*	ISUTIM		CHAR*		Issue time--GEMPAK format	*
C*	STRTIM		CHAR*		Watch start time--GEMPAK format	*
C*	STPTIM		CHAR*		Watch ending time--GEMPAK format*
C*      WNUM            CHAR*		Watch number			*
C*      NMPT            INTEGER		Number of lat/lon pairs decoded *
C*      ALAT (*)	REAL            Latitudes in degree-decimal     *
C*      ALON (*)	REAL            Longitude in degree-decimal	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wtype, isutim, strtim, stptim, wnum
        REAL            alat (*), alon (*)
C*
	CHARACTER       string*55, anmpt*3
C-----------------------------------------------------------------------
	iret = 0
C
C*	Setting up the information string.
C
	CALL ST_LSTR ( wtype, lnwtyp, ier )
	CALL ST_LSTR ( isutim, lnissu, ier )
	CALL ST_LSTR ( strtim, lnstrt, ier )
	CALL ST_LSTR ( stptim, lnstpt, ier )
	CALL ST_LSTR ( wnum, lnwnum, ier )
	CALL ST_INCH ( nmpt, anmpt, ier )
	CALL ST_LSTR ( anmpt, lnnmpt, ier )

 	string = '|' // wtype(:lnwtyp)  // '|' // isutim(:lnissu) //
     +		 '|' // strtim(:lnstrt) // '|' // stptim(:lnstpt) //
     +		 '|' // wnum(:lnwnum)   // '|' // anmpt(:lnnmpt)
C
C*	Write the WCP information to the ASCII file. 
C
	WRITE ( lunf, 20 ) string
 20     FORMAT ( A )
C
C*	Write the lat/lon pairs to the ASCII file.
C
	DO ii = 1, nmpt 
	    WRITE ( lunf, 100 ) alat(ii), alon(ii) 
 100	    FORMAT ( 2F9.2 )
	END DO
C*
	RETURN
	END
