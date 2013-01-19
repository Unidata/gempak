	SUBROUTINE HC_OUT ( lunf, stype, sname, strmtim, ocnstm, advnum,
     +                      rlat, rlon, posnm, direc, speed, minpres, 
     +                      icor, sixty, fifty, thirty, seaft, flat,
     +                      flon, fstype, f34kt, f50kt, f64kt, iexflg,
     +			    isbflg, nfcst, iret )
C************************************************************************
C* HC_OUT 								*
C*									*
C* This subroutine writes hurricane, tropical storm forecast/advisory   *
C* information to an ASCII file.					*
C*                                                                      *
C* HC_OUT ( LUNF, STYPE, SNAME, STRMTIM, OCNSTM, ADVNUM, RLAT, RLON,    *
C*	    POSNM, DIREC, SPEED, MINPRES, ICOR, SIXTY, FIFTY, THIRTY,   *
C*	    SEAFT, FLAT, FLON, FSTYPE, F34KT, F50KT, F64KT, IEXFLG,     *
C*	    ISBFLG, NFCST, IRET )		                        *
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	STYPE		CHAR*		Storm type                      *
C*	SNAME		CHAR*		Storm name                      *
C*	STRMTIM		CHAR*		Valid time, GEMPAK format 	*
C*	OCNSTM		CHAR*		Ocean storm number 		*
C*	ADVNUM		CHAR*		Advisory number			*
C*	RLAT 		REAL		Latitude of points              *
C*	RLON 		REAL		Longitude of points             *
C*	POSNM		CHAR*		Position accuracy in NM		*
C*	DIREC		CHAR*		Direction of storm movement(deg)*
C*	SPEED 		CHAR*		Speed of movement (knots)	*
C*	MINPRES		CHAR*		Minimum central pressure (mb)   *
C*	ICOR		INTEGER		Correction indicator            *
C*	SIXTY		CHAR*		64 kt current quadrant string	*
C*	FIFTY		CHAR*		50 kt current quadrant string	*
C*	THIRTY		CHAR*		34 kt current quadrant string	*
C*	SEAFT		CHAR*		12 ft seas quadrant string	*
C*	FLAT(*) 	REAL		Array of forecasted latitudes   *
C*	FLON(*)		REAL		Array of forecasted longitudes  *
C*	FSTYPE(*)	CHAR*		Array of forecasted storm types *
C*	F34KT(*)	CHAR*		34kt string at all fcst times   *
C*	F50KT(*)	CHAR*		50kt string at all fcst times   *
C*	F64KT(*)	CHAR*		64kt string at all fcst times   *
C*      IEXFLG		INTEGER	 	Flag for extratropical storm	*
C*      ISBFLG		INTEGER	 	Flag for subtropical storm	*
C*      NFCST		INTEGER	 	Number of forecast times        *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 9/99						*
C* A. Hardy/GSC          5/00	Added ocean number; wind radii;sea ft   *
C* D. Kidwell/NCEP       7/01	Added fcst storm type and 34kt fcst     *
C*			        radii at 24, 48, 72 hrs		        *
C* A. Hardy/SAIC 	 8/01   Added extratropical storm flag; added   *
C*				34kt fcst radii at 12 and 36 hrs	*
C* D. Kidwell/NCEP	 3/02	Added f50kt & f64kt radii               *
C* A. Hardy/NCEP	10/02	Added check for valid lat/lon		*
C* D. Kidwell/NCEP	 2/03	Added subtropical storm flag            *
C* D. Kidwell/NCEP	 2/03	Added argument nfcst                    *
C* A. Hardy/NCEP	10/03	Added 'HUT' for TYPHOON storm type	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stype, sname, strmtim, advnum, posnm, direc, 
     +                  speed, minpres, ocnstm, sixty, fifty, thirty,
     +                  seaft, fstype(*), f34kt(*), f50kt(*), f64kt(*)
	REAL   		flat(*), flon(*)
C*
	CHARACTER	type*3, corr*1, string*80
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Construct the header string.
C
	IF ( stype .eq. 'HURRICANE' ) THEN
	    type = 'HU'
	  ELSE IF ( stype .eq. 'TYPHOON' ) THEN
	    type = 'HUT'
	  ELSE IF ( stype .eq. 'STORM' ) THEN
	    type = 'TS'
	  ELSE IF ( stype .eq. 'DEPRESSION' ) THEN
	    type = 'TD'
	  ELSE
	    type = ' '
	END IF
C
C*	Check for subtropical or extratropical storm flag.
C*	S - subtropical 
C*	E - becoming extratropical 
C*	B - both subtropical and becoming extratropical
C
        IF ( (type (1:1) .ne. ' ' ) .and. (type(:3) .ne. 'HUT' ) ) THEN
            IF ( isbflg .eq. 1 ) THEN
                type = type (:2)  // 'S'
                IF ( iexflg .eq. 1 ) type = type (:2)  // 'B'
              ELSE IF ( iexflg .eq. 1 ) THEN
                type = type (:2)  // 'E'
            END IF 
        END IF 
C
        CALL ST_LSTR ( type, lentp, ier )
        CALL ST_LSTR ( sname, lenam, ier )
        CALL ST_LSTR ( strmtim, lenstr, ier )
        CALL ST_LSTR ( ocnstm, lenocn, ier )
        CALL ST_LSTR ( advnum, lenadv, ier )
        CALL ST_LSTR ( posnm, lennm, ier )
        CALL ST_LSTR ( direc, lendir, ier )
        CALL ST_LSTR ( speed, lenspd, ier )
        CALL ST_LSTR ( minpres, lenpres, ier )
        CALL ST_INCH ( icor, corr, ier )
C
	string = '|' // type(:lentp) // '|' // strmtim ( :lenstr ) // 
     +		 '|' //sname ( :lenam) // '|' // ocnstm ( :lenocn) // 
     +           '|' // advnum ( :lenadv)// '|' // posnm ( :lennm) // 
     +           '|' // direc ( :lendir) // '|' // speed ( :lenspd) // 
     +           '|' // minpres ( :lenpres)// '|' // corr
C
	WRITE ( lunf, 10 ) string
   10	FORMAT ( A )
C
C*	Write out the current latitude and longitude points.
C
	WRITE ( lunf, 20 ) rlat, rlon, type(:2)
   20 	FORMAT ( 2F9.2, 4X, A )
C
C*	Write out the forecasted latitude and longitude and storm type
C*	for all forecast times.
C
        DO ii = 1, nfcst
            IF ( ERMISS ( flat(ii) ) .or. 
     +		      ERMISS ( flon (ii) ) ) THEN
	        flat(ii) = RMISSD 
                flon(ii) = RMISSD
                fstype(ii) = ' '
            END IF
	    WRITE ( lunf, 20 ) flat(ii), flon(ii), fstype(ii)
        END DO
C
C*	Write out the current wind speeds and quadrants.
C
	WRITE ( lunf, 30 ) sixty
	WRITE ( lunf, 30 ) fifty
	WRITE ( lunf, 30 ) thirty
	WRITE ( lunf, 30 ) seaft
   30	FORMAT ( 4X,A )
C
C*	Write out the 34kt, 50kt and 64kt radii for all forecast time 
C*	periods.
C
	DO ii = 1, nfcst
	    WRITE ( lunf, 30 ) f64kt (ii)
	    WRITE ( lunf, 30 ) f50kt (ii)
	    WRITE ( lunf, 30 ) f34kt (ii)
	END DO
C*
	RETURN
	END
