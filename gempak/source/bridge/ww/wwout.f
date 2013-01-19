	SUBROUTINE WW_OUT ( lunf, itype, strtim, stptim, wnum, icorr,
     +			    icancl, rlat, rlon, npt, iret )
C************************************************************************
C* WW_OUT 								*
C*									*
C* This subroutine writes watch box data to an ASCII file.              *
C*                                                                      *
C* WW_OUT ( LUNF, ITYPE, STRTIM, STPTIM, WNUM, ICORR, ICANCL, RLAT,     *
C*	    RLON, NPT, IRET )             				*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	ITYPE		INTEGER		Watch type                      *
C*	STRTIM		CHAR*		Watch start time, GEMPAK format *
C*	STPTIM		CHAR*		Watch end time, GEMPAK format   *
C*	WNUM		CHAR*		Watch number			*
C*	ICORR		INTEGER		Correction indicator            *
C*	ICANCL		INTEGER		Watch cancellation indicator    *
C*	RLAT (*)	REAL		Latitudes of points             *
C*	RLON (*)	REAL		Longitudes of points            *
C*	NPT		INTEGER		Number of points                *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 5/99	                                        *
C* D. Kidwell/NCEP	 7/99	Added replacement watch type RP         *
C* D. Kidwell/NCEP	 8/99	Added status report type ST             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wnum, strtim, stptim
	REAL   		rlat (*), rlon (*)
C*
	CHARACTER	type*2, corr*1, cancl*1, string*40
C------------------------------------------------------------------------
	iret = 0
C
C*	Construct the header string.
C
	IF ( itype .eq. 0 ) THEN
	    type = 'TN'
	  ELSE IF ( itype .eq. 1 ) THEN
	    type = 'TS'
	  ELSE IF ( itype .eq. 2 ) THEN
	    type = 'RP'
	  ELSE IF ( itype .eq. 3 ) THEN
	    type = 'ST'
	  ELSE
	    type = ' '
	END IF
	CALL ST_INCH ( icorr, corr, ier )
	CALL ST_INCH ( icancl, cancl, ier )
	CALL ST_LSTR ( strtim, lenstr, ier )
	CALL ST_LSTR ( stptim, lenstp, ier )
	string = '|' // type // '|' // strtim ( :lenstr ) // '|' //
     +		 stptim ( :lenstp ) // '|' // wnum // '|' // corr //
     +		 '|' // cancl
C
	WRITE ( lunf, 10 ) string
   10	FORMAT ( A )
C
C*	Write out the points.
C
	DO i = 1, npt
	    WRITE ( lunf, 20 ) rlat (i), rlon (i)
	END DO
   20 	FORMAT ( 2F9.2 )
C*
	RETURN
	END
