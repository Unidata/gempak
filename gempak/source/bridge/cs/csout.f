	SUBROUTINE CS_OUT ( lunf, type, strtim, stptim, seq, intsy, 
     +                      dir, spd, flvl, dist, icor, rlat, rlon, 
     +                      npt, iret )
C************************************************************************
C* CS_OUT 								*
C*									*
C* This subroutine writes convective sigmet and outlook data to an 	*
C* ASCII file.  							*
C*                                                                      *
C* CS_OUT ( LUNF, TYPE, STRTIM, STPTIM, SEQ, INTSY, DIR, SPD, FLVL,     *
C*          DIST, ICOR, RLAT, RLON, NPT, IRET )				*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	TYPE		CHAR*		Convective/outlook type         *
C*	STRTIM		CHAR*		Start time, GEMPAK format       *
C*	STPTIM		CHAR*		End time, GEMPAK format         *
C*	SEQ		CHAR*		Sequence number within report   *
C*	INTSY		CHAR*		Intensity level			*
C*	DIR		CHAR*		Direction			*
C*	SPD		CHAR*		Speed (kts)			*
C*	FLVL (2)	CHAR*		Flight level(s)                 *
C*	DIST		CHAR*		Distance (nm)			*
C*	ICOR		INTEGER		Correction/test indicator       *
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
C* A. Hardy/NCEP	 8/02	Created					*
C* A. Hardy/NCEP	 1/04	Put IF condition around lat/lon DO loop *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	type, strtim, stptim, seq, intsy, dir, spd,
     +                  flvl (*), dist 
	REAL   		rlat (*), rlon (*)
C*
	CHARACTER	corr*2, string*80
C------------------------------------------------------------------------
	iret = 0
C
C*	Construct the header string.
C
	CALL ST_INCH ( icor, corr, ier )
        CALL ST_RMBL ( dist, dist, lendis, ier )
        CALL ST_RMBL ( seq, seq, lenseq, ier )
C
	CALL ST_LSTR ( strtim, lenstr, ier )
	CALL ST_LSTR ( stptim, lenstp, ier )
	CALL ST_LSTR ( intsy, lenint, ier )
	CALL ST_LSTR ( dir, lendir, ier )
	CALL ST_LSTR ( spd, lenspd, ier )
	CALL ST_LSTR ( flvl ( 1 ), lenfl1, ier )
	CALL ST_LSTR ( flvl ( 2 ), lenfl2, ier )
	CALL ST_LSTR ( corr, lencor, ier )
C
	string = '|' // type // '|' // strtim ( :lenstr ) // '|'
     +		 // stptim ( :lenstp ) // '|' // seq(:lenseq)  // '|'
     +		 // intsy ( :lenint) // '|' // dir ( :lendir ) // '|'
     +           // spd ( :lenspd ) // '|' //  flvl (1) ( :lenfl1 )
     +		 // '|' // flvl (2) ( :lenfl2 ) // '|'
     +		 // dist ( :lendis ) // '|' // corr ( :lencor ) 
C
 	CALL ST_LSTR ( string, lens, ier )
C
 	WRITE ( lunf, 10 ) string
  10	FORMAT ( A )
C
C*	Write out the points.
C
        IF ( npt .gt. 0 ) THEN
 	    DO i = 1, npt
 	        WRITE ( lunf, 20 ) rlat (i), rlon (i)
 	    END DO
  20 	    FORMAT ( 2F9.2 )
        END IF
C*
	RETURN
	END
