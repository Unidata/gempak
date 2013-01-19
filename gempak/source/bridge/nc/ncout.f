	SUBROUTINE NC_OUT ( lunf, type, strtim, stptim, sname, flvl,
     +			    icor, rlat, rlon, npt, iret )
C************************************************************************
C* NC_OUT 								*
C*									*
C* This subroutine writes non-convective sigmet data to an ASCII file.  *
C*                                                                      *
C* NC_OUT ( LUNF, TYPE, STRTIM, STPTIM, SNAME, FLVL, ICOR,              *
C* RLAT, RLON, NPT, IRET )    			                	*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	TYPE		CHAR*		Sigmet type                     *
C*	STRTIM		CHAR*		Start time, GEMPAK format       *
C*	STPTIM		CHAR*		End time, GEMPAK format         *
C*	SNAME		CHAR*		Sigmet name                     *
C*	FLVL (2)	CHAR*		Flight level(s)                 *
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
C* D. Kidwell/NCEP	 8/00	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	type, strtim, stptim, sname, flvl (*)
	REAL   		rlat (*), rlon (*)
C*
	CHARACTER	corr*1, string*50
C------------------------------------------------------------------------
	iret = 0
C
C*	Construct the header string.
C
	CALL ST_INCH ( icor, corr, ier )
	CALL ST_LSTR ( strtim, lenstr, ier )
	CALL ST_LSTR ( stptim, lenstp, ier )
	CALL ST_LSTR ( sname, lensnm, ier )
	CALL ST_LSTR ( flvl ( 1 ), lenfl1, ier )
	CALL ST_LSTR ( flvl ( 2 ), lenfl2, ier )
	string = '|' // type // '|' // strtim ( :lenstr ) // '|'
     +		 // stptim ( :lenstp ) // '|' // sname ( :lensnm )
     +		 // '|' // flvl (1) ( :lenfl1 )
     +		 // '|' // flvl (2) ( :lenfl2 ) // '|'
     +		 // corr
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
