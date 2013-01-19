	SUBROUTINE AM_OUT ( lunf, stype, isstim, strtim, stptim, reg,
     +			    iseq, updt, flvl, icor, icancl, rlat, rlon,
     +			    npt, iret )
C************************************************************************
C* AM_OUT 								*
C*									*
C* This subroutine writes airmet data to an ASCII file.                 *
C*                                                                      *
C* AM_OUT ( LUNF, STYPE, ISSTIM, STRTIM, STPTIM, REG, ISEQ, UPDT, FLVL, *
C*	    ICOR, ICANCL, RLAT, RLON, NPT, IRET )    			*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	STYPE		CHAR*		Airmet subtype                  *
C*	ISSTIM		CHAR*		Issue time, GEMPAK format       *
C*	STRTIM		CHAR*		Start time, GEMPAK format       *
C*	STPTIM		CHAR*		End time, GEMPAK format         *
C*	REG		CHAR*		FAA region			*
C*	ISEQ		INTEGER		Sequence number within report   *
C*	UPDT		CHAR*		Update number                   *
C*	FLVL (2)	CHAR*		Flight level(s)                 *
C*	ICOR		INTEGER		Correction/test indicator       *
C*	ICANCL		INTEGER		Airmet cancellation indicator   *
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
C* D. Kidwell/NCEP	 7/00	                                        *
C* m.gamazaychikov/SAIC	07/04	Added ISSTIM as input parameter		*
C* J. Lewis/AWC		05/05	Changed input parameter ORG to REG	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stype, isstim, strtim, stptim, reg, updt, 
     +                  flvl (*)
	REAL   		rlat (*), rlon (*)
C*
	CHARACTER	seq*2, corr*2, cancl*1, string*72
C------------------------------------------------------------------------
	iret = 0
C
C*	Construct the header string.
C
	CALL ST_INCH ( iseq, seq, ier )
	IF ( iseq .lt. 10 )  seq = '0' // seq
	CALL ST_INCH ( icor, corr, ier )
	CALL ST_INCH ( icancl, cancl, ier )
	CALL ST_LSTR ( isstim, lensts, ier )
	CALL ST_LSTR ( strtim, lenstr, ier )
	CALL ST_LSTR ( stptim, lenstp, ier )
	CALL ST_LSTR ( updt, lenupd, ier )
	CALL ST_LSTR ( flvl ( 1 ), lenfl1, ier )
	CALL ST_LSTR ( flvl ( 2 ), lenfl2, ier )
	CALL ST_LSTR ( corr, lencor, ier )
	string = '|' // stype // '|' // isstim ( :lensts) // '|' 
     +           // strtim ( :lenstr ) // '|'
     +		 // stptim ( :lenstp ) // '|' // reg // seq  // '|'
     +		 // updt ( :lenupd) // '|' // flvl (1) ( :lenfl1 )
     +		 // '|' // flvl (2) ( :lenfl2 ) // '|'
     =		 // corr ( :lencor ) // '|' // cancl
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
