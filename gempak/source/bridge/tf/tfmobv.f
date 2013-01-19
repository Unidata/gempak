	SUBROUTINE TF_MOBV  ( stntbl, stid, obscmt, nstn, iret )
C************************************************************************
C* TF_MOBV							        *
C*							 	        *
C* This subroutine gets the mountain obscuration values from the        *
C* additional parameters portion of the station table.                  *
C*								        *
C* TF_MOBV  ( STNTBL, STID, OBSCMT, NSTN, IRET )                        *
C*								        *
C* Input parameters:						        *
C*	STNTBL		CHAR*		Station table   	        *
C*								        *
C* Output parameters:						        *
C*	STID (*)	CHAR*		Station ID                      *
C*	OBSCMT (*)	REAL		Mountain obscuration value      *
C*	NSTN		INTEGER		Number of stations              *
C*	IRET		INTEGER		Return code
C*					  0 = normal return	        *
C*					-13 = Cannot open station table *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP 	 5/03	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stntbl, stid (*)
	REAL		obscmt (*)
C*
        CHARACTER       stat (LLSTFL)*2, coun (LLSTFL)*2, 
     +			stnnam (LLSTFL)*32, tbchrs (LLSTFL)*20
        INTEGER         isnm (LLSTFL), ispri (LLSTFL)
        REAL            slat (LLSTFL), slon (LLSTFL), selv (LLSTFL)
C-----------------------------------------------------------------------
	iret = 0
C
	CALL DC_STNS ( stntbl, stid, isnm, stnnam, stat, coun, slat,
     +		       slon, selv, ispri, tbchrs, nstn, ier ) 
C
	IF ( ier .eq. 0 ) THEN
	    DO ii = 1, nstn
		CALL ST_CRNM ( tbchrs ( ii ), obscmt ( ii ), ier )
	    END DO
	  ELSE
	    nstn = 0
	    iret = -13
	    CALL DC_WLOG ( 0, 'DC', iret, stntbl, ier )
	END IF
C*
	RETURN
	END
