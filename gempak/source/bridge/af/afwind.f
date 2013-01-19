	SUBROUTINE AF_WIND  ( cdrct, csknt, iret )
C************************************************************************
C* AF_WIND								*
C*									*
C* This subroutine decodes and stores wind direction and wind speed	*
C* from AMDAR, AIREP, PIREP, and RECCO reports.				*
C*									*
C* AF_WIND  ( CDRCT, CSKNT, IRET )					*
C*									*
C* Input parameters:							*
C*	CDRCT		CHAR*		Encoded	wind direction 		*
C*	CSKNT		CHAR*		Encoded wind speed 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRDRCT)	REAL		Wind direction in degrees	*
C*	RIVALS (IRSKNT)	REAL		Wind speed in knots		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = error during decoding	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Allow wind direction to	be 360 degrees 	*
C* J. Ator/NP12		11/96	Added capability for RECCO reports 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 5/00	Allow 1 digit wind speed for AIREPs     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	cdrct, csknt
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	drct = RMISSD
	sknt = RMISSD
C
C*	Decode the wind direction.
C
	lcdrct = LEN ( cdrct )
	IF  (  ( lcdrct .eq. 3 ) .or.
     +	      ( ( lcdrct .eq. 2 ) .and. ( bultyp .eq. RECCO ) )  )  THEN
	    CALL ST_INTG  ( cdrct (1:lcdrct), idrct, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  (  ( lcdrct .eq. 2 ) .and.
     +			( bultyp .eq. RECCO )  )  THEN
		    idrct = idrct * 10
		END IF
		IF  ( ( idrct .ge. 0 ) .and. ( idrct .le. 360 ) )  THEN
		    IF  ( idrct .eq. 360 )  THEN
			idrct = 0
		    END IF
		    drct = FLOAT ( idrct )
		END IF
	    END IF
	END IF
C
C*	Decode the wind speed.
C
	lcsknt = LEN ( csknt )
	IF  (  ( lcsknt .eq. 3 ) .or.
     +	      ( ( lcsknt .eq. 2 ) .and. ( bultyp .ne. AMDAR ) )  .or.
     +	      ( ( lcsknt .eq. 1 ) .and. ( bultyp .eq. AIREP ) ) )  THEN
	    CALL ST_INTG  ( csknt (1:lcsknt), isknt, ier )
	    IF  ( ier .eq. 0 )  THEN
		sknt = FLOAT ( isknt )
	    END IF
	END IF
C
	IF  ( ( ERMISS ( drct ) ) .or. ( ERMISS ( sknt ) ) )  THEN
	    logmsg = 'wind '  //  cdrct (1:lcdrct)  //  '/'  //
     +		csknt (1:lcsknt)
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    iret = -1
	ELSE
C
C*	    Store the wind direction and wind speed.
C
	    rivals ( irdrct ) = drct
	    rivals ( irsknt ) = sknt
	END IF
C*
	RETURN
	END
