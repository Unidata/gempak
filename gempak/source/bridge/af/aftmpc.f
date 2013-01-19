	SUBROUTINE AF_TMPC  ( csign, ctmpc, iret )
C************************************************************************
C* AF_TMPC								*
C*									*
C* This subroutine decodes and stores the temperature from AMDAR,	*
C* AIREP, and PIREP reports.						*
C*									*
C* AF_TMPC  ( CSIGN, CTMPC, IRET )					*
C*									*
C* Input parameters:							*
C*	CSIGN		CHAR*		Encoded	sign of temperature 	*
C*	CTMPC		CHAR*		Encoded temperature 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRTMPC)	REAL		Temperature in Celsius		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = error during decoding	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		03/97	Reject 3-digit temperatures within AIREP*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		09/99	Allow 1-digit temperatures within PIREP	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	csign, ctmpc
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	rmult = RMISSD
	rdiv = RMISSD
	tmpc = RMISSD
C
C*	Determine the length of the input temperature string.
C
	lctmpc = LEN ( ctmpc )
	IF  ( ( lctmpc .eq. 3 ) .and. ( bultyp .ne. AIREP ) )  THEN
	    rdiv = 10.0
	ELSE IF  ( ( lctmpc .eq. 2 ) .and. ( bultyp .ne. AMDAR ) )  THEN
	    rdiv = 1.0
	ELSE IF  ( ( lctmpc .eq. 1 ) .and. ( bultyp .eq. PIREP ) )  THEN
	    rdiv = 1.0
	END IF
C
C*	Determine the length of the input temperature sign string,
C*	and then decode the sign of the temperature accordingly.
C
	lcsign = LEN ( csign )
	IF  ( lcsign .eq. 2 )  THEN
	    IF  ( csign (1:2) .eq. 'PS' )  THEN
		rmult = 1.0
	    ELSE IF  ( csign (1:2) .eq. 'MS' )  THEN
		rmult = -1.0
	    END IF
	ELSE IF  ( ( lcsign .eq. 1 ) .and. ( bultyp .ne. AMDAR ) )  THEN
	    IF  ( ( csign (1:1) .eq. 'P' ) .or.
     +		  ( csign (1:1) .eq. '+' ) )  THEN
		rmult = 1.0
	    ELSE IF  ( ( csign (1:1) .eq. 'M' ) .or.
     +		       ( csign (1:1) .eq. '-' ) )  THEN
		rmult = -1.0
	    END IF
	END IF
C
	IF  (  ( .not. ( ERMISS ( rmult ) ) ) .and.
     +	       ( .not. ( ERMISS ( rdiv ) ) )  )  THEN 
C
C*	    Decode the temperature.
C
	    CALL ST_INTG  ( ctmpc (1:lctmpc), itmpc, ier )
	    IF  ( ier .eq. 0 )  THEN
		tmpc = ( FLOAT ( itmpc ) * rmult ) / rdiv
	    END IF
	END IF
C
	IF  ( ERMISS ( tmpc ) )  THEN
	    logmsg = 'temperature '  //  csign (1:lcsign)  //
     +			ctmpc (1:lctmpc)
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    iret = -1
	ELSE
C
C*	    Store the temperature.
C
	    rivals ( irtmpc ) = tmpc
	END IF
C*
	RETURN
	END
