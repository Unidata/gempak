	SUBROUTINE AF_RSWD  ( ddff, iret )
C************************************************************************
C* AF_RSWD								*
C*									*
C* This subroutine decodes and stores the surface wind data from within	*
C* a RECCO report.							*
C*									*
C* AF_RSWD  ( DDFF, IRET )						*
C*									*
C* Input parameters:							*
C*	DDFF		CHAR*4		Surface wind data 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRWDR1)	REAL		Sfc wind direction in degrees	*
C*	RIVALS (IRWSK1)	REAL		Sfc wind direction in knots	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		01/97						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER	ddff*4
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	sdrct = RMISSD
	ssknt = RMISSD
C
C*	Decode the surface wind speed.
C
	CALL ST_INTG  ( ddff (3:4), issknt, ier )
	IF  ( ier .eq. 0 )  THEN 
	    ssknt = FLOAT ( issknt )
	END IF
C
C*	Decode the surface wind direction.
C
	CALL ST_INTG  ( ddff (1:2), isdrct, ier )
	IF  ( ier .eq. 0 )  THEN 
	    IF  ( ( isdrct .ge. 51 ) .and. ( isdrct .le. 86 ) )  THEN
		isdrct = isdrct - 50
		IF  ( .not. ERMISS ( ssknt ) )  THEN
		    ssknt = ssknt + 100.0
		END IF
	    END IF
	    IF  ( isdrct .eq. 36 )  THEN
		sdrct = 0.0
	    ELSE IF  ( ( isdrct .ge. 1 ) .and.
     +		       ( isdrct .le. 35 ) )  THEN
		sdrct = FLOAT ( isdrct ) * 10.0
	    END IF
	END IF
C
	IF  ( ( .not. ERMISS ( sdrct ) ) .and.
     +	      ( .not. ERMISS ( ssknt ) ) )  THEN
C
C*	    Store the surface wind data.
C
	    rivals ( irwdr1 ) = sdrct
	    rivals ( irwsk1 ) = ssknt
	END IF
C*
	RETURN
	END
