	SUBROUTINE AF_ICID  ( cicid, afic, iret )
C************************************************************************
C* AF_ICID								*
C*									*
C* For PIREP reports, this subroutine determines whether CICID is a	*
C* known alphabetic icing intensity indicator and, if so, decodes it.	*
C* If CICID is an unknown indicator, then AFIC is set to RMISSD.	*
C*									*
C* AF_ICID  ( CICID, AFIC, IRET )					*
C*									*
C* Input parameters:							*
C*	CICID		CHAR*		Alphabetic icing intensity	*
C*					indicator 			*
C*									*
C* Output parameters:							*
C*	AFIC		REAL		Airframe icing 			*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C**									*
C* Log:									*
C* J. Ator/NP12		10/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 6/99	More abbrevs., use GEMPAK icing values  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	cicid
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	afic = RMISSD
C
C*	Determine the length of the input string.
C
	lcicid = LEN ( cicid )
C
C*	Determine if the string contains a known alphabetic icing
C*	intensity indicator and, if so, decode it.
C*	The icing intensity indicators are
C*		NONE     = 0
C*		TRACE    = 1
C*		LIGHT    = 3
C*		MODERATE = 5
C*		SEVERE   = 8
C
	IF  ( lcicid .eq. 5 )  THEN
	    IF  ( cicid (1:5) .eq. 'TRACE' )  THEN
		afic = 1.0
	      ELSE IF ( cicid (1:5) .eq. 'LIGHT' ) THEN
		afic = 3.0
	    END IF
	ELSE IF  ( lcicid .eq. 4 )  THEN
	    IF ( cicid (1:4) .eq. 'NONE' ) THEN
		afic = 0.0
	    END IF
	ELSE IF  ( lcicid .eq. 3 )  THEN
	    IF  ( ( cicid (1:3) .eq. 'SEV' ) .or.
     +		  ( cicid (1:3) .eq. 'SVR' ) )  THEN
		afic = 8.0
	    ELSE IF  ( ( cicid (1:3) .eq. 'MOD' ) .or.
     +		       ( cicid (1:3) .eq. 'MDT' ) )  THEN
		afic = 5.0
	    ELSE IF  ( cicid (1:3) .eq. 'LGT' )  THEN
		afic = 3.0
	    ELSE IF  ( ( cicid (1:3) .eq. 'NIL' ) .or.
     +		       ( cicid (1:3) .eq. 'NEG' ) )  THEN
		afic = 0.0
	    END IF
	ELSE IF  ( lcicid .eq. 2 )  THEN
	    IF ( cicid (1:2) .eq. 'LT' ) THEN
		afic = 3.0
	    END IF
	END IF
C*
	RETURN
	END
