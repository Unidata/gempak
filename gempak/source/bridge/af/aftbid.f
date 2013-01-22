	SUBROUTINE AF_TBID  ( ctbid, dgot, iret )
C************************************************************************
C* AF_TBID								*
C*									*
C* For AIREP and PIREP reports, this subroutine determines whether	*
C* CTBID is a known alphabetic turbulence intensity indicator and,	*
C* if so, decodes it.  If CTBID is an unknown indicator, then DGOT is	*
C* set to RMISSD.							*
C*									*
C* AF_TBID  ( CTBID, DGOT, IRET )					*
C*									*
C* Input parameters:							*
C*	CTBID		CHAR*		Alphabetic turbulence intensity	*
C*					indicator 			*
C*									*
C* Output parameters:							*
C*	DGOT		REAL		Degree of turbulence 		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Allow "SMOOTH" and "SMTH" within AIREP	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 6/99	More abbrevs., use GEMPAK turb. values  *
C* D. Kidwell/NCEP	 2/00	Added more abbreviations, restructured  *
C* D. Kidwell/NCEP	 4/00	Added more abbreviations                *
C* S. Jacobs/NCEP	 9/12	Set intensity = 0 if MTN is present	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	ctbid
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	dgot = RMISSD
C
C*	Determine the length of the input string.
C
	lctbid = LEN ( ctbid )
C
C*	Determine if the string contains a known alphabetic turbulence
C*	intensity indicator and, if so, decode it.
C*	The turbulence indicators are
C*		NONE	 = 0
C*		LIGHT	 = 2
C*		MODERATE = 4
C*		SEVERE	 = 6
C*		EXTREME	 = 8
C
	IF  ( lctbid .ge. 2 )  THEN
	    IF  ( ctbid (1:2) .eq. 'LT' )  THEN
		dgot = 2.0
		RETURN
	    END IF
	END IF
C
	IF  ( lctbid .ge. 3 )  THEN
	    IF  (  ( ctbid (1:3) .eq. 'SEV' ) .or.
     +		   ( ctbid (1:3) .eq. 'SVR' ) .or.
     +		   ( ( ctbid (1:3) .eq. 'HVY' ) .and.
     +		     ( bultyp .eq. PIREP ) )  )  THEN
		dgot = 6.0
	      ELSE IF  (  ( ctbid (1:3) .eq. 'MOD' ) .or.
     +		          ( ctbid (1:3) .eq. 'MDT' ) .or.
     +                    ( ( ctbid (1:3) .eq. 'TWO' ) .and.
     +			    ( bultyp .eq. AIREP ) )  )  THEN
		dgot = 4.0
	      ELSE IF  (  ( ctbid (1:3) .eq. 'LGT' ) .or.
     +			  ( ctbid (1:3) .eq. 'LIT' ) .or.
     +			  ( ctbid (1:3) .eq. 'LIG' ) .or.
     +                    ( ( ctbid (1:3) .eq. 'ONE' ) .and.
     +			    ( bultyp .eq. AIREP ) )  )  THEN
		dgot = 2.0
	      ELSE IF  (  ( ctbid (1:3) .eq. 'NIL' ) .or.
     +		          ( ctbid (1:3) .eq. 'NEG' ) .or.
     +			  ( ctbid (1:3) .eq. 'MTN' )  )  THEN
		dgot = 0.0
	    END IF
	    IF ( .not. ERMISS ( dgot ) )  RETURN
	END IF
C
	IF  ( lctbid .ge. 4 )  THEN
	    IF  ( ctbid (1:4) .eq. 'LGHT' )  THEN
		dgot = 2.0
	      ELSE IF  (  ( ctbid (1:4) .eq. 'SMTH' ) .or.
     +			  ( ( ctbid (1:2) .eq. 'SM' ) .and.
     +			    ( ( ctbid (3:3) .eq. 'T' ) .or.
     +			      ( ctbid (4:4) .eq. 'H' ) ) ) .or.
     +                    ( ctbid (1:4) .eq. 'NONE' )  )  THEN
		dgot = 0.0
	      ELSE IF  (  ( ctbid (1:4) .eq. 'ZERO' ) .and.
     +	   	          ( bultyp .eq. AIREP )  )  THEN
		dgot = 0.0
	      ELSE IF  ( ctbid (1:4) .eq. 'XTRM' )  THEN
		dgot = 8.0
	    END IF
	    IF ( .not. ERMISS ( dgot ) )  RETURN
	END IF
C
	IF  ( lctbid .ge. 5 )  THEN
	    IF  (  ( ctbid (1:5) .eq. 'EXTRM' )  .or.
     +             ( ctbid (1:5) .eq. 'EXTRE' )  )  THEN
		dgot = 8.0
	      ELSE IF  (  ( ctbid (1:5) .eq. 'THREE' ) .and.
     +		          ( bultyp .eq. AIREP )  )  THEN
		dgot = 6.0
	    END IF
	    IF ( .not. ERMISS ( dgot ) )  RETURN
	END IF
C
	IF  ( lctbid .ge. 6 )  THEN
	    IF  ( ctbid (1:6) .eq. 'SLIGHT' )  THEN
		dgot = 2.0
	      ELSE IF  ( ctbid (1:6) .eq. 'SMOOTH' )  THEN
		dgot = 0.0
	    END IF
	END IF
C*
	RETURN
	END
