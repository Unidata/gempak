	SUBROUTINE AF_PTRB  ( report, istrb, ietrb, iret )
C************************************************************************
C* AF_PTRB								*
C*									*
C* This subroutine decodes and stores the turbulence data from within	*
C* a PIREP report.							*
C*									*
C* AF_PTRB  ( REPORT, ISTRB, IETRB, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISTRB		INTEGER		Pointer to start of turbulence	*
C*					data within REPORT 		*
C*	IETRB		INTEGER		Pointer to end of turbulence	*
C*					data within REPORT 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRNTRB)	REAL		Number of turbulence levels	*
C*	RIVALS (IRDGOT)	REAL		Degree of turbulence		*
C*	RIVALS (IRHBOT)	REAL		Base of turbulence in feet  	*
C*	RIVALS (IRHTOT)	REAL		Top of turbulence in feet  	*
C*	RIVALS (IRFQOT)	REAL		Frequency of turbulence		*
C*	RIVALS (IRTPOT)	REAL		Type of turbulence		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 6/99	Added frequency, type, document turb.   *
C* D. Kidwell/NCEP	 7/99	Used flight level if heights missing,   *
C*				changed meters to feet in prologue      *
C* S. Jacobs/NCEP	 9/12	Set heights to missing if intensity=0	*
C*				or is itself missing			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	INTEGER		islyr ( MXNLYR ), ielyr ( MXNLYR )
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Break up the input string into groups of "like-type" in order
C*	to facilitate decoding.
C
	CALL AF_BKGP  ( report ( istrb : ietrb ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	There may be multiple layers of turbulence data reported;
C*	if so, then each layer is separated from the others by a
C*	"like-type" group containing a "/" character.
C*	Separate these layers.
C
	CALL AF_PLYR  ( '/TB', islyr, ielyr, nlyr, ierlyr )
C
C*	Decode and store the turbulence data.
C
C*	Degree of turbulence values are stored in the interface format
C*	as GEMPAK turbulence numbers.
C*		NONE		= 0
C*		LIGHT		= 2
C*		LIGHT-MODERATE  = 3
C*		MODERATE	= 4
C*		MODERATE-SEVERE	= 5
C*		SEVERE		= 6
C*		EXTREME		= 8
C
	ntrb = 0
	DO ii = 1, nlyr
	    CALL AF_PTLR  ( islyr (ii), ielyr (ii),
     +			    dgot, hbot, htot, fqot, tpot, iertlr )
	    IF  ( iertlr .eq. 0 )  THEN
C
C*		If heights are missing, use flight level.
C
		IF ( dgot .gt. 0 ) THEN
		    IF ( ERMISS ( hbot ) .and. ERMISS ( htot ) ) THEN
			hbot = rivals ( irflvl )
			htot = rivals ( irflvl )
		    END IF
		ELSE
		    hbot = RMISSD
		    htot = RMISSD
		END IF
C
		ntrb = ntrb + 1
		rivals ( irdgot ( ntrb ) ) = dgot
		rivals ( irhbot ( ntrb ) ) = hbot
		rivals ( irhtot ( ntrb ) ) = htot
		rivals ( irfqot ( ntrb ) ) = fqot
		rivals ( irtpot ( ntrb ) ) = tpot
	    END IF
	END DO
	rivals ( irntrb ) = ntrb
C*
	RETURN
	END
