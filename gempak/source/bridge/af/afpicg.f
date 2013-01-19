	SUBROUTINE AF_PICG  ( report, isicg, ieicg, iret )
C************************************************************************
C* AF_PICG								*
C*									*
C* This subroutine decodes and stores the icing data from within a	*
C* PIREP report.							*
C*									*
C* AF_PICG  ( REPORT, ISICG, IEICG, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISICG		INTEGER		Pointer to start of icing data	*
C*					within REPORT 			*
C*	IEICG		INTEGER		Pointer to end of icing data	*
C*					within REPORT 			*
C*									*
C* Output parameters:							*
C*	RIVALS (IRNICG)	REAL		Number of icing levels		*
C*	RIVALS (IRAFIC)	REAL		Airframe icing			*
C*	RIVALS (IRHBOI)	REAL		Base of icing in feet    	*
C*	RIVALS (IRHTOI)	REAL		Top of icing in feet  		*
C*	RIVALS (IRTPOI)	REAL		Type of icing                   *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		10/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 6/99	Added type, document for icing values   *
C* D. Kidwell/NCEP	 7/99	Used flight level if heights missing,   *
C*			        changed meters to feet in prologue      *
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
	CALL AF_BKGP  ( report ( isicg : ieicg ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	There may be multiple layers of icing data reported;
C*	if so, then each layer is separated from the others by a
C*	"like-type" group containing a "/" character.
C*	Separate these layers.
C
	CALL AF_PLYR  ( '/IC', islyr, ielyr, nlyr, ierlyr )
C
C*	Decode and store the icing data.
C
C*	Airframe icing values are stored in the interface format as
C*	GEMPAK icing numbers.
C*		NONE		= 0
C*		TRACE		= 1
C*		TRACE-LIGHT	= 2
C*		LIGHT		= 3
C*		LIGHT-MODERATE	= 4
C*		MODERATE	= 5
C*		MODERATE-SEVERE	= 7
C*		SEVERE		= 8
C
	nicg = 0
	DO ii = 1, nlyr
	    CALL AF_PILR  ( islyr (ii), ielyr (ii),
     +			    afic, hboi, htoi, tpoi, ierilr )
	    IF  ( ierilr .eq. 0 )  THEN
C
C*		If heights are missing, use flight level.
C
		IF ( ERMISS ( hboi ) .and. ERMISS ( htoi ) ) THEN
		    hboi = rivals ( irflvl )
		    htoi = rivals ( irflvl ) 
		END IF
C
		nicg = nicg + 1
		rivals ( irafic ( nicg ) ) = afic
		rivals ( irhboi ( nicg ) ) = hboi
		rivals ( irhtoi ( nicg ) ) = htoi
		rivals ( irtpoi ( nicg ) ) = tpoi
	    END IF
	END DO
	rivals ( irnicg ) = nicg
C*
	RETURN
	END
