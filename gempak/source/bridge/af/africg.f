	SUBROUTINE AF_RICG  ( report, lenr, irptr, iret )
C************************************************************************
C* AF_RICG								*
C*									*
C* This subroutine decodes and stores the icing data from within	*
C* a RECCO report.							*
C*									*
C* AF_RICG  ( REPORT, LENR, IRPTR, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		RECCO report 			*
C*	LENR		INTEGER		Length of REPORT 		*
C*									*
C* Input and output parameters:						*
C*	IRPTR		INTEGER		Pointer within REPORT 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRNICG)	REAL		Number of icing levels		*
C*	RIVALS (IRAFIC)	REAL		Airframe icing			*
C*	RIVALS (IRHBOI)	REAL		Base of icing in feet  		*
C*	RIVALS (IRHTOI)	REAL		Top of icing in feet  		*
C*	RIVALS (IRTPOI)	REAL		Type of icing                   *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = critical error in report	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		01/97						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 6/99	Added type, change afic codes           *
C* D. Kidwell/NCEP	 7/99	Changed meters to feet in prologue      *
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C* J. Ator/NCEP		01/02	Use PR_HCDM				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	field*(MXLENF)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = -1
	afic = RMISSD
	hboi = RMISSD
	htoi = RMISSD
	tpoi = RMISSD
C
C*	The next group should contain information on the rate of icing
C*	and the type of icing that occurred.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  (  ( ier .ne. 0 ) .or. ( lenf .ne. 5 ) .or.
     +		( field (1:1) .ne. '7' )  )  THEN
	    RETURN
	END IF
	CALL ST_INTG  ( field (2:2), iir, ier )
	CALL ST_INTG  ( field (3:3), iit, ier )
	IF  ( iit .eq. 0 )  THEN
	    afic = 0.0
	  ELSE IF  ( ( iir .ge. 7 ) .and. ( iir .le. 9 ) )  THEN
	    afic = FLOAT ( iir * 2 - 11 )
	    IF ( afic .gt. 6. ) afic = 8.
	    IF  ( ( iit .ge. 1 ) .and. ( iit .le. 9 ) )  THEN
		tpoi = FLOAT ( iit + 3 )
	    END IF
	END IF
C
C*	The next group should contain information on the altitude of
C*	the layer in which the icing occurred.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ( ier .eq. 0 ) .and. ( lenf .eq. 5 ) .and.
     +		( field (1:1) .eq. '7' )  )  THEN
	    iret = 0
	    hboi = PR_HGMF ( PR_HCDM ( field (2:3) ) )
	    htoi = PR_HGMF ( PR_HCDM ( field (4:5) ) )
	END IF
C
	IF  ( .not. ERMISS ( afic ) )  THEN
C
C*	    Store the icing data.
C
C*	    Airframe icing values are stored in the interface format 
C*	    using the following code figures -
C*		NONE     = 0
C*		LIGHT    = 3
C*		MODERATE = 5
C*		SEVERE   = 8
C*	    Type of icing is from WMO RECCO Code Table 4-7, plus 3.
C*	    (Icing types 1, 2 and 3 are reserved for rime, clear and
C*	    mixed, respectively.)
C
	    rivals ( irnicg ) = 1
	    rivals ( irafic (1) ) = afic
	    rivals ( irhboi (1) ) = hboi
	    rivals ( irhtoi (1) ) = htoi
	    rivals ( irtpoi (1) ) = tpoi
	END IF
C*
	RETURN
	END
