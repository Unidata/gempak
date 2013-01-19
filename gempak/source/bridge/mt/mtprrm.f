	SUBROUTINE MT_PRRM ( stprrm, itrace, precrm, iret )
C************************************************************************
C* MT_PRRM							        *
C*								        *
C* This subroutine will decode precipitation amount in hundredths of	*
C* inches.								*
C*								        *
C* MT_PRRM ( STPRRM, ITRACE, PRECRM, IRET )    	     		        *
C*								        *
C* Input parameters:						        *
C*	STPRRM		CHAR*		Precip string			*
C*	ITRACE		LOGICAL		Flag for trace precip check     *
C*									*
C* Output parameters:						        *
C*	PRECRM		REAL		Precip amount		        *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C*					  -2 = decode error 	        *
C*								        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	11/95	Original author		                *
C* D. Kidwell/NCEP	 9/96	Modified for indeterminate & trace amts *
C* K. Tyle/GSC	 	 1/97	Modify for trace amounts; reorganize	*
C*				header and comments			*
C* D. Kidwell/NCEP       6/97   Replaced ST_CRNM with ST_INTG           *
C* D. Kidwell/NCEP	 4/98   Changed error handling; set flag value  *
C*				for indeterminate amt; reinstated trace *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	stprrm
	LOGICAL		itrace
C------------------------------------------------------------------------
	iret = 0
C
C*      Check for indeterminate amount.
C
	IF ( stprrm ( 2:5 ) .eq. '////' ) THEN
	    precrm = -99.
	  ELSE
C
C*	    Decode precip amount.
C
	    CALL ST_INTG ( stprrm ( 2:5 ), iprec, iret )
	    IF ( iret. eq. 0 ) THEN
		precrm = FLOAT ( iprec ) * .01
C
C*		Interface value for trace precip is -1.
C
		IF ( itrace ) THEN
		    IF ( precrm .eq. 0. ) precrm = -1.
		END IF
	      ELSE
		precrm = RMISSD
	    END IF
	END IF
C*
	RETURN
	END
