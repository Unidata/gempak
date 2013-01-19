	SUBROUTINE SC_PRRM  ( stprrm, precrm, iret )
C************************************************************************
C* SC_PRRM							        *
C*								        *
C* This subroutine decodes precipitation amount in hundredths of inches.*
C*								        *
C* SC_PRRM  ( STPRRM, PRECRM, IRET )     	     		        *
C*								        *
C* Input parameters:						        *
C*	STPRRM		CHAR*		Precipitation string		*
C*									*
C* Output parameters:						        *
C*	PRECRM		REAL		Precipitation amount	        *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C*					  -2 = decode error 	        *
C*								        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 3/97	Based on MT_PRRM                        *
C* A. Hardy/GSC         12/97   Added sccmn.cmn for interface           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE         'sccmn.cmn'
C*
	CHARACTER*(*) 	stprrm
C*
C------------------------------------------------------------------------
	iret = 0
C
C*      Check for an indeterminate amount of precipitation.
C
	IF ( stprrm ( 2:5 ) .eq. '////' ) THEN
	    precrm = RMISSD
	  ELSE
	    CALL ST_CRNM ( stprrm ( 2:5 ), precrm, iret )
	    IF ( iret. eq. 0 ) precrm = precrm * .01
	END IF
C*
	RETURN
	END
