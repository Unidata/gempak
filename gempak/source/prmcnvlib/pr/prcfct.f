	FUNCTION PR_CFCT ( cfrt )
C************************************************************************
C* PR_CFCT								*
C*									*
C* This function computes CLCT from CFRT.  CLCT is the numeric total	*
C* cloud cover; CFRT is the WMO fractional cloud cover table.		*
C*									*
C* REAL PR_CFCT ( CFRT )						*
C*									*
C* Input parameters:							*
C*	CFRT		REAL		Numeric total cloud cover	*
C*									*
C* Output parameters:							*
C*	PR_CFCT		REAL		WMO fractional cloud cover	*
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89						*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		itable (10)
	INCLUDE		'ERMISS.FNC'
C*
	DATA		itable / 1, 6, 6, 2, 2, 7, 3, 8, 4, 5 /
C*
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF ( ERMISS ( cfrt ) ) THEN
	    PR_CFCT = RMISSD
	    RETURN
	END IF
C
C*	Get fractional cloud cover WMO code.
C
	IF  ( ( cfrt .ge. 0 ) .and. ( cfrt .le. 9 ) )  THEN
	    icfrt = NINT ( cfrt )
	    PR_CFCT = itable ( icfrt + 1 )
	  ELSE
	    PR_CFCT = RMISSD
	END IF
C*
	RETURN
	END
