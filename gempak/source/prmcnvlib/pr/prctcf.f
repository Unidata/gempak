	FUNCTION PR_CTCF  ( clct )
C************************************************************************
C* PR_CTCF								*
C* 									*
C* This function computes CFRT from CLCT.  CLCT is the numeric total	*
C* cloud cover; CLCT is the WMO fractional cloud cover table.		*
C*									*
C* REAL PR_CTCF  ( CLCT )						*
C*									*
C* Input parameters:							*
C*	CLCT		REAL		WMO fractional cloud cover	*
C*									*
C* Output parameters:							*
C*	PR_CTCF		REAL		Numeric total cloud cover	*
C**									*
C* Log:									*
C* M. desJardins/NMC	10/91						*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		itable (9)
	INCLUDE		'ERMISS.FNC'
C*
	DATA		itable / 0, 3, 6, 8, 9, 2, 5, 7, 0 /
C*
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF ( ERMISS ( clct ) ) THEN
	    PR_CTCF = RMISSD
	    RETURN
	END IF
C
C*	Get fractional cloud cover WMO code.
C
	IF  ( ( clct .gt. 0 ) .and. ( clct .le. 9 ) )  THEN
	    iclct = NINT ( clct )
	    PR_CTCF = itable ( iclct )
	  ELSE
	    PR_CTCF = RMISSD
	END IF
C*
	RETURN
	END
