	FUNCTION PR_WPHF  ( powv, howv, group )
C************************************************************************
C* PR_WPHF								*
C*									*
C* This function computes WPHF, the combined wave period and wave       *
C* height in feet, from POWV and HOWV.  A group number is prefixed to   *
C* the combined value to distinguish among wind waves, predominant swell*
C* waves, and secondary swell waves.                                    *
C* The following equation is used:                                      *
C*									*
C*       WPHF = PR_WVPH ( POWV, PR_HGMF ( HOWV ), GROUP )               *
C*									*
C* REAL PR_WPHF  ( POWV, HOWV, GROUP )					*
C*									*
C* Input parameters:							*
C*	POWV		REAL		Wave period in seconds          *
C*	HOWV 		REAL		Wave height in meters    	*
C*	GROUP		REAL		Wave group number               *
C*					  2 = wind waves                *
C*					  4 = predominant swell waves   *
C*					  5 = secondary swell waves     *
C*									*
C* Output parameters:							*
C*	PR_WPHF		REAL		Combined wave period and height *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/99	                                        *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ( .not. ERMISS ( powv ) ) .and. 
     +       ( .not. ERMISS ( howv ) ) .and.
     +       ( .not. ERMISS ( group ) ) ) THEN
	    PR_WPHF = PR_WVPH ( powv, PR_HGMF ( howv ), group )
	  ELSE
	    PR_WPHF = RMISSD
	END IF
C*
	RETURN
	END
