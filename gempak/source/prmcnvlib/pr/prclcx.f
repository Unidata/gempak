	FUNCTION PR_CLCX ( comx )
C************************************************************************
C* PR_CLCX								*
C*									*
C* This function gets CLCx from COMx, where x represents the L (Low),	*
C* M (Mid), or H (High) cloud level.  COMX is the cloud height		*
C* (in hundreds of feet) * 10 + the numeric cloud coverage code.	*
C*									*
C* REAL PR_CLCX ( COMX )						*
C*									*
C* Input parameters:							*
C*	COMX		REAL		Combined height and coverage	*
C*									*
C* Output parameters:							*
C*	PR_CLCX		REAL		Numeric cloud coverage code	*
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89						*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing code
C
	IF ( ERMISS ( comx ) ) THEN
	    PR_CLCX = RMISSD
	    RETURN
	END IF
C
C*	Get numeric coverage code
C
	PR_CLCX = mod (int ( comx ), 10)
C*
	RETURN
	END
