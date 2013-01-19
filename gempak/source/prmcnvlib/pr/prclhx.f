	FUNCTION PR_CLHX ( comx )
C************************************************************************
C* PR_CLHX								*
C*									*
C* This function gets CLHx from COMx, where x represents the L (Low),	*
C* M (Mid), or H (High) cloud level.  COMx is the cloud height		*
C* (in hundreds of feet) * 10 + the numeric cloud coverage code.	*
C*									*
C* REAL PR_CLHX ( COMX )						*
C*									*
C* Input parameters:							*
C*	COMX		REAL		Combined height and coverage	*
C*									*
C* Output parameters:							*
C*	PR_CLHX		REAL		Cloud height in feet * 100	*
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89						*
C* S. Schotz/GSC	11/89	corrected for partially	(thin)		*
C*					obscured case			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	INTEGER		icomx
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing code.
C
	IF  ( ERMISS ( comx ) .or. ( comx .lt. 10 ) )  THEN
	    PR_CLHX = RMISSD
	    RETURN
	END IF
C
C*	Check for partially obscured case.
C
	IF  ( comx .gt. 10000 )  THEN
	    icomx = INT ( comx ) - 10000
	  ELSE
	    icomx = INT ( comx)
	END IF
C
C*	Get cloud height in hundreds of feet.
C
	PR_CLHX = icomx / 10
C*
	RETURN
	END
