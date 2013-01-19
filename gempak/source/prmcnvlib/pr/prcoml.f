	FUNCTION PR_COML  ( chc1, chc2, chc3 )
C************************************************************************
C* PR_COML								*
C*									*
C* This function gets COML from CHC1, CHC2, and CHC3.  COML is the	*
C* combined height and numeric sky coverage for low clouds which	*
C* are those whose height is less than 63,000 feet.			*
C*									*
C* REAL PR_COML  ( CHC1, CHC2, CHC3 )					*
C*									*
C* Input parameters:							*
C*	CHC1		REAL		Cloud height & coverage 1	*
C*	CHC2		REAL		Cloud height & coverage 2	*
C*	CHC3		REAL		Cloud height & coverage 3	*
C*									*
C* Output parameters:							*
C*	PR_COML		REAL		Low combined height & coverage	*
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89						*
C* M. desJardins/GSFC	 7/90	Documentation; -X always to low clouds	*
C* K. Brill/NMC		11/91	Choose low cld rprt of greatest cvrg	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing code at first report.
C
	PR_COML = RMISSD
	icovr = -1 
C*
	IF  ( ERMISS (chc1) )  THEN
C
C*          Return with a missing value.
C
	  ELSE IF  ( chc1 .eq. 10000 )  THEN
C
C*	    Partially obscured conditions reported.
C
	    PR_COML = 9
C*
	  ELSE
C
C*	    Check for thin obscured combined with cloud height and
C*          coverage and get height code
C
	    IF  ( chc1 .gt. 10000 )  THEN
		ichc1 = INT (chc1) - 10000
	      ELSE
		ichc1 = INT (chc1) 
	    END IF
	    ihght = ichc1 / 10
C
C*	    Check height against low-level limit.
C
	    IF  ( ihght .le. 63 )  THEN
	        PR_COML = chc1
	    	icovr = PR_CTCF ( FLOAT ( MOD ( ichc1, 10 ) ) )
	      ELSE IF  ( chc1 .gt. 10000 )  THEN
		PR_COML = 9
	    END IF
C
C*	    Check 2nd cloud report.
C
	    IF ( .not. ERMISS ( chc2 ) ) THEN
		ihght = INT ( chc2 ) / 10
	        icovx = PR_CTCF ( ( chc2 - FLOAT ( ihght * 10 ) ) )
		IF ( ihght .le. 63 .and. icovx .gt. icovr ) THEN
		    PR_COML = chc2
		    icovr = icovx
		END IF
	    END IF
C
C*	    Check 3rd cloud report.
C
	    IF ( .not. ERMISS ( chc3 ) ) THEN
		ihght = INT ( chc3 ) /10
	        icovx = PR_CTCF ( ( chc3 - FLOAT ( ihght * 10 ) ) )
		IF ( ihght .le. 63 .and. icovx .gt. icovr )
     +		    PR_COML = chc3
	    END IF
        END IF
C*
	RETURN
	END
