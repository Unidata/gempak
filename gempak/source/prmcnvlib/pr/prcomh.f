	FUNCTION PR_COMH  ( chc1, chc2, chc3 )
C************************************************************************
C* PR_COMH								*
C*									*
C* This function gets COMH from CHC1, CHC2, and CHC3.  COMH is the	*
C* combined height and numeric sky coverage for high clouds which	*
C* are those whose height is greater than 17,900 feet.			*
C*									*
C* REAL PR_COMH  ( CHC1, CHC2, CHC3 )					*
C*									*
C* Input parameters:							*
C*	CHC1		REAL		Cloud height & coverage 1	*
C*	CHC2		REAL		Cloud height & coverage 2	*
C*	CHC3		REAL		Cloud height & coverage 3	*
C*									*
C* Output parameters:							*
C*	PR_COMH		REAL		Hi combined height & coverage	*
C**									*
C* Log:									*
C* S. Schotz/GSC	10/89						*
C* M. desJardins/GSFC	 7/90	Documentation; -X always to low clouds	*
C* K. Brill/NMC		11/91	Choose the hi cld rpt of greatest cvrg  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing code at first report.
C
	PR_COMH = RMISSD
	icovr = -1
C*
	IF  ( ERMISS (chc1) )  THEN
C
C*	  Return with a missing value.
C
	  ELSE
C
C*	    Check for thin obscured combined with cloud height and
C*          coverage and get height code.
C
	    IF  ( chc1 .ge. 10000 )  THEN
                ichc1 = INT (chc1) - 10000
	      ELSE
		ichc1 = INT (chc1) 
	    END IF
	    ihght = ichc1 / 10
C
C*          Check height against high level limits.
C
	    IF  ( ihght .gt. 178 ) THEN
		PR_COMH = MOD ( chc1, 10000. )
		icovr = PR_CTCF ( FLOAT ( MOD ( ichc1, 10 ) ) )
	    END IF
C
C*          Check 2nd report.
C
	    IF  ( .not. ERMISS (chc2) )  THEN
	    	ihght = INT (chc2) / 10
		icovx = PR_CTCF ( ( chc2 - FLOAT ( ihght * 10 ) ) )
	    	IF  ( ihght .gt. 178 .and. icovx .gt. icovr )  THEN
		    PR_COMH = chc2
		    icovr = icovx
		END IF
	    END IF
C
C*          Check 3rd report.
C
	    IF  ( .not. ERMISS (chc3) )  THEN
	    	ihght = INT (chc3) / 10
		icovx = PR_CTCF ( ( chc3 - FLOAT ( ihght * 10 ) ) )
	    	IF  ( ihght .gt. 178 .and. icovx .gt. icovr )
     +		    PR_COMH = chc3
	    END IF
C*
	END IF
C*
	RETURN
	END
