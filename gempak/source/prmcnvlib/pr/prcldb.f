	REAL FUNCTION PR_CLDB  ( ceil, chc1, chc2, chc3 )
C************************************************************************
C* PR_CLDB								*
C*									*
C* This function returns the lowest ceiling from ceil, chc1, chc2 and	*
C* chc3. If there is no ceiling, get the lowest layer from chc1, chc2,	*
C* and chc3.								* 
C*									*
C* REAL PR_CLDB  ( CEIL, CHC1, CHC2, CHC3 )				*
C*									*
C* Input parameters:							*
C*	CEIL		REIL		Ceiling in hundreds of feet	*
C*	CHC1		REAL		Cloud height & coverage 1	*
C*	CHC2		REAL		Cloud height & coverage 2	*
C*	CHC3		REAL		Cloud height & coverage 3	*
C*									*
C* Output parameters:							*
C*	PR_CLDB		REAL		The lowest ceiling combined	*
C*					with a numeric cloud coverage	*
C**									*
C* Log:									*
C* M. Li/SAIC		07/06 		Created				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_CLDB = RMISSD
	temp    = RMISSD 
C*
	IF  ( .not. ERMISS (ceil) ) THEN
	   IF ( .not. ERMISS (chc1) .and.
     +         ABS ( ceil - INT(chc1/10) ) .lt. RDIFFD )  THEN
	       PR_CLDB = chc1
	     ELSE IF ( .not. ERMISS (chc2) .and.
     +           ABS ( ceil - INT(chc2/10) ) .lt. RDIFFD )  THEN
		 PR_CLDB = chc2
	     ELSE IF ( .not. ERMISS (chc3) .and.
     +           ABS ( ceil - INT(chc3/10) ) .lt. RDIFFD )  THEN
		 PR_CLDB = chc3
	   END IF
C
	   RETURN
	END IF
C
C*	Check the first report.
C*	
	IF ( .not. ERMISS (chc1) ) temp = chc1 
C
C*	Check 2nd report.
C
	IF ( .not. ERMISS (chc2) .and. .not. ERMISS (chc1) 
     +        .and. chc1 .gt. chc2 ) 
     +      temp = chc2
C
C*	Check 3rd report.
C
	IF ( .not. ERMISS (chc3) .and. .not. ERMISS (temp)
     +        .and. temp .gt. chc3  ) 
     +      temp = chc3
C*
	IF ( .not. ERMISS (temp) ) PR_CLDB = temp 
C*
	RETURN
	END
