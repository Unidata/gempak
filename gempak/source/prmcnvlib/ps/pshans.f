	FUNCTION PS_HANS  ( tc1, tc2, dwpc, itype )
C************************************************************************
C* PS_HANS								*
C*									*
C* This subroutine computes low, middle, and high elevation Haines 	*
C* Indices from TMPC and DWPC.						*
C*									*
C* PS_HANS ( TC1, TC2, DWPC, ITYPE )					*
C*									*
C* Input parameters:							*
C*	TC1		REAL		Temperature in Celsius		*
C*	TC2		REAL		Temperature in Celsius		*
C*	DWPC		REAL    	Dewpoint in Celsius		*
C*	ITYPE		INTEGER		Type of Haines index		*
C*					  1 = Low			*
C*					  2 = Middle			*
C*					  3 = High			*
C*									*
C* Output parameters:							*
C*	PS_HANS		REAL		Haines index			*
C**									*
C* Log:									*
C* T. Lee/SAIC		 6/03	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for missing data.
C
	IF  ( ( ERMISS ( tc1 ) ) .or. ( ERMISS ( tc2 ) ) .or.
     +	      ( ERMISS ( dwpc ) ) )  THEN
		PS_HANS = RMISSD
	  ELSE
C
C*	    Compute the Haines index.
C
	    IF  ( itype .eq. 1 )  THEN
		a = ( ( tc2  -  tc1  ) - 3. ) * (2./5.) + 1.
		b = ( ( tc1  - dwpc  ) - 5. ) * (2./5.) + 1.
	      ELSE IF ( itype .eq. 2 )  THEN
		a = ( ( tc1  -  tc2  ) - 5. ) * (2./6.) + 1.
		b = ( ( tc1  - dwpc  ) - 5. ) * (2./8.) + 1.
	      ELSE IF ( itype .eq. 3 )  THEN
		a = ( ( tc1  -  tc2  ) - 17.) * (2./5.) + 1.
		b = ( ( tc1  - dwpc  ) - 14.) * (2./7.) + 1.
 	    END IF
	    a = AMAX1 ( a, 0.9 )
	    a = AMIN1 ( a, 3.1 )
	    b = AMAX1 ( b, 0.9 )
	    b = AMIN1 ( b, 3.1 )
	    PS_HANS = a + b
	END IF
C*
	RETURN
	END
