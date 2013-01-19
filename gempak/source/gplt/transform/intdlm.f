	INTEGER FUNCTION INTDLM  ( x, ix1, ix2 )
C************************************************************************
C* INTDLM								*
C* 									*
C* This function rounds first argument, X, in the direction of the 	*
C* vector IX1 - IX2.  For values between IX1 and IX2, this means that 	*
C* the value returned will be the integer closest to IX1.  If the 	*
C* resulting value would be outside the range IX1 to IX2, the nearest 	*
C* boundary, IX1 or IX2, is returned.					*
C* 									*
C* INTEGER INTDLM  ( X, IX1, IX2 )					*
C* 									*
C* Input parameters:							*
C* 	X		REAL		Data value			*
C* 	IX1		INTEGER		First point			*
C* 	IX2		INTEGER		Second point			*
C* 									*
C* Output parameters:							*
C* 	INTDLM		INTEGER		Value of X rounded towards IX1	*
C**									*
C* Log:									*
C* G. Chatters/RDS	8/84						*
C* M. desJardins/GSFC	5/85						*
C************************************************************************
C------------------------------------------------------------------------
C*	Round X in the direction of IX2 to IX1.
C*	Formula used depends on sign of X and sign of IX2 - IX1.
C
	IF  ( x .lt. 0.0  .and.  ( ix2 - ix1 ) .ge. 0.0 )  THEN
	   IF  ( x .eq. REAL ( INT ( x ) ) ) THEN
	      ix = INT ( x )
	   ELSE
	      ix = INT ( x ) - 1
	   END IF
	ELSE IF ( x .ge. 0.0  .and.  ( ix2 - ix1 ) .lt. 0.0 ) THEN
	   IF ( x .eq. REAL ( INT ( x ) ) ) THEN
	      ix = INT ( x )
	   ELSE
	      ix = INT ( x ) + 1
	   END IF
	ELSE
	   ix = INT ( x )
	END IF
C
C*	If resulting value is outside range, limit it to boundary value.
C
	IF ( ( ix2 - ix1 ) .ge. 0 ) THEN
	   intdlm = MAX0 ( ix1, MIN0 ( ix, ix2 ) )
	ELSE
	   intdlm = MAX0 ( ix2, MIN0 ( ix, ix1 ) )
	END IF
C*
	RETURN
	END
