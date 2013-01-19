	FUNCTION PS_VTOT  ( t850, t500 )
C************************************************************************
C* PS_VTOT								*
C*									*
C* This function computes the vertical totals index:			*
C*									*
C*      VTOT = T850 - T500						*
C*									*
C* REAL PS_VTOT  ( T850, T500 )						*
C*									*
C* Input parameters:							*
C*	T850		REAL		850 mb temperature in Celsius	*
C*	T500		REAL		500 mb temperature in Celsius	*
C*									*
C* Output parameters:							*
C*	PS_VTOT		REAL		Vertical totals index		*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/84	Original source				*
C* M. desJardins/GSFC	 3/88	Documentation				*
C************************************************************************
        INCLUDE       'GEMPRM.PRM'
        INCLUDE       'ERMISS.FNC'
C-----------------------------------------------------------------------------
	IF  ( ERMISS (t850) .or. ERMISS (t500) )  THEN
	    PS_VTOT= RMISSD
	  ELSE
C
C*	    Compute vertical totals index by subtracting temperature at
C*	    500 mb from temperature at 850 mb.
C
	    PS_VTOT= t850 - t500
	END IF
C*
	RETURN
	END
