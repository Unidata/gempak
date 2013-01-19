	FUNCTION PS_CTOT  ( td850, t500 )
C************************************************************************
C* PS_CTOT								*
C*									*
C* This function computes the cross totals index:			*
C*									*
C*     CTOT = TD850 - T500 						*
C*									*
C* REAL PS_CTOT ( TD850, T500 )						*
C*									*
C* Input parameters:							*
C*	TD850		REAL		850 mb dewpoint in Celsius	*
C*	T500		REAL		500 mb temperature in Celsius	*
C*									*
C* Output parameters:							*
C*	PS_CTOT		REAL		Cross totals index		*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/84	Original source				*
C* M. desJardins/GSFC	 3/88	Documentation				*
C************************************************************************
        INCLUDE        'GEMPRM.PRM'
        INCLUDE        'ERMISS.FNC'
C------------------------------------------------------------------------------
	IF  ( ERMISS (td850) .or. ERMISS (t500) )  THEN
	    PS_CTOT = RMISSD
	  ELSE
C
C*	    Compute the cross totals index by subtracting 500 mb 
C*	    temperature from the 850 mb dewpoint.
C
	    PS_CTOT = td850 - t500
	END IF
C*
	RETURN
	END
