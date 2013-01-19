	FUNCTION PR_MIXR  ( dwpc, pres )
C************************************************************************
C* PR_MIXR								*
C*									*
C* This function computes MIXR from DWPC and PRES.  The following	*
C* equation is used:							*
C*									*
C*         MIXR = .62197 * ( e / ( PRES - e ) ) * 1000.			*
C*									*
C*              e    =  VAPR * corr					*
C*              corr =  (1.001 + ( ( PRES - 100. ) / 900. ) * .0034)	*
C*									*
C* University of Wisconsin green sheet.					*
C*									*
C* This function can also be used for the following computations:	*
C*            MIXS from TMPC and PRES					*
C*            SMXR from DWPC and PALT					*
C*            SMXS from TMPC and PALT					*
C*									*
C* REAL PR_MIXR  ( DWPC, PRES )						*
C*									*
C* Input parameters:							*
C*	DWPC		REAL   		Dewpoint in Celsius		*
C*	PRES		REAL   		Pressure in millibars		*
C*									*
C* Output parameters:							*
C*	PR_MIXR		REAL		Mixing ratio in g/kg		*
C**									*
C* Log:									*
C* P. Kocin/914		1980	Original source code			*
C* M. Goodman/RDS	3/84	Added PR_TMCK				*
C* I. Graffman/RDS	12/84	Changed .622 to .62197			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	7/88	Documentation; check missing VAPR,	*
C*				large E at low PRES			*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( dwpc ) .or. ERMISS ( pres ) )  THEN
	    PR_MIXR = RMISSD
	  ELSE
C
C*	    Calculate vapor pressure.
C
	    vapr = PR_VAPR ( dwpc )
	    IF  ( ERMISS ( vapr ) )  THEN
	        PR_MIXR = RMISSD
		RETURN
	    END IF
C
C*	    CORR is a correction to the vapor pressure
C*	    since the atmosphere is not an ideal gas.
C
	    corr = (1.001 + (( pres - 100.) / 900.) * .0034)
	    e    = corr * vapr
C
C*	    Test for unphysical case of large E at low PRES.
C
	    IF  ( e .gt. ( .5 * pres ) )  THEN
                PR_MIXR = RMISSD
	      ELSE
C
C*	    Calculate mixing ratio.
C
	        PR_MIXR = .62197 * (e / (pres - e)) * 1000.
	    END IF
	END IF
C*
	RETURN
	END
