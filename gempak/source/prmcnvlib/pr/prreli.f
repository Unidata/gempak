	FUNCTION PR_RELI  ( tmpc, dwpc )
C************************************************************************
C* PR_RELI								*
C*									*
C* This function computes RELI from TMPC and DWPC.  The following	*
C* equation is used:							*
C*									*
C*               RELI  =  VAPR / VAPSi * 100				*
C*									*
C*                     VAPR = vapor pressure				*
C*                          = PR_VAPR ( DWPC )				*
C*                    VAPSi = saturation vapor pressure wrt ice		*
C*                          = PR_VAPI ( TMPC )				*
C*									*
C* REAL PR_RELI  ( TMPC, DWPC )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*	DWPC		REAL    	Dewpoint in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_RELI		REAL		Relative humidity wrt ice (%)	*
C**									*
C* Log:									*
C* S. Chiswell/Unidata	1/07						*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( tmpc ) .or. ERMISS ( dwpc ) )  THEN
	    PR_RELI = RMISSD
	  ELSE
C
C*	    Find the vapor pressure .
C
	    e = PR_VAPR ( dwpc )
C
C*	    Find the saturated vapor pressure with respect to ice.
C
	    esi = PR_VAPI ( tmpc )
C
C*	    Calculate relative humidity with respect to ice
C
	    PR_RELI = ( e / esi ) * 100.
	END IF
C*
	RETURN
	END
