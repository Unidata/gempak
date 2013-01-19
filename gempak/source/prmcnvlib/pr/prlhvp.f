	FUNCTION PR_LHVP  ( tmpc )
C************************************************************************
C* PR_LHVP 								*
C*									*
C* This function computes LHVP from TMPC.  LHVP, the latent heat of	*
C* vaporization at constant pressure, is computed as follows:		*
C*									*
C*              LHVP = ( 2.500 - .00237 * TMPC ) * 10E6			*
C*									*
C* REAL PR_LHVP  ( TMPC )						*
C*	                                                                *
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*	                                                                *
C* Output parameters:							*
C*	PR_LHVP		REAL		Latent heat in J/kg		*
C**	                                                                *
C* Log:									*
C* P. Kocin/CSC	 	1980    Original code				*
C* M. Goodman/RDS	8/84	Cleaned code				*
C* I. Graffman/RDS   	11/84 	Renamed from PR_HEAT			*
C* I. Graffman/RDS   	12/87	GEMPAK4					*
C* G. Huffman/GSC	7/88	Documentation				*
C* K. Brill/NMC		2/92	Change 10e6 to 1.0e6			*
C* L. Sager/NCEP	4/96	Updated factor for latent heat of	* 
C*				vaporization to NCEP standard		*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( tmpc ) )  THEN
	    PR_LHVP = RMISSD
	  ELSE
	    PR_LHVP = (2.500 - .00237 * TMPC) * 1.0E6
	END IF
C*
	RETURN
	END
