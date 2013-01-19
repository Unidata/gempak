	FUNCTION PR_KNMS  ( sknt )
C************************************************************************
C* PR_KNMS								*
C*									*
C* This function computes SPED from SKNT.  The following equation is	*
C* used:								*
C*									*
C*               SPED = SKNT / 1.9425					*
C*									*
C* REAL PR_KNMS  ( SKNT )						*
C*									*
C* Input parameters:							*
C*	SKNT		REAL		Speed in knots			*
C*									*
C* Output parameters:							*
C*	PR_KNMS		REAL		Speed in meters/second		*
C**									*
C*Log:									*
C* I. Graffman/CSC	 8/83	Original source code			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 7/88	Documentation				*
C* L. Sager/NCEP	 4/96	Updated knots to m/sec conversion 	*
C*				factor to NCEP standard			*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing data.
C
	IF  ( ERMISS ( sknt ) )  THEN
	    PR_KNMS = RMISSD
	  ELSE
	    PR_KNMS = sknt / 1.9425
	END IF
C*
	RETURN
	END
