	FUNCTION PR_MSKN  ( sped )
C************************************************************************
C* PR_MSKN								*
C*									*
C* This function computes SKNT from SPED.  The following equation is	*
C* used:					 			*
C*									*
C*                 SKNT  =  SPED * 1.9425 				*
C*									*
C* REAL PR_MSKN  ( SPED )						*
C*									*
C* Input parameters:							*
C*	SPED		REAL		Speed in meters/second		*
C*									*
C* Output parameters:							*
C*	PR_MSKN		REAL		Speed in knots			*
C**									*
C* Log:									*
C* I. Graffman/CSC	8/83	Original source code			*
C* M. Goodman/RDS  	3/84	Modified prologue 			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	7/88	Documentation				*
C* L. Sager/NCEP	4/96	Updated knots to m/sec conversion	*
C*				factor to NCEP standard			*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( sped ) )  THEN
	    PR_MSKN = RMISSD
	  ELSE
	    PR_MSKN = 1.9425 * sped
	END IF
C*
	RETURN
	END
