	FUNCTION PR_ALTM  ( alti )
C************************************************************************
C* PR_ALTM								*
C*									*
C* This function computes ALTM from ALTI.  The following equation is	*
C* used:								*
C*									*
C*              ALTM = ALTI * 1013.25 / 29.921				*
C*									*
C* REAL PR_ALTM  ( ALTI )						*
C*									*
C* Input parameters:							*
C*	ALTI		REAL	 	Altimeter in inches		*
C*									*
C* Output parameters:							*
C*	PR_ALTM		REAL		Altimeter in millibars		*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84						*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC       7/88	Modify documentation			*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for bad data.
C
	IF  ( ERMISS ( alti ) )  THEN
	    PR_ALTM = RMISSD
	    RETURN
	END IF
C
C*	Calculate.
C
	PR_ALTM = alti * 1013.25 / 29.921
C*
	RETURN
	END
