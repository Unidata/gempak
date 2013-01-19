	FUNCTION PR_ALTI  ( altm ) 
C************************************************************************
C* PR_ALTI								*
C*									*
C* This function computes ALTI from ALTM.  The following equation is	*
C* used:								*
C*									*
C*            ALTI = ALTM * 29.921 / 1013.25				*
C*									*
C* REAL PR_ALTI ( ALTM )						*
C*									*
C* Input parameters:							*
C*	ALTM		REAL	 	Altimeter in millibars		*
C*									*
C* Output parameters:							*
C*	PR_ALTI		REAL		Altimeter in inches		*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84						*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC       7/88    Modify documentation			*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for bad data.
C
	IF  ( ERMISS ( altm ) )  THEN
	    PR_ALTI = RMISSD
	    RETURN
	END IF
C
C*	Calculate.
C
	PR_ALTI = altm * 29.921 / 1013.25 
C*
	RETURN
	END
