	FUNCTION PR_HGKM  ( value )
C************************************************************************
C* PR_HGKM								*
C*									*
C* This function multiplies a value by a thousand.  It can be used to	*
C* compute meters from kilometers.					*
C*									*
C* REAL PR_HGKM  ( VALUE )						*
C*									*
C* Input parameters:							*
C*	VALUE		REAL	 	Value				*
C*									*
C* Output parameters:							*
C*	PR_HGKM		REAL		Value * 1000			*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84	Original source				*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	7/88	Documentation				*
C* S. Schotz/GSC	10/89   Documentation				*
C************************************************************************
        INCLUDE    'GEMPRM.PRM'
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing values.
C*	
	IF  ( ERMISS ( value ) )  THEN
	    PR_HGKM = RMISSD
	  ELSE
	    PR_HGKM = value * 1000.
	END IF
C*
	RETURN
	END
