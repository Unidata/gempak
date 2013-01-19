	FUNCTION PR_HGMK  ( value )
C************************************************************************
C* PR_HGMK								*
C*									*
C* This function divides a value by 1000.  It can be used to convert	*
C* meters to kilometers.						*
C*									*
C* REAL PR_HGMK  ( VALUE )						*
C*									*
C* Input parameters:							*
C*	VALUE		REAL		Value				*
C*									*
C* Output parameters:							*
C*	PR_HGMK		REAL		Value / 1000			*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84	Original source				*
C* G. Huffman/GSC	7/88	Documentation				*
C* S. Schotz/GSC        9/89    Documentation				*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C-----------------------------------------------------------------------
C*	Check for missing value.
C
	IF  ( ERMISS ( value ) )  THEN
	    PR_HGMK = RMISSD
	  ELSE
	    PR_HGMK = value / 1000.
	END IF
C*
	RETURN
	END
