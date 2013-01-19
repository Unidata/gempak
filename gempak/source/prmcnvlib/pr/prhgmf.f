	FUNCTION PR_HGMF  ( hght )
C************************************************************************
C* PR_HGMF								*
C*									*
C* This function computes HGFT from HGHT.  The following equation is	*
C* used:  								*
C*									*
C*                HGFT  =  NINT ( HGHT * 3.28084 )			*
C*									*
C* (Note:  This function rounds to the nearest foot.)			*
C*									*
C* REAL PR_HGMF  ( HGHT )						*
C*									*
C* Input parameters:							*
C*	HGHT		REAL		Height in meters		*
C*									*
C* Output parameters:							*
C*	PR_HGMF		REAL		Height in feet  		*
C**									*
C* Log:									*
C* G. Huffman/GSC	7/88	Build from PR_HTMT			*
C* S. Schotz/GSC	10/89   modify to round to nearest foot		*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for invalid data.
C
	IF  ( ERMISS ( hght ) )  THEN
	    PR_HGMF = RMISSD
	  ELSE
C
C*	    Do the conversion.
C
	    PR_HGMF = NINT(hght * 3.28084)
	END IF
C*
	RETURN
	END
