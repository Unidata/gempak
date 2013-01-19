	FUNCTION PR_HGFM  ( hgft )
C************************************************************************
C* PR_HGFM								*
C*									*
C* This function computes HGHT from HGFT.  The following equation is	*
C* used:								*
C*									*
C*                   HGHT  =  HGFT * .3048				*
C*									*
C* REAL PR_HGFM  ( HGFT )						*
C*									*
C* Input parameters:							*
C*	HGFT		REAL		Height in feet			*
C*									*
C* Output parameters:							*
C*	PR_HGFM		REAL		Height in meters		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/87						*
C* G. Huffman/GSC	 7/88	rename HGFT, documentation		*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for invalid data.
C
	IF  ( ERMISS ( hgft ) ) THEN
	    PR_HGFM = RMISSD
	  ELSE
C
C*	    Multiply by .3048.
C
	    PR_HGFM = hgft * .3048
	END IF
C*
	RETURN
	END
