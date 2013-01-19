	FUNCTION PR_HGMD  ( hght )
C************************************************************************
C* PR_HGMD								*
C*									*
C* This function computes HGTD from HGHT.  The following equation is	*
C* used:								*
C*									*
C*                     HGTD = HGHT / 10.				*
C*									*
C* REAL PR_HGMD  ( HGHT )						*
C*									*
C* Input parameters:							*
C*	HGHT		REAL		Height in meters		*
C*									*
C* Output parameters:							*
C*	PR_HGMD		REAL		Height in decameters		*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84   Original source				*
C* M. Goodman/RDS  	3/86	Corrected division error		*
C* G. Huffman/GSC	7/88	Documentation				*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for invalid height.
C	
	IF  ( ERMISS (hght) )  THEN
	    PR_HGMD = RMISSD 
	  ELSE
	    PR_HGMD = hght / 10.
	END IF
C*
	RETURN
	END
