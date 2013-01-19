	FUNCTION PR_VWND  ( sped, drct )
C************************************************************************
C* PR_VWND 								*
C*									*
C* This function computes VWND from SPED and DRCT or VKNT from SKNT	*
C* and DRCT.  The following equation is used:				*
C*									*
C*                VWND = -COS ( DRCT ) * SPED				*
C*									*
C* REAL PR_VWND  ( SPED, DRCT )						*
C*									*
C* Input parameters:							*
C*	SPED		REAL		Wind speed			*
C*	DRCT		REAL		Wind direction in degrees	*
C*									*
C* Output parameters:							*
C*	PR_VWND		REAL		V component			*
C**									*
C* Log:									*
C* P. Kocin/914		1980	Original source code			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
        INCLUDE   	'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( sped ) .or. ERMISS ( drct ) )  THEN
	    PR_VWND = RMISSD
	  ELSE
	    PR_VWND = (-COS( drct * DTR )) * sped
	END IF
C*
	RETURN
	END
