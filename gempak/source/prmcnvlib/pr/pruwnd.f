	FUNCTION PR_UWND  ( sped, drct )
C************************************************************************
C* PR_UWND 								*
C*									*
C* This function computes UWND from SPED and DRCT or UKNT from SKNT	*
C* and DRCT.  The following equation is used:				*
C*									*
C*                  UWND = -SIN ( DRCT ) * SPED				*
C*									*
C* REAL PR_UWND  ( SPED, DRCT )						*
C*									*
C* Input parameters:							*
C*	SPED		REAL		Wind speed			*
C*	DRCT		REAL		Wind direction in degrees	*
C*									*
C* Output parameters:							*
C*	PR_UWND		REAL		U component			*
C**									*
C* Log:									*
C* P. Kocin/914		1980	Original source code			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( sped ) .or. ERMISS ( drct ) )  THEN
	    PR_UWND = RMISSD
	  ELSE
	    PR_UWND = (-SIN( drct * DTR )) * sped
	END IF
C*
	RETURN
	END
