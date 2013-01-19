	FUNCTION PR_AMSL  ( pmsl )
C************************************************************************
C* PR_AMSL								*
C*									*
C* This function computes a standard abbreviated 3-digit display of	*
C* pressure containing the tens and units digits and the first digit	*
C* after the decimal point.  The input is multiplied by 10, truncated,	*
C* and the original thousand and hundred digits are dropped.  The	*
C* following equation is used:						*
C*									*
C*              AMSL = NINT ( AMOD ( PMSL, 100 ) * 10 )			*
C*									*
C* This function can be used to compute SALT from ALTM, and SMSL from 	*
C* PMSL.								*
C*									*
C* REAL PR_AMSL  ( PMSL )						*
C*									*
C* Input parameters:							*
C*	PMSL		REAL		Pressure in mb			*
C*									*
C* Output parameters:							*
C*	PR_AMSL		REAL		Standard abbreviated pressure	*
C**									*
C* Log:									*
C* M. Goodman/RDS	 2/85	Original source				*
C* M. desJardins/GSFC	 9/85	Corrected				*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 7/88	Documentation				*
C* K. F. Brill/GSC       6/89   Generalized for pmsl and altm		*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( pmsl ) )  THEN
	    pr_amsl = RMISSD
	  ELSE
C
C*	    Drop the leading thousand and/or hundreds digits.
C
	    aalt = AMOD ( pmsl, 100. )
C
C*	    Include the tenths digit.
C
	    aalt = aalt * 10.
	    PR_AMSL = ANINT ( aalt ) 
	END IF
C*
	RETURN
	END
