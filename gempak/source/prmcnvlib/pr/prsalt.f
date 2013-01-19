	FUNCTION PR_SALT  ( alti )
C************************************************************************
C* PR_SALT								*
C*									*
C* This function computes SALT from ALTI.  SALT is a standard		*
C* abbreviated pressure code.  ALTI, the altimeter setting in		*
C* inches is converted to ALTM, the altimeter in millibars.  Then	*
C* ALTM is multiplied by 10, truncated, and the original thousand	*
C* and hundred digits are dropped.  The following equation is used:	*
C*									*
C*              SALT = NINT ( MOD ( ALTM, 100 ) * 10 )			*
C*									*
C* REAL PR_SALT  ( ALTI )						*
C*									*
C* Input parameters:							*
C*	ALTI		REAL		Altimeter setting in inches	*
C*									*
C* Output parameters:							*
C*	PR_SALT		REAL		Abbreviated standard altimeter	*
C**									*
C* Log:									*
C* M. Goodman/RDS	 2/85	Original source				*
C* M. desJardins/GSFC	 9/85	Corrected				*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 7/88	Documentation				*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( alti ) )  THEN
	    pr_salt = RMISSD
	  ELSE
C
C*	    Convert altimeter to millibars,
C
	    altm = PR_ALTM ( alti )
C
C*	    Drop the leading thousand and/or hundreds digits.
C
	    aalt = AMOD ( altm, 100. )
C
C*	    Include the tenths digit.
C
	    aalt = aalt * 10.
	    PR_SALT = ANINT ( aalt ) 
	END IF
C*
	RETURN
	END
