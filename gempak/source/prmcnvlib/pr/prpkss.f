	FUNCTION PR_PKSS  ( pspd ) 
C************************************************************************
C* PR_PKSS								*
C*									*
C* This function computes SPED from PSPD.  PSPD is in the form DDFFF,	*
C* where DD is the wind direction in tens of degrees, and FFF is	*
C* either the wind speed or the wind speed plus 500, depending on	*
C* the unit digit of direction rounded to the nearest 5 degrees.	*
C* The following equation is used:					*
C*									*
C*            SPED = MOD ( INT (PSPD) , 500 )				*
C*									*
C* REAL PR_PKSS  ( PSPD )						*
C*									*
C* Input parameters:							*
C*	PSPD		REAL		Packed speed and direction	*
C*									*
C* Output parameters:							*
C*	PR_PKSS		REAL		Wind speed in knots		*
C**									*
C* Log:									*
C* I. Graffman/CSC	8/83	Original source code			*
C* G. Huffman/GSC	8/88	Documentation and modulus fix		*
C************************************************************************
        INCLUDE  'GEMPRM.PRM'
        INCLUDE  'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing values.
C
	IF  ( ERMISS (pspd) )  THEN
	    PR_PKSS = RMISSD
C
C*      Formally, SPED is MOD ( PSPD, 1000 ), with 500 subtracted off if
C*      needed, but 1000 is a multiple of 500, so it is done in 1 step.
C
	  ELSE
	    PR_PKSS = MOD ( IFIX (pspd), 500 )
	END IF
C*
	RETURN
	END
