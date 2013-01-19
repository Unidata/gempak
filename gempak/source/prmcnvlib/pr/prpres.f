	FUNCTION PR_PRES  ( tmpc, thta )
C************************************************************************
C* PR_PRES      							*
C*									*
C* This function computes PRES from TMPC and THTA.  Poisson's equation	*
C* is used:								*
C*									*
C*     PRES = 1000. * ( PR_TMCK (TMPC) / THTA ) ** (1 / RKAPPA)		*
C*									*
C* REAL PR_PRES  ( TMPC, THTA )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL     	Temperature in Celsius		*
C*	THTA		REAL     	Potential temperature in Kelvin	*
C*									*
C* Output parameters:							*
C*	PR_PRES		REAL		Station pressure in millibars	*
C**									*
C* Log:									*
C* M. Goodman/RDS	8/84	Cleaned code				*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC       7/88	Documentation; bounds on tmpc, thta	*
C* G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ( ERMISS (tmpc) ) .or. ( ERMISS (thta) )
     *        .or. ( tmpc .le. -TMCK ) .or. ( thta .le. 0.) )  THEN
	    PR_PRES = RMISSD
	  ELSE
	    tmpk = PR_TMCK (tmpc)
	    PR_PRES = 1000. * (tmpk / thta) ** (1 / RKAPPA)
	END IF
C*
	RETURN
	END
