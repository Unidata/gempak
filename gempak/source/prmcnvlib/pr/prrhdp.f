	FUNCTION PR_RHDP  ( tmpc, relh )
C************************************************************************
C* PR_RHDP								*
C*									*
C* This function computes DWPC from TMPC and RELH.  The following	*
C* equation is used:							*
C*									*
C*           DWPC = 243.5 * LN (6.112) - 243.5 * LN (VAPR) /		*
C*                  ( LN (VAPR) - LN (6.112) - 17.67 )			*
C*									*
C*                VAPR = VAPS * RELH					*
C*                VAPS = saturation vapor pressure			*
C*                     = PR_VAPR ( TMPC )				*
C*									*
C* Note: If DWPC is less than -190 degrees C, it is treated as		*
C* missing data.							*
C*									*
C* REAL PR_RHDP  ( TMPC, RELH )						*
C*									*
C* Input parameters:							*
C*	TMPC		REAL		Temperature in Celsius		*
C*	RELH		REAL		Relative humidity in percent	*
C*									*
C* Output parameters:							*
C*	PR_RHDP		REAL		Dewpoint in Celsius		*
C**									*
C* Log:									*
C* I. Graffman/RDS	7/86						*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* M. desJardins/GSFC	3/88	Fixed log of 0 problem			*
C* G. Huffman/GSC	7/88	Documentation; refixed log of 0		*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Missing values.
C
	IF  ( ERMISS ( tmpc ) .or. ERMISS ( relh ) )  THEN
	    PR_RHDP = RMISSD
	  ELSE
C
C*	    Calculate saturation vapor pressure; test for existence.
C
	    vaps = PR_VAPR ( tmpc )
	    IF  ( ERMISS ( vaps ) )  THEN
		PR_RHDP = RMISSD
		RETURN
	    END IF
C
C*	    Calculate vapor pressure.
C
	    vapr = relh * vaps / 100.
C
C*	    Calculate dewpoint.  The VAPR test prevents LOG blowups.
C
	    IF  ( vapr .lt. 1.E-30 )  THEN
	        PR_RHDP = RMISSD
	      ELSE
	        PR_RHDP = 243.5 * ( LOG (6.112) - LOG (vapr) ) /
     *                 (LOG (vapr) - LOG (6.112) - 17.67)
	    END IF
	END IF
C*
	RETURN
	END
