	FUNCTION PR_DWDP  ( tmpx, dpdx )
C************************************************************************
C* PR_DWDP								*
C*									*
C* This function computes DWPX from TMPX and DPDX, both of which must	*
C* be in the same units (Celsius, Kelvin, or Fahrenheit).  DWPX will	*
C* be calculated in these units.  The following equation is used:	*
C*									*
C*                    DWPX = TMPX - DPDX				*
C*									*
C* REAL PR_DWDP  ( TMPX, DPDX )						*
C*									*
C* Input parameters:							*
C*	TMPX		REAL	 	Temperature 			*
C*	DPDX		REAL	 	Dewpoint depression		*
C*									*
C* Output parameters:							*
C*	PR_DWDP		REAL		Dewpoint			*
C**									*
C* Log:									*
C* I. Graffman/RDS	3/86						*
C* G. Huffman/GSC	7/88	Documentation and variable names	*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( (.not. ERMISS (tmpx)) .and. (.not. ERMISS (dpdx)) ) THEN
	    PR_DWDP  = tmpx - dpdx
	  ELSE
	    PR_DWDP = RMISSD
	END IF
C*
	RETURN
	END
