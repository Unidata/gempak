	FUNCTION PR_DDEN  ( pres, tmpc )
C************************************************************************
C* PR_DDEN								*
C*									*
C* This function computes DDEN from PRES, TMPC.  The following		*
C* equation is used:							*
C*									*
C*           DDEN = 100 * PRES / ( RDGAS * TMPK )		 	*
C*									*
C*                100:  conversion from millibars to pascals		*
C*									*
C* REAL PR_DDEN  ( PRES, TMPC )						*
C*									*
C* Input parameters:							*
C*	PRES		REAL	 	Pressure in millibars		*
C*	TMPC		REAL		Temperature in Celsius		*
C*									*
C* Output parameters:							*
C*	PR_DDEN		REAL	 	Density of dry air in kg/(m**3)	*
C**									*
C* Log:									*
C* G. Huffman/GSC	8/88	Original PR_DDEN			*
C* G. Krueger/EAI        4/96   Replaced C->K constant with TMCK	*
C************************************************************************
        INCLUDE  	'GEMPRM.PRM'
        INCLUDE  	'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for bad data.
C
	IF  ( ERMISS ( pres ) .or. ERMISS ( tmpc )
     *       .or. ( tmpc .lt. -TMCK ) )  THEN
	    pr_dden = RMISSD
	    RETURN
	END IF
C
C*	Convert temperature and compute.
C
	tmpk = PR_TMCK ( tmpc )
	PR_DDEN = 100. * pres / ( RDGAS * tmpk )
C*
	RETURN
	END
