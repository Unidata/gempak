        FUNCTION PR_CMSL ( ceil, selv )
C************************************************************************
C* PR_CMSL                                                              *
C*                                                                      *
C* This function computes CMSL, the ceiling converted to MSL (mean sea  *
C* level) in hundreds of feet.                                          *
C*                                                                      *
C* REAL PR_CMSL  ( CEIL, SELV )                                         *
C*                                                                      *
C* Input parameters:                                                    *
C*	CEIL 		REAL    Ceiling in hundreds of feet             *
C*      SELV		REAL	Station elevation in meters             *
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_CMSL         REAL    Ceiling converted to MSL in 100's of ft *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 5/03                                           *
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
	INCLUDE	  	'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ERMISS ( selv ) .or. ERMISS ( ceil ) ) THEN
	    PR_CMSL = RMISSD
	  ELSE
	    PR_CMSL = PR_HGMF ( selv ) / 100. + ceil
	END IF
C*
        RETURN
        END
