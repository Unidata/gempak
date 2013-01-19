        FUNCTION PR_WCMS ( cmsl, tcms )
C************************************************************************
C* PR_WCMS                                                              *
C*                                                                      *
C* This function computes WCMS, the hierarchical value of the ceiling   *
C* converted to MSL (mean sea level) in hundreds of feet, with a        *
C* temporary/probability value taking precedence over a prevailing one. *
C*                                                                      *
C* REAL PR_WCMS  ( CMSL, TCMS )                                         *
C*                                                                      *
C* Input parameters:                                                    *
C*      CMSL            REAL    Prevailing ceiling (MSL) in 100's of ft *
C*	TCMS 		REAL    Tempo/prob ceiling (MSL) in 100's of ft *
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_WCMS         REAL    Hierarchical ceiling (MSL), 100's of ft *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 8/03                                           *
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
	INCLUDE	  	'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( .not. ERMISS ( tcms ) ) THEN
	    PR_WCMS = tcms
	  ELSE
	    PR_WCMS = cmsl
	END IF
C*
        RETURN
        END

