        FUNCTION PR_MOBS ( cmsl, otval )
C************************************************************************
C* PR_MOBS                                                              *
C*                                                                      *
C* This function computes MOBS, the indicator that a mountain           *
C* obscuration threshold is met.                                        *
C*                                                                      *
C* REAL PR_MOBS  ( CMSL, OTVAL )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      CMSL            REAL    Ceiling converted to MSL in 100's of ft *
C*	OTVAL 		REAL    Mtn. obsc. threshold in 100's of ft     *
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_MOBS         REAL    Mountain obscur. threshold met indicator*
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 5/03                                           *
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
	INCLUDE	  	'ERMISS.FNC'
C------------------------------------------------------------------------
	IF ( ERMISS ( cmsl ) .or. ERMISS ( otval ) ) THEN
	    PR_MOBS = RMISSD
	  ELSE
	    IF ( cmsl .lt. otval ) THEN
		PR_MOBS = 1.
	      ELSE
		PR_MOBS = 0.
	    END IF
	END IF
C*
        RETURN
        END

