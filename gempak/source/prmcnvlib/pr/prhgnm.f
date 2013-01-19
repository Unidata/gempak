	FUNCTION PR_HGNM  ( hgtn )
C************************************************************************
C* PR_HGNM								*
C*									*
C* This function computes a distance in meters, given a distance in     *
C* nautical miles.  The following equation is used:                     *
C*									*
C*                HGTM  =  1852. * HGTN	                 		*
C*									*
C* REAL PR_HGNM  ( HGTN )						*
C*									*
C* Input parameters:							*
C*	HGTN		REAL		Distance in nautical miles      *
C*									*
C* Output parameters:							*
C*	PR_HGNM		REAL		Distance in meters              *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	11/99	                      			*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for invalid data.
C
	IF  ( ERMISS ( hgtn ) )  THEN
	    PR_HGNM = RMISSD
	  ELSE
C
C*	    Do the conversion.
C
	    PR_HGNM = 1852. * hgtn
	END IF
C*
	RETURN
	END
