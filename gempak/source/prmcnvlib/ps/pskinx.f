	FUNCTION PS_KINX  ( t850, t700, t500, td850, td700 )
C************************************************************************
C* PS_KINX 								*
C*									*
C* This function computes the K index:					*
C*                                                                      *
C*      KINX = ( T850 - T500 ) + TD850 - ( T700 - TD700 )		*
C*                                                                      *
C* REAL PS_KINX ( T850, T700, T500, TD850, TD700 )             		*
C*									*
C* Input parameters:							*
C*	T850		REAL		850 mb temperature in Celsius	*
C*	T700		REAL		700 mb temperature in Celsius	*
C*	T500		REAL		500 mb temperature in Celsius	*
C*	TD850		REAL		850 mb dewpoint in Celsius	*
C*	TD700		REAL		700 mb dewpoint in Celsius	*
C*									*
C* Output parameters:							*
C*	PS_KINX		REAL		K index				*
C**	                                                                *
C* Log:									*
C* P. Kocin/CSC		1980    Originally called PR_K			*
C* M. Goodman/RDS	8/84	Modified prologue and renamed		*
C* M. desJardins/GSFC	9/84	Corrected errors; changed to Celsius	*
C* M. desJardins/GSFC	3/88	Documentation				*
C************************************************************************
        INCLUDE        'GEMPRM.PRM'
        INCLUDE        'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ERMISS ( t850 ) .or. ERMISS ( t700 ) .or.
     +	      ERMISS ( t500 ) .or. ERMISS ( td850 ) .or. 
     +	      ERMISS ( td700 ) )  THEN
	    PS_KINX = RMISSD
	  ELSE
	    PS_KINX = ( t850 - t500 ) + td850 - ( t700 - td700 )
	END IF
C*
	RETURN
	END
