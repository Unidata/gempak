	FUNCTION PR_WCCV ( wmoccv )
C************************************************************************
C* PR_WCCV								*
C*									*
C* This function converts WMO cloud cover fraction code to the decimal	*
C* cloud cover fraction.						*
C*									*
C* REAL PR_WCCV ( WMOCCV )						*
C*									*
C* Input parameters:							*
C*	WMOCCV		REAL		WMO cloud cover code		*
C*									*
C* Output parameters:							*
C*	PR_WCCV		REAL		Decimal cloud cover fraction	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/88						*
C* K. Brill/NMC		12/90	Documentation				*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		cover (10)
	INCLUDE		'ERMISS.FNC'
C*
	DATA		cover / 0., .1, .2, .4, .5, .6, .7, .9, 1., 1. /
C------------------------------------------------------------------------
	IF  ( ERMISS ( wmoccv ) ) THEN
	    PR_WCCV = RMISSD
	  ELSE IF ( ( wmoccv .ge. 0. ) .and. ( wmoccv .le. 9. ) ) THEN
	    iccv = NINT ( wmoccv ) + 1
	    PR_WCCV = cover ( iccv )
	  ELSE
	    PR_WCCV = RMISSD
	END IF
C*
	RETURN
	END
