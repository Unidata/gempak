	FUNCTION PR_WVIS ( wmovis )
C************************************************************************
C* PR_WVIS								*
C*									*
C* This subroutine translates a WMO visibility code into visibility	*
C* in kilometers.							*
C*									*
C* REAL PR_WVIS ( WMOVIS )						*
C*									*
C* Input parameters:							*
C*	WMOVIS		REAL		WMO visibility code		*
C*									*
C* Output parameters:							*
C*	PR_WVIS		REAL		Visibility in km		*
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
	REAL		vis90 (10)
	INCLUDE		'ERMISS.FNC'
	DATA		vis90 / 0., .1, .2, .5, 1., 2., 4., 10., 20.,
     +				50. /
C*
C------------------------------------------------------------------------
	IF  ( ERMISS ( wmovis ) ) THEN
	    PR_WVIS = RMISSD
	  ELSE
C
C*	    Values between 0 - 50 translate into tenths of kilometers.
C
	    IF  ( ( wmovis .ge. 0. ) .and. ( wmovis .le. 50. ) ) THEN
		PR_WVIS = wmovis / 10.
C
C*		Values between 56 and 80 are 50 + whole kilometers.
C
	      ELSE IF ( ( wmovis .ge. 56. ) .and.
     +			( wmovis .le. 80. ) ) THEN
		PR_WVIS = wmovis - 50.
C
C*		Values from 81 - 89 are in increments of 5.
C
	      ELSE IF ( ( wmovis .ge. 81. ) .and.
     +			( wmovis .le. 89. ) ) THEN
		PR_WVIS = ( wmovis - 80. ) * 5. + 30.
C
C*		The values from 90 - 99 are in a data statement.
C
	      ELSE IF ( ( wmovis .ge. 90. ) .and.
     +			( wmovis .le. 99. ) ) THEN
		iknt = wmovis - 89
		PR_WVIS = vis90 ( iknt )
C
C*		Otherwise, return missing data.
C
	      ELSE
		PR_WVIS = RMISSD
	    END IF
	END IF
C*
	RETURN
	END
