	FUNCTION PR_WTND ( ctend )
C************************************************************************
C* PR_WTND								*
C*									*
C* This subroutine translates a WMO pressure tendency code into a	*
C* pressure change in millibars.					*
C*									*
C* REAL PR_WTND ( CTEND )						*
C*									*
C* Input parameters:							*
C*	CTEND		CHAR*		WMO pressure tendency code	*
C*									*
C* Output parameters:							*
C*	PR_WTND		REAL		Pressure change in mb		*
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
	CHARACTER*(*)	ctend
C*
	CHARACTER	c*1
	REAL		psign ( 9 )
	INCLUDE		'ERMISS.FNC'
	DATA		psign / 4 * 1., 0., 4 * -1. /
C*
C------------------------------------------------------------------------
	PR_WTND = RMISSD
C
C*	Get pressure tendency characteristic.
C
	c = ctend ( 1:1 )
	IF  ( ( c .ge. '0' ) .and. ( c .le. '8' ) ) THEN
	    CALL ST_INTG ( c, itendc, ier )
	    IF  ( ctend ( 2:4 ) .ne. '999' ) THEN
		CALL ST_CRNM ( ctend ( 2:4 ), ptend, ier )
		IF  ( .not. ERMISS ( ptend ) ) THEN
		    PR_WTND = psign ( itendc + 1 ) * ptend / 10.
		END IF
	    END IF
	END IF
C*
	RETURN
	END
