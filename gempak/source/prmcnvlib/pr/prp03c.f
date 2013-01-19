	FUNCTION PR_P03C ( p03d )
C************************************************************************
C* PR_P03C								*
C*									*
C* This subroutine extracts the pressure change in millibars from	*
C* the pressure tendency information.					*
C*									*
C* REAL PR_P03C ( P03D )						*
C*									*
C* Input parameters:							*
C*	P03D		REAL		Pressure tendency information	*
C*									*
C* Output parameters:							*
C*	PR_P03C		REAL		Pressure change in mb		*
C**									*
C* Log:									*
C* K. Brill/NMC		11/91						*
C* K. Brill/NMC		02/92	Change to 4-digit representation	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		psign ( 9 )
	INCLUDE		'ERMISS.FNC'
	DATA		psign / 4 * 1., 0., 4 * -1. /
C*
C------------------------------------------------------------------------
	PR_P03C = RMISSD
C
C*	Get pressure tendency characteristic.
C
	IF  ( .not. ERMISS ( p03d ) ) THEN
	    itendc  = INT   ( p03d / 1000. )
	    ptend   = FLOAT ( MOD ( INT ( p03d ), 1000 ) ) / 10.
	    PR_P03C = psign ( itendc + 1 ) * ptend
	END IF
C*
	RETURN
	END
