	FUNCTION PR_PTSY ( p03d )
C************************************************************************
C* PR_PTSY								*
C*									*
C* This subroutine extracts the symbol code from the pressure tendency	*
C* information.	 The code number is returned follow by 999 so that the	*
C* output is a 4-digit number.						*
C*									*
C* REAL PR_PTSY ( P03D )						*
C*									*
C* Input parameters:							*
C*	P03D		REAL		Pressure tendency information	*
C*									*
C* Output parameters:							*
C*	PR_PTSY		REAL		Pressure tendency symbol code	*
C**									*
C* Log:									*
C* K. Brill/NMC		11/91						*
C* K. Brill/NMC		02/92	Use 4-digit output			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_PTSY = RMISSD
C
C*	Get pressure tendency characteristic symbol number.
C
	IF  ( .not. ERMISS ( p03d ) ) THEN
	    PR_PTSY = FLOAT ( INT ( p03d / 1000. ) ) * 1000. + 999.
	END IF
C*
	RETURN
	END
