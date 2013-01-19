	FUNCTION PC_PSYM  ( tb, tt, delh, psb )
C************************************************************************
C* PC_PSYM								*
C*									*
C* This function computes the Montgomery stream function.		*
C*									*
C*  REAL PC_PSYM  ( TB, TT, DELH, PSB )					*
C*									*
C* Input parameters:							*
C*	TB		REAL		Bottom temperature		*
C*	TT		REAL		Top temperature			*
C*	DELH		REAL		Incremental dry hydrostatic z	*
C*	PSB		REAL		Bottom psi			*
C*									*
C* Output parameters:							*
C*	PC_PSYM		REAL		Montgomery stream function	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	 9/88						*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
C*	NOTE:  The values of CP and G are divided by 100. in order
C*	       to make the Montgomery Stream Function (PSYM) smaller
C*	       than 10**6.
C
	PARAMETER	( CP = RDGAS / RKAPPA / 100. )
	PARAMETER	( G = GRAVTY / 100. )
C*
        INCLUDE    'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ( .not. ERMISS (tb) ) .and. ( .not. ERMISS (tt) ) .and.
     +	      ( .not. ERMISS (delh) ) .and. ( .not. ERMISS (psb) ) ) 
     +								THEN
	    PC_PSYM = psb + ( CP * ( tt - tb ) + G * delh )
	  ELSE
	    PC_PSYM = RMISSD
	END IF
C*
	RETURN
	END
