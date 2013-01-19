	FUNCTION PC_DHGT  ( tkb, tkt, pb, pt, hb, delh )
C************************************************************************
C* PC_DHGT								*
C*									*
C* This function computes the dry hydrostatic height.			*
C*									*
C* REAL PC_DHGT  ( TKB, TKT, PB, PT, HB, DELH )				*
C*									*
C* Input parameters:							*
C*	TKB		REAL		Bottom temperature		*
C*	TKT		REAL		Top temperature			*
C*	PB		REAL		Bottom pressure			*
C*	PT		REAL		Top pressure			*
C*	HB		REAL		Bottom height			*
C*									*
C* Output parameters:							*
C*	DELH		REAL		Height increment		*
C*	PC_DHGT		REAL		Dry hydrostatic height		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/84						*
C* T. Piper/GSC		11/98		Updated prolog			*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ( .not. ERMISS (tkb) ) .and. ( .not. ERMISS (tkt) ) .and.
     +	      ( .not. ERMISS (pb)  ) .and. ( .not. ERMISS (pt) ) .and.
     +	      ( .not. ERMISS (hb)  ) .and. ( pt .ne. 0. ) )  THEN
	    delh = RKAP * 0.5 * (tkb + tkt) * ALOG ( pb / pt )
	    PC_DHGT = hb + delh
	  ELSE
	    delh = RMISSD
	    PC_DHGT = RMISSD
	END IF
C*
	RETURN
	END
