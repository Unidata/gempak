	FUNCTION PC_MHGT  ( tb, tt, tdb, tdt, pb, pt, hb )
C************************************************************************
C* PC_MHGT								*
C*									*
C* This function computes the moist hydrostatic height.			*
C*									*
C* REAL PC_MHGT  ( TB, TT, TDB, TDT, PB, PT, HB )			*
C*									*
C* Input parameters:							*
C*	TB		REAL		Bottom temperature		*
C*	TT		REAL		Top temperature			*
C*	TDB		REAL		Bottom dewpoint 		*
C*	TDT		REAL		Top dewpoint 			*
C*	PB		REAL		Bottom pressure			*
C*	PT		REAL		Top pressure			*
C*	HB		REAL		Bottom height			*
C*									*
C* Output parameters:							*
C*	PC_MHGT		REAL		Moist hydrostatic height	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	IF  ( ( .not. ERMISS (tb) ) .and. ( .not. ERMISS (tt) ) .and. 
     +	      ( .not. ERMISS (pb) ) .and. ( .not. ERMISS (pt) ) .and. 
     +	      ( .not. ERMISS (hb) ) .and. ( pt .ne. 0.) )  THEN
	    tvb = PR_TVRK ( tb, tdb, pb )
	    tvt = PR_TVRK ( tt, tdt, pt )
	    tav = ( tvb + tvt ) / 2.0
	    PC_MHGT = hb + RKAP * tav * ALOG ( pb / pt )
	  ELSE
	    PC_MHGT = RMISSD
	END IF
C*
	RETURN
	END
