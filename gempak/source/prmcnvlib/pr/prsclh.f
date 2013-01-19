	FUNCTION  PR_SCLH  ( tb, tt, tdb, tdt, pb, pt )
C************************************************************************
C* PR_SCLH								*
C*									*
C* This function computes SCLH from TB, TT, TDB, TDT, PB, and PT.	*
C* SCLH, the scale height in a layer, can then be used to compute 	*
C* the moist hydrostatic height.  The following equation is used:	*
C*									*
C*              SCLH  =  ( RDGAS / GRAVTY ) * TAV			*
C*									*
C*                    TAV    = average virtual temperature in layer	*
C*                           = ( TVIRTB + TVIRTT ) / 2			*
C*                    TVIRTB = virtual temperature at bottom		*
C*                    TVIRTT = virtual temperature at top		*
C*									*
C* REAL PR_SCLH  ( TB, TT, TDB, TDT, PB, PT )				*
C*									*
C* Input parameters:							*
C*	TB		REAL		Bottom temperature in Celsius	*
C*	TT		REAL		Top temperature in Celsius	*
C*	TDB		REAL		Bottom dewpoint in Celsius	*
C*	TDT		REAL		Top dewpoint in Celsius		*
C*	PB		REAL		Bottom pressure in millibars	*
C*	PT		REAL		Top pressure in millibars	*
C*									*
C* Output parameters:							*
C*	PR_SCLH		REAL		Scale height in meters		*
C**									*
C* Log:									*
C* M. desJardins/GSFC							*
C* G. Huffman/GSC	 7/88	Documentation; test td's, tv's		*
C* M. desJardins/GSFC	12/88	Eliminated check for missing td for	*
C*				the SECOND time.  DO NOT PUT IT BACK.	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing data.
C
C*	DO NOT PUT CHECK FOR MISSING TD HERE.  IT WILL SCREW ALL THE
C*	UPPER AIR MERGING SOFTWARE UP!!!!!!!!!!!!!
C
	IF  ( ( ERMISS (tb)  ) .or. ( ERMISS (tt)  ) .or.
     +        ( ERMISS (pb)  ) .or. ( ERMISS (pt)  ) )  THEN
	    PR_SCLH = RMISSD
	  ELSE
	    tvb = PR_TVRK ( tb, tdb, pb )
	    tvt = PR_TVRK ( tt, tdt, pt )
	    IF  ( ( ERMISS (tvb)) .or. ( ERMISS (tvt) ) )  THEN
	        PR_SCLH = RMISSD
              ELSE
	        tav = ( tvb + tvt ) / 2.0
	        PR_SCLH = RKAP * tav 
	    END IF
	END IF
C*
	RETURN
	END
