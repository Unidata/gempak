	FUNCTION PS_SHOW  ( t850, td850, t500 )
C************************************************************************
C* PS_SHOW 								*
C*									*
C* This function computes the Showalter index:				*
C*									*
C*      SHOW = T500 - Tparcel						*
C*									*
C*           Tparcel = Temperature in Celsius at 500 mb of a parcel	*
C*                     which lies on the moist adiabat determined	*
C*                     by the sounding at 850 mb			*
C*									*
C* REAL PS_SHOW  ( T850, TD850, T500 )					*
C*                							*
C* Input parameters:                                                    *
C*	T850    	REAL		850 mb temperature in Celsius	*
C*	TD850		REAL		850 mb dewpoint in Celsius	*
C*	T500		REAL		500 mb temperature in Celsius	*
C*									*
C* Output parameters:							*
C*	PS_SHOW		REAL		Showalter index			*
C**									*
C* Log:									*
C* P. Kocin/GSFC	 1980	Original source called PR_SHOW		*
C* M. Goodman/RDS	 8/84	Renamed and cleaned prologue		*
C* M. desJardins/GSFC	 3/88	Documentation				*
C************************************************************************
        INCLUDE      'GEMPRM.PRM'
        INCLUDE      'ERMISS.FNC'
C----------------------------------------------------------------------------
	IF  ( ERMISS ( t850 ) .or. ERMISS ( td850 ) .or. 
     +	      ERMISS ( t500 ) )  THEN
	    PS_SHOW = RMISSD
	  ELSE
	    p850 = 850.
C
C*	    Find equivalent potential temperture at the LCL using 850 mb
C*	    temperature and dewpoint.
C
	    thtlcl = PR_THTE ( p850, t850, td850 )
	    p500   = 500.
C
C*	    Find parcel temperature along pseudoadiabat at 500 mb.
C
	    guess = 0.
	    tp    = PR_TMST  ( thtlcl, p500, guess )
C
C*	    If parcel temp is no good, then set the index to missing.
C
	    IF  ( ERMISS ( tp ) )  THEN
		PS_SHOW = RMISSD
	      ELSE
C
C*	 	Else subtract the parcel temp from the temp at 500 mb.
C
		t500k   = pr_tmck ( t500 )
		PS_SHOW = t500k - tp
	    END IF
	END IF
C*
	RETURN
	END
