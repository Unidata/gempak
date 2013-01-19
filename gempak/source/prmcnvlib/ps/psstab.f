	FUNCTION PS_STAB  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_STAB								*
C*									*
C* This function computes the stability which is the potential		*
C* temperature lapse rate.						*
C*									*
C* REAL PS_STAB  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_STAB		REAL		Stability (d THETA / dz)	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C* T. Lee/GSC		 8/97	Changed default depth to RMISSD		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		stndb (10), stndt (10), stndl (10)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
	PS_STAB = RMISSD
C
C*	Get depth information.
C
	dfdpth = RMISSD
	idfcrd = 3
	CALL PC_DPTH  ( datain, nparm, clev, ivcord, dfdpth, idfcrd, 1,
     +			depth, idcord, stndl, stndb, stndt, ier )
C
C*	Check for missing data.
C
	IF  ( ( ier .ne. 0 ) .or. ERMISS ( stndt (2) ) .or. 
     +	      ERMISS ( stndb (2) ) .or. ERMISS ( stndt (6) ) .or.
     +	      ERMISS ( stndb (6) ) )  RETURN
	depth = stndt (6) - stndb (6)
	IF  ( depth .eq. 0.0 )  RETURN
C*
	thtop = PR_THTA ( stndt (2), stndt (1) )
	thbot = PR_THTA ( stndb (2), stndb (1) )

	PS_STAB = ( thtop - thbot ) / depth
C*
	RETURN
	END
