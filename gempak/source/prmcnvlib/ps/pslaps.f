	FUNCTION PS_LAPS  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_LAPS								*
C*									*
C* This function computes the temperature lapes rate.			*
C*									*
C* REAL PS_LAPS  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_LAPS		REAL		Lapse rate (dT/dz)		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C* J. Nielsen/SUNYA	 8/90	Allow user requested layer depth.	*
C* T. Lee/GSC		 8/97	Changed default depth to RMISSD		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		stndl (10), stndb (10), stndt (10)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
	PS_LAPS = RMISSD
C
C*	Get data at this layer, unless requested otherwise.
C
	dfdpth = RMISSD
	idfcrd = 3
	CALL PC_DPTH  ( datain, nparm, clev, ivcord, dfdpth, idfcrd, 1,
     +			depth, idcord, stndl, stndb, stndt, ier )
C
C*	Check for RETURN conditions.
C
	IF ( ier .ne. 0 ) RETURN
	IF  ( ERMISS ( stndt (2) ) .or. ERMISS ( stndb (2) ) .or.
     +	      ERMISS ( stndt (6) ) .or. ERMISS ( stndb (6) ) )  RETURN
	depth = stndt (6) - stndb (6)
	IF  ( depth .eq. 0.0 )  RETURN
C*
	PS_LAPS = ( stndt (2) - stndb (2) ) / depth
C*
	RETURN
	END
