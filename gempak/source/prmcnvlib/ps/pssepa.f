	FUNCTION PS_SEPA  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_SEPA								*
C*									*
C* This function computes the separation in mb between bounding		*
C* isentropic levels.  The default layer width is 5 K.			*
C*									*
C* REAL PS_SEPA  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_SEPA		REAL		Separation (mb)			*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 8/90						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		stndl (10), stndb (10), stndt (10)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
	PS_SEPA = RMISSD
C
C*	Get data over a 5 K layer, unless requested otherwise.
C
	dfdpth = 5.
	idfcrd = 2
	CALL PC_DPTH  ( datain, nparm, clev, ivcord, dfdpth, idfcrd, 1,
     +			depth, idcord, stndl, stndb, stndt, ier )
C
C*	Check for RETURN conditions.
C
	IF  ( ( ier .ne. 0 ) .or. ERMISS ( stndt (1) ) .or.
     +	      ERMISS ( stndb (1) ) )  RETURN
	deltap = stndt (1) - stndb (1)
	IF  ( deltap .eq. 0.0 )  RETURN
	PS_SEPA = deltap
C*
	RETURN
	END
