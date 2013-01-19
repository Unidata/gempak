	FUNCTION PS_STAP  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_STAP								*
C*									*
C* This function computes the stability as the negative of the		*
C* potential temperature vertical gradient with respect to pressure.	*
C*									*
C* REAL PS_STAP  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_STAP		REAL		Stability ( - d THETA / dp)	*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 8/90						*
C* T. Lee/GSC		 8/97	Changed default depth to RMISSD		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		stndl (10), stndb (10), stndt (10)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
	PS_STAP = RMISSD
C
C*	Get the depth.
C
	dfdpth = RMISSD
	idfcrd = 1
	CALL PC_DPTH  ( datain, nparm, clev, ivcord, dfdpth, idfcrd, 1,
     +			depth, idcord, stndl, stndb, stndt, ier )
C
C*	Check for missing data.
C
	IF  ( ( ier .ne. 0 ) .or. ERMISS ( stndt (2) ) .or.
     +	      ERMISS ( stndb (2) ) )  RETURN
	deltap = stndt (1) - stndb (1)
	IF  ( deltap .eq. 0.0 )  RETURN
C*
	thtop = PR_THTA ( stndt (2), stndt (1) )
	thbot = PR_THTA ( stndb (2), stndb (1) )

	PS_STAP = - (( thtop - thbot ) / ( deltap * 100.0 ))
C*
	RETURN
	END
