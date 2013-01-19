	FUNCTION PS_BVPD  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_BVPD								*
C*									*
C* This function computes the Brunt-Vaisala Period.			*
C*									*
C* REAL PS_BVPD  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_BVPD		REAL		Brunt-Vaisala Period		*
C**									*
C* Log:									*
C* K. Brill /NMC	 7/90						*
C* T. Lee/GSC		 8/97	Changed default depth to RMISSD 	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		stndb (10), stndt (10), stndl (10)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
	PS_BVPD = RMISSD
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
	IF  ( ( ier .ne. 0 ) .or. ERMISS ( stndt (1) ) .or. 
     +		ERMISS ( stndb (1) ) .or. ERMISS ( stndt (2) ) .or.
     +		ERMISS ( stndb (2) ) .or. ERMISS ( stndt (6) ) .or.
     +		ERMISS ( stndb (6) ) )  RETURN
	depth = stndt (6) - stndb (6)
	IF  ( depth .eq. 0.0 )  RETURN
C*
	thtop = PR_THTA ( stndt (2), stndt (1) )
	thbot = PR_THTA ( stndb (2), stndb (1) )
        theta = .5 * ( thtop + thbot )
        bvfsqd = ( GRAVTY / theta ) * ( thtop - thbot ) / depth
	IF ( bvfsqd .le. 0.0 ) RETURN
C*
	PS_BVPD = 2. * PI / SQRT ( bvfsqd )
C*
	RETURN
	END
