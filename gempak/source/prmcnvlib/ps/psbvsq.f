	FUNCTION PS_BVSQ  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_BVSQ								*
C*									*
C* This function computes the square of the Brunt-Vaisala Frequency.	*
C*									*
C* REAL PS_BVSQ  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_BVSQ		REAL		Brunt-Vaisala Frequency **2	*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 8/90						*
C* T. Lee/GSC		 8/97	Changed default depth to RMISSD		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datain (*)
C*
	REAL		stndl (10), stndb (10), stndt (10)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
	PS_BVSQ = RMISSD
C
C*	Get data at this layer, unless requested otherwise.
C
	dfdpth = RMISSD
	idfcrd = 3
	CALL PC_DPTH  ( datain, nparm, clev, ivcord, dfdpth, idfcrd, 1,
     +		 	depth, idcord, stndl, stndb, stndt, ier )
C
C*	Check for missing data.
C
	IF  ( ier .ne. 0 )  RETURN
	IF  ( ERMISS ( stndt (1) ) .or. ERMISS ( stndb (1) ) .or.
     +	      ERMISS ( stndt (2) ) .or. ERMISS ( stndb (2) ) .or.
     +	      ERMISS ( stndt (6) ) .or. ERMISS ( stndb (6) ) )  RETURN
	depth = stndt (6) - stndb (6)
	IF  ( depth .eq. 0.0 )  RETURN
C*
	thtop = PR_THTA ( stndt (2), stndt (1) )
	thbot = PR_THTA ( stndb (2), stndb (1) )
        theav = ( thtop + thbot ) / 2.
C
C*	Compute N**2.
C
        bvfsqd = ( GRAVTY / theav ) * ( thtop - thbot ) / depth
	IF  ( bvfsqd .lt. 0.0 )  RETURN
	PS_BVSQ = bvfsqd
C*
	RETURN
	END
