	FUNCTION SND_RCH  ( datain, nparm, k, deltaz, ivcord )
C************************************************************************
C* SND_RCH								*
C*									*
C* This function computes the Richardson number for a layer.		*
C*									*
C*		RICH = BFVQ ** 2 / SHRM ** 2				*
C*									*
C* SND_RCH  ( DATAIN, NPARM, CLEV, IVCORD )				*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	SND_RCH		REAL		Richardson number		*
C**									*
C* Log:									*
C* K. Brill/NMC		 7/90						*
C* J. Whistler/SSAI	 4/93		Modified for SNDIAG		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	REAL            datain (*)
C*
	REAL    	stndb (10), stndt (10), stndl (10)
	CHARACTER       cvalue*20
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	SND_RCH = RMISSD
C
C*	Get information over a depth.
C
	depth = deltaz * 2.
	thtop = datain((k-1+1)*nparm+ITHTV)
	thbot = datain((k-1-1)*nparm+ITHTV)
	theta = .5 * ( thtop + thbot )
	bvfsqd = ( GRAVTY / theta ) * ( thtop - thbot ) / depth
C*
	utop = datain((k-1+1)*nparm+IUWND)
	vtop = datain((k-1+1)*nparm+IVWND)
	ubot = datain((k-1-1)*nparm+IUWND)
	vbot = datain((k-1-1)*nparm+IVWND)
	dudz = ( utop - ubot ) / depth
	dvdz = ( vtop - vbot ) / depth
	shrsqd = ( dudz * dudz + dvdz * dvdz )
	IF  ( shrsqd .eq. 0.0 )  RETURN
C*
	SND_RCH = bvfsqd / shrsqd 
C*
	RETURN
	END
