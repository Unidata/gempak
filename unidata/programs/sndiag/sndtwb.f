	SUBROUTINE SNDTWB ( ip, it, itd, itflg, thw, iplcl )
C************************************************************************
C* SNDTWB								*
C*									*
C* This routine will calculate:						*
C*	1) Wet-bulb potential temperature				*
C*	2) Saturation wet-bulb potential temperature			*
C*	3) Wet-bulb temperature						*
C*	4) Lifting condensation level					*
C*									*
C* SNDTWB ( IP, IT, ITD, ITFLG, THW, IPLCL )				*
C*									*
C* Input parameters:							*
C*	IP		INTEGER		Pressure			*
C*	IT		INTEGER		Temperature			*
C*	ITD		INTEGER		Dewpoint			*
C*	ITFLG		INTEGER		Saturation flag			*
C*									*
C* Output parameters:							*
C*	THW		REAL		Wet-bulb potential temperature	*
C*	IPLCL		INTEGER		Pressure of the LCL		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( RD = .28704, CP = 1.0057,
     +			  RV = .46213, RKP = .28541 )
C------------------------------------------------------------------------
C*      Functions.
C
	XLEVAP(t) = ( 597.3 - .57 * (t - 273.17) ) * 4.186
	SATVAP(td) = 6.11 * EXP(XLEVAP(t) * (1./273. - 1./td) / RV )
	WFUN(e)   = .622 * e / ( p - e )
C
	dp = 160.
	p0 = 1000.
	p  = FLOAT ( ip  )
	t  = FLOAT ( it  )
	td = FLOAT ( itd )
C
C*	Calculate mixing ratios and potential temperature.
C
	esat  = SATVAP ( t )
	e     = SATVAP ( td )
	xmixs = WFUN ( esat )
	xmixr = WFUN ( e )
	thta  = t * ( p0/p ) ** RKP
C
C*	Decrement P by 80 mb. Calculate new TMP using constant THTA.
C*	Loop until XMIXS < XMIXR. This occurs only after P < PLCL.
C
	IF  ( itflg  .eq. 3     )  GOTO 260
	IF  ( it     .eq. itd   )  GOTO 260
230	IF  ( xmixs  .le. xmixr )  GOTO 250
	p = p - 80.
	t = thta / ( p0 / p ) ** RKP
	xmixs = WFUN ( SATVAP ( t ) )
	GOTO 230
C
C*	Now zero in on PLCL until criterion in first "IF" is met.
C
250	dp = dp / 2.
	IF  ( (ABS(xmixs-xmixr) .le. 5e-5) .or. (ABS(dp) .lt. 1.) )
     +		GOTO 260
	IF  ( (xmixs-xmixr) .lt. 0. ) p = p + dp
	IF  ( (xmixs-xmixr) .ge. 0. ) p = p - dp
	t = thta / ( p0 / p ) ** RKP
	xmixs = WFUN ( SATVAP ( t ) )
	GOTO 250
260	iplcl = INT ( p )
	IF  ( itflg .eq. 2 ) GOTO 280
C
C*	Go down moist adiabat to 1000 mb or surface pressure.
C*	Loop until criterion is met.
C*	See the Ceaseless Wind for TDP calculation (278,16)
C
	dp = 20.
	IF  ( itflg .eq. 0 ) p0 = ip
270	IF  ( p .gt. (p0-dp) )  dp = p0 - p
	p = p + 0.5 * dp
	xmixr = WFUN ( SATVAP ( t ) )
	pllv = XLEVAP ( t )
	pllvrs = pllv * xmixr
	dt = ( ( ( RD + pllvrs / t ) * ( dp / p ) ) /
     +		( CP + ( pllv * pllvrs ) / ( RV * t * t ) ) ) * t
	p = p + 0.5 * dp
	t = t + dt
	IF  ( ABS(p-p0) .gt. 0.5)  GOTO 270
	thw = t
C*
280	RETURN
	END
