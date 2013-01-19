	SUBROUTINE SNDRHF ( prs, tmp, dwp, relh )
C************************************************************************
C* SNDRHF								*
C*									*
C* This routine will compute the relative humidity.			*
C*									*
C* SNDRHF ( PRS, TMP, DWP, RELH )					*
C*									*
C* Input parameters:							*
C*	PRS		REAL		Pressure			*
C*	TMP		REAL		Temperature			*
C*	DWP		REAL		Dewpoint			*
C*									*
C* Output parameters:							*
C*	RELH		REAL		Relative humidity		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		prs, tmp, dwp, relh
C*
	DATA		const3 / 0.003661 /
C------------------------------------------------------------------------
C*	Functions.
C
	XLEVAP(t) = 597.3 - .566 * (t - 273.15)
	SATVAP(t) = 6.11 * EXP( 9.045 * xlevap(t) * (const3 - 1. / t) )
	WFUN(e)   = .622 * e / ( p - e )
C
	p  = FLOAT ( prs )
	t  = FLOAT ( tmp )
	td = FLOAT ( dwp )
C
	esat = SATVAP ( t )
	e    = SATVAP ( td )
	wsat = WFUN ( esat )
	w    = WFUN ( e )
	relh = INT ( ( w / wsat ) * 100. )
C*
	RETURN
	END
