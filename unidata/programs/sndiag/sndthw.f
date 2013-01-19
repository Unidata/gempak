	FUNCTION SND_THW ( temp, pres, dwpt )
C************************************************************************
C* SND_THW								*
C*									*
C* This function will compute the Wet-bulb potential temperature.	*
C*									*
C* SND_THW ( TEMP, PRES, DWPT )						*
C*									*
C* Input parameters:							*
C*	TEMP		REAL		Temperature			*
C*	PRES		REAL		Pressure			*
C*	DWPT		REAL		Dew point			*
C*									*
C* Output parameters:							*
C*	SND_THW		REAL		Wet-bulb potential temperature	*
C**									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C------------------------------------------------------------------------
	thtak = PR_THTA ( temp, pres )
	thtac = PR_TMKC ( thtak )
C
	dwpd = temp - dwpt
	delt =  dwpd * ( 1.2185 + 1.278e-3 * temp +
     +		dwpd * ( -2.19e-3 + 1.173e-5 * dwpd - 5.2e-6 * temp ) )
	tlfc = temp - delt
C
	SND_THW = thtac - SND_WOB ( thtac ) + SND_WOB ( tlfc )
C*
	RETURN
	END
