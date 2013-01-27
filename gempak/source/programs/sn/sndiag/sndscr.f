	SUBROUTINE SNDSCR ( nparms, nlevel, wavlen, wavspd, delz,
     +			    hdata, iret )
C************************************************************************
C* SNDSCR								*
C*									*
C* This routine computes the Scorer Parameter.				*
C*									*
C* SNDSCR ( NPARMS, NLEVEL, WAVLEN, WAVSPD, DELZ, HDATA, IRET )		*
C*									*
C* Input parameters:							*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEVEL		INTEGER		Number of levels		*
C*	WAVLEN		CHAR*		Length of wave			*
C*	WAVSPD		CHAR*		Speed of wave			*
C*	DELZ		CHAR*		Height interval			*
C*									*
C* Input/Output parameters:						*
C*	HDATA (LLMXLV)	REAL		Interpolated sounding data	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C* J. Whistler/SSAI	 6/93		Wavlen inputed in KM		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	CHARACTER*(*)	wavlen, wavspd, delz
	REAL		hdata(*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	CALL ST_CRNM ( wavlen, wvln, ier )
	IF  ( ( ier .ne. 0 ) .or. ( wvln .eq. 99999. ) ) THEN
	    wvln = 500.
	ELSE
	    wvln = wvln * 1000.
	END IF
C
	CALL ST_CRNM ( wavspd, wspd, ier )
	IF  ( ier .ne. 0 )  wspd = 0.
C
	CALL ST_CRNM ( delz, deltaz, ier )
	IF  ( ier .ne. 0 )  deltaz = 500.
C
C*	Loop over all levels filling the appropriate sections of the 
C*	data array.
C
	ivcord = 3
	DO  k = 2, nlevel-1
c	    clev = hdata((k-1)*nparms+IHGHT)
C
C*	    Compute the Brunt-Vaisala Freq squared.
C
c	    bvfqsq = SND_BVS ( hdata, nparms, clev, deltaz, ivcord )
	    bvfqsq = SND_BVS ( hdata, nparms, k, deltaz, ivcord )
	    richno = SND_RCH ( hdata, nparms, k, deltaz, ivcord )
	    rrchno = SND_RRC ( hdata, nparms, k, deltaz, ivcord )
	    shrdab = SND_SHD ( hdata, nparms, k, deltaz, ivcord )
	    shrmab = SND_SHM ( hdata, nparms, k, deltaz, ivcord )
C
C*	    Check for missing data.
C
	    IF  ( (deltaz .le. 0.0) .or.
     +	    	  ERMISS ( hdata((k-1-1)*nparms+IUWND) ) .or.
     +	    	  ERMISS ( hdata((k-1  )*nparms+IUWND) ) .or.
     +		  ERMISS ( hdata((k-1+1)*nparms+IUWND) ) .or.
     +	    	  ERMISS ( bvfqsq ) )  THEN
C
C*	    	Get next level.
C
		hdata((k-1)*nparms+ISCOR) = RMISSD
		hdata((k-1)*nparms+ISCR1) = RMISSD
		hdata((k-1)*nparms+ISCR2) = RMISSD
		hdata((k-1)*nparms+IUCVR) = RMISSD
		hdata((k-1)*nparms+IBVIN) = RMISSD
		hdata((k-1)*nparms+IBVFQ) = RMISSD
		hdata((k-1)*nparms+IRICH) = RMISSD
		hdata((k-1)*nparms+IRRCH) = RMISSD
		hdata((k-1)*nparms+ISHRD) = RMISSD
		hdata((k-1)*nparms+ISHRM) = RMISSD
	    ELSE
C
C*	        Compute the wind curvature of the U-component.
C
		ucvrot = ( hdata((k-1+1)*nparms+IUWND) -
     +		           2. * hdata((k-1)*nparms+IUWND) +
     +		           hdata((k-1-1)*nparms+IUWND))/(deltaz*deltaz)
C
C*	    	Check for critical levels.
C
		IF  ( ABS(hdata((k-1)*nparms+IUWND)-wspd) .le. 1e-3 ) 
     +			hdata((k-1)*nparms+IUWND) = 
     +			    hdata((k-1)*nparms+IUWND) + .01
C
		denom1 = hdata((k-1)*nparms+IUWND) - wspd
C
C*	    	Compute the first term in the calculation.
C
		term1  = bvfqsq / (denom1*denom1)
C
C*		Compute the second term of the calculation.
C
		term2 = ucvrot * ( -1. / denom1 )
C
C*	    	Compute the wave number from the wavelength.
C*		Compute the third term of the calculation.
C
		IF  ( wvln .eq. 99999. )  THEN
		    wavenum = 99999.
		    term3   = 0.
		ELSE
		    wavenum = ( 2. * PI ) / ( wvln * 1000. )
		    term3   = -1. * wavenum * wavenum
		END IF
C
C*	    	Compute the Scorer parameter and scale it.
C
		scorer = term1 + term2 + term3
C
C*		Compute the Brunt-Vaisala freq / Intrinsic freq.
C
		IF ( wavenum .ne. 99999. ) THEN
		    bvin = SQRT(term1) / wavenum
		ELSE
		    bvin = RMISSD
		END IF
C
C*		Set the data array values to the computed values.
C
		hdata((k-1)*nparms+ISCOR) = scorer * 1e6
		hdata((k-1)*nparms+ISCR1) = term1  * 1e6
		hdata((k-1)*nparms+ISCR2) = term2  * 1e6
		hdata((k-1)*nparms+IUCVR) = ucvrot * 1e6
		hdata((k-1)*nparms+IBVIN) = bvin
		hdata((k-1)*nparms+IBVFQ) = SQRT( bvfqsq ) * 1e2
		hdata((k-1)*nparms+IRICH) = richno
		hdata((k-1)*nparms+IRRCH) = rrchno
		hdata((k-1)*nparms+ISHRD) = shrdab
		hdata((k-1)*nparms+ISHRM) = shrmab
	    END IF
	END DO
C*
	RETURN
	END
