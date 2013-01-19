	SUBROUTINE SNDFLX ( squall, nparms, nlevel, hdata, iret )
C************************************************************************
C* SNDFLX								*
C*									*
C* This routine will compute the air and moisture fluxes.		*
C*									*
C* SNDFLX ( SQUALL, NPARMS, NLEVEL, HDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	SQUALL		CHAR*		Length of squall line		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEVEL		INTEGER		Number of levels		*
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
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	CHARACTER*(*)	squall
	REAL		hdata(*)
C------------------------------------------------------------------------
	iret   = 0
	sfluxa = 0.
	sfluxm = 0.
C
	CALL ST_CRNM ( squall, xlen, ier )
	IF  ( ier .ne. 0 )  xlen = 20000.
C
	DO  i = 2, nlevel-1
	    delp  = ( hdata((i-1+1)*nparms+IPRES) -
     +		      hdata((i-1-1)*nparms+IPRES) ) / 2.
	    xmixr = PR_MIXR ( hdata((i-1)*nparms+IDWPT), 
     +			      hdata((i-1)*nparms+IPRES) )
	    sfluxa = sfluxa -
     +		(1.e5*hdata((i-1)*nparms+IVWND)*xlen*delp) / GRAVTY
	    sfluxm = sfluxm -
     +		(1.e5*hdata((i-1)*nparms+IVWND)*xmixr*1.e-3*xlen*delp)/
     +		    GRAVTY
	    hdata((i-1)*nparms+IFLXA) = sfluxa / 1.e11
	    hdata((i-1)*nparms+IFLXM) = sfluxm / 1.e9
	END DO
C*
	RETURN
	END
