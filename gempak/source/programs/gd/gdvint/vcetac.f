	SUBROUTINE VC_ETAC  ( p, ps, zs, ptop, nxy, eta, iret )
C************************************************************************
C* VC_ETAC								*
C*									*
C* This subroutine computes eta coordinate values.			*
C*									*
C* VC_ETAC ( P, PS, ZS, PTOP, NXY, ETA,  IRET )				*
C*									*
C* Input parameters:							*
C*	P  (NXY)	REAL		Pressure			*
C*	PS (NXY)	REAL		Surface pressure		*
C*	ZS (NXY)	REAL		Surface height			*
C*	PTOP		REAL		Top pressure			*
C*	NXY		INTEGER		Number of grid points		*
C*									*
C* Output parameters:							*
C*	ETA (NXY)	REAL		Output eta values * 10000	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* R. Miller/COMET	06/94	Changed 100000 to 10000 for eta and sgma*
C* T. Lee/GSC		12/99	Used TMCK				*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		p (*), ps (*), zs (*), eta (*)
C*
	INCLUDE		'ERMISS.FNC'
	DATA		po, tlr / 1013.25, .0065 /
	DATA		expo / 5.25612 /
C*
C-----------------------------------------------------------------------
	iret = 0
C
C*	Compute the eta coordinate values.
C
	to = TMCK + 15.
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( p (i) ) .or. ERMISS ( ps (i) ) .or.
     +		 ERMISS ( zs (i) ) ) THEN
		eta (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		sgma  = ( p (i) - ptop ) / ( ps (i) - ptop )
		aa    = 1. - tlr * zs (i) / to
		prefz = po * aa ** expo
		eee   = ( prefz - ptop ) / ( po - ptop )
		eta (i) = eee * sgma * 10000.
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
