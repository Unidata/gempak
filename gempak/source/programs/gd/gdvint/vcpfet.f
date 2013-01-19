	SUBROUTINE VC_PFET  ( etav, ps, zs, ptop, nxy, p, iret )
C************************************************************************
C* VC_PFET								*
C*									*
C* This subroutine computes pressure values on an ETA surface		*
C*									*
C* VC_PFET ( ETAV, PS, ZS, PTOP, NXY, P, IRET )				*
C*									*
C* Input parameters:							*
C*	ETAV  		REAL		ETA value			*
C*	PS (NXY)	REAL		Surface pressure		*
C*	ZS (NXY)	REAL		Surface height			*
C*	PTOP		REAL		Top pressure			*
C*	NXY		INTEGER		Number of grid points		*
C*									*
C* Output parameters:							*
C*	P (NXY)		REAL		Output pressure values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/NMC		08/92	Added all missing check			*
C* T. Lee/GSC		12/99	Used TMCK				*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		p (*), ps (*), zs (*)
C*
	INCLUDE		'ERMISS.FNC'
	DATA		po, tlr / 1013.25, .0065 /
	DATA		expo / 5.25612 /
C*
C-----------------------------------------------------------------------
	iret = 0
	to = TMCK + 15.
C
C*	Compute the pressure values.
C
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( zs (i) ) .or. ERMISS ( ps (i) ) ) THEN
		p (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		aa    = 1. - tlr * zs (i) / to
		prefz = po * aa ** expo
		eee   = ( prefz - ptop ) / ( po - ptop )
		IF ( eee .eq. 0. ) THEN
		    iret = +1
		    RETURN
		END IF
		sgma  = etav / eee
		p (i) = sgma * ( ps (i) - ptop ) + ptop
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
