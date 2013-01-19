	SUBROUTINE VC_BLDZ  ( t, p, q, nxy, update, tl, pl, zl, z,
     +			      iret )
C************************************************************************
C* VC_BLDZ								*
C*									*
C* This subroutine builds height hydrostatically.			*
C*									*
C* VC_BLDZ ( T, P, Q, NXY, UPDATE, TL, PL, ZL, Z, IRET )		*
C*									*
C* Input parameters:							*
C*	T   (NXY)	REAL		Temperature			*
C*	P   (NXY)	REAL		LN (pressure)			*
C*	Q   (NXY)	REAL		Specific humidity		*
C*	NXY		INTEGER		Number of points		*
C*	UPDATE		LOGICAL		Flag to update lower level	*
C*									*
C* Input and output parameters:						*
C*	TL  (NXY)	REAL		Lower level virtual temperature *
C*	PL  (NXY)	REAL		Lower level LN (pressure)	*
C*	ZL  (NXY)	REAL		Lower level z			*
C*									*
C* Output parameters:							*
C*	Z   (NXY)	REAL		Height				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC      05/92						*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	REAL		tl (*), pl (*), zl (*), t (*), p (*), z (*), q(*)
	LOGICAL		update
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C*
	icnt = 0
	DO i = 1, nxy
	    IF ( ERMISS ( tl (i) ) .or. ERMISS ( t (i) ) .or.
     +		 ERMISS ( pl (i) ) .or. ERMISS ( p (i) ) .or.
     +		 ERMISS ( zl (i) ) ) THEN
		z (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		IF ( .not. ERMISS ( q (i) ) ) THEN
		    tv = t (i) * ( 1 + .608 * q (i) )
		ELSE
		    tv = t (i)
		END IF
		tbar = .5 * ( tl (i) + tv )
		dlnp = p (i) - pl (i) 
		dz   = RKAP * tbar * dlnp
	   	z (i) = zl (i) - dz
		IF ( update ) THEN
		    tl (i) = tv
		    pl (i) = p (i)
		    zl (i) = z (i)
		END IF
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
