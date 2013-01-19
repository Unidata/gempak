	SUBROUTINE VC_PONZ  ( z, zt, zb, tt, tb, rlnpt, rlnpb,
     +			      nxy, p, iret ) 
C************************************************************************
C* VC_PONZ								*
C*									*
C* This subroutine computes pressure on a height surface given		*
C* pressure, temperature and height on bounding surfaces.		*
C*									*
C* VC_PONZ ( Z, ZT, ZB, TT, TB, RLNPT, RLNPB, NXY, P, IRET )		*
C*									*
C* Input parameters:							*
C*	Z		REAL		Height coordinate value		*
C*	ZT(*)		REAL		Top height			*
C*	ZB(*)		REAL		Bottom height			*
C*	TT(*)		REAL		Top temperature			*
C*	TB(*)		REAL		Bottom temperature		*
C*	RLNPT(*)	REAL		Top ln (p)			*
C*	RLNPB(*)	REAL		Bottom ln (p)			*
C*	NXY		INTEGER		Number of points		*
C*									*
C* Output parameters:							*
C*	P    (NXY)	REAL		Output pressure values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = all missing		*
C**									*
C* Log:									*
C* K. Brill/NMC      07/92						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		zt (*), zb (*), tt (*), tb (*), rlnpt (*),
     +			rlnpb (*), p (*)
C*
	PARAMETER	( govr = 2. * GRAVTY / RDGAS )
	LOGICAL		betwen
C*
	INCLUDE		'ERMISS.FNC'
C*
	BETWEN ( rlev, rlo, rhi ) = 
     +     ( ( rlev .ge. rlo .and. rlev .le. rhi )   .or.
     +       ( rlev .le. rlo .and. rlev .ge. rhi ) )
C-----------------------------------------------------------------------
	iret = 0
C*
	icnt = 0
	DO i = 1, nxy
	    IF ( .not. BETWEN ( z, zb (i), zt (i) ) .or.
     +		 ERMISS ( zt   (i) ) .or. ERMISS ( zb   (i) ) .or.
     +		 ERMISS ( tt    (i) ) .or. ERMISS ( tb    (i) ) .or.
     +		 ERMISS ( rlnpt (i) ) .or. ERMISS ( rlnpb (i) ) .or.
     +		 ( zb (i) .gt. zt (i) ) .or.
     +		 ( rlnpt (i) .eq. rlnpb (i) ) ) THEN
		p (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		gama = ( tt (i) - tb (i) ) / ( rlnpt (i) - rlnpb (i) )
		dzt = zt (i) - z
		IF ( gama .eq. 0.0 ) THEN
		    pln = rlnpt (i) + .5 * govr * dzt / tt (i)
		ELSE
		    sq = tt (i) * tt (i) + govr * gama * dzt
		    sq = ( SQRT ( sq ) - tt (i) ) / gama
		    pln = rlnpt (i) + sq
		END IF
		p (i) = EXP ( pln )
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
