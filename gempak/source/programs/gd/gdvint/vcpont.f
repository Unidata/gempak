	SUBROUTINE VC_PONT  ( theta, tht, thb, tt, tb, rlnpt, rlnpb,
     +			      nxy, p, iret ) 
C************************************************************************
C* VC_PONT								*
C*									*
C* This subroutine computes pressure on an isentropic surface given	*
C* pressure, temperature and potential temperature on bounding surfaces.*
C*									*
C* The method solves an implicit equation derived by combining the	*
C* definition of potential temperature and the assumption that tem-	*
C* perature varies linearly with ln (p).  Newton iteration is used to	*
C* solve for ln (p).							*
C*									*
C* VC_PONT ( THETA, THT, THB, TT, TB, RLNPT, RLNPB, NXY, P, IRET )	*
C*									*
C* Input parameters:							*
C*	THETA		REAL		Isentropic coordinate value	*
C*	THT(*)		REAL		Top potential temperature	*
C*	THB(*)		REAL		Bottom potential temperature	*
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
C* J. Wu/GSC         07/00      Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		tht (*), thb (*), tt (*), tb (*), rlnpt (*),
     +			rlnpb (*), p (*)
C*
	LOGICAL		betwen
	INCLUDE		'ERMISS.FNC'
	DATA		eps /.001/, pok /7.196857/, kmax /10/
C*
	BETWEN ( rlev, rlo, rhi ) = 
     +     ( ( rlev .ge. rlo .and. rlev .le. rhi )   .or.
     +       ( rlev .le. rlo .and. rlev .ge. rhi ) )
C-----------------------------------------------------------------------
	iret = 0
C*
	icnt = 0
	DO i = 1, nxy
	    IF ( .not. BETWEN ( theta, thb (i), tht (i) ) .or.
     +		 ERMISS ( tht   (i) ) .or. ERMISS ( thb   (i) ) .or.
     +		 ERMISS ( tt    (i) ) .or. ERMISS ( tb    (i) ) .or.
     +		 ERMISS ( rlnpt (i) ) .or. ERMISS ( rlnpb (i) ) .or.
     +		 ( thb (i) .gt. tht (i) ) .or.
     +		 ( rlnpt (i) .eq. rlnpb (i) ) ) THEN
		p (i) = RMISSD
		icnt = icnt + 1
	    ELSE
		a = ( tt (i) - tb (i) ) / ( rlnpt (i) - rlnpb (i) )
		b = tt (i) - a * rlnpt (i)
		IF ( tht (i) .ne. thb (i) ) THEN
		    rm = ( theta - thb (i) ) / ( tht (i) - thb (i) )
		ELSE
		    rm = .5
		END IF
		pln = rlnpb (i) + rm * ( rlnpt (i) - rlnpb (i) )
		res = 1.
		k = 1
		DO WHILE ( res .gt. eps .and. k .lt. kmax )
		    ekp = EXP ( (-RKAPPA) * pln )
		    t = a * pln + b
		    f = theta - pok * t * ekp
		    fp = pok * ekp * ( RKAPPA * t - a )
		    pin = pln - f/fp
		    res = ABS ( pln - pin )	
		    pln = pin
		    k = k + 1
		END DO
		p (i) = EXP ( pln )
	    END IF
	END DO
C*
	IF ( icnt .eq. nxy ) iret = +1
C*
	RETURN
	END
