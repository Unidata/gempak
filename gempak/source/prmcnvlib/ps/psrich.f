	FUNCTION PS_RICH  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_RICH								*
C*									*
C* This function computes the Richardson number for a layer.		*
C*									*
C*		RICH = BFVQ ** 2 / SHRM ** 2				*
C*									*
C* REAL PS_RICH  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_RICH		REAL		Richardson number		*
C**									*
C* Log:									*
C* K. Brill/NMC		 7/90						*
C* T. Lee/GSC		 8/97	Changed default depth to RMISSD		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL            datain (*)
C*
	REAL    	stndb (10), stndt (10), stndl (10)
	CHARACTER       cvalue*20
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
	PS_RICH = RMISSD
C
C*	Get information over a depth.
C
	dfdpth = RMISSD
	idfcrd = 3
	CALL PC_DPTH  ( datain, nparm, clev, ivcord, dfdpth, idfcrd, 1,
     +			depth, idcord, stndl, stndb, stndt, ier )
C
C*	Check for missing data.
C
	IF  ( ier .ne. 0 )  RETURN
	DO  i = 1, 6
	    IF ( i .ne. 3 ) THEN
		IF  ( ERMISS ( stndt (i) ) .or. ERMISS ( stndb (i) ) )
     +					RETURN
	    END IF
	END DO
	depth = stndt (6) - stndb (6)
	IF  ( depth .eq. 0.0 )  RETURN
C*
	thtop = PR_THTA ( stndt (2), stndt (1) )
	thbot = PR_THTA ( stndb (2), stndb (1) )
        theta = .5 * ( thtop + thbot )
        bvfsqd = ( GRAVTY / theta ) * ( thtop - thbot ) / depth
C*
	CALL PC_GCND ( '$', 1, dir, cvalue, ier )
	utop = stndt (4)
	vtop = stndt (5)
	ubot = stndb (4)
	vbot = stndb (5)
	IF ( ier .ne. 0 ) THEN
	    dudz = ( utop - ubot ) / depth
	    dvdz = ( vtop - vbot ) / depth
	    shrsqd = ( dudz * dudz + dvdz * dvdz )
	  ELSE
	    alpha = ( 270. - dir ) * DTR
	    uutop = utop * COS ( alpha ) + vtop * SIN ( alpha )
	    uubot = ubot * COS ( alpha ) + vbot * SIN ( alpha )
	    dudz = ( uutop - uubot ) / depth
	    shrsqd = dudz * dudz
	END IF
	IF  ( shrsqd .eq. 0.0 )  RETURN
C*
	PS_RICH = bvfsqd / shrsqd 
C*
	RETURN
	END
