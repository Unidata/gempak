	FUNCTION PS_SHRD  ( datain, nparm, clev, ivcord )
C************************************************************************
C* PS_SHRD								*
C*									*
C* This function computes the wind shear direction for a layer.		*
C*									*
C* REAL PS_SHRD  ( DATAIN, NPARM, CLEV, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of data set parameters	*
C*	CLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER 	Vertical coordinate number	*
C*									*
C* Output parameters:							*
C*	PS_SHRD		REAL		Wind shear direction		*
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
	PS_SHRD = RMISSD
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
	DO  i = 4, 6
	    IF ( ERMISS ( stndb (i) ) ) RETURN
	    IF ( ERMISS ( stndt (i) ) ) RETURN
	END DO
	depth = stndt (6) - stndb (6)
	IF  ( depth .eq. 0.0 )  RETURN
C
C*	Do shear calculation.
C
	CALL PC_GCND ( '$', 1, dir, cvalue, ier )
	utop = stndt (4)
	vtop = stndt (5)
	ubot = stndb (4)
	vbot = stndb (5)
	IF ( ier .ne. 0 ) THEN
	    dudz = ( utop - ubot ) / depth
	    dvdz = ( vtop - vbot ) / depth
	    PS_SHRD = PR_DRCT ( dudz, dvdz )
	  ELSE
	    PS_SHRD = dir
	END IF
C*
	RETURN
	END
