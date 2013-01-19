	SUBROUTINE PD_CAPE ( zbot, ztop, thbot, thtop, thpbot, thptop,
     +                          cape, cins )
C************************************************************************
C* PD_CAPE  ( ZBOT, ZTOP, THBOT, THTOP, THPBOT, THPTOP, CAPE, CINS )	*
C*									*
C* This routine calculates CAPE & CINS for a specified layer. The input	*
C* CAPE and CINS values are updated with the sum of computed layer.	*
C*									*
C* A trapezoidal integration will be performed. The layer will be	*
C* divided into depths with a maximum of 100m. Only positive area will	*
C* be accumulated for CAPE, and negative area for CINS.			*
C*									*
C* Input parameters:							*
C*	ZBOT		REAL		Z bottom			* 
C*	ZTOP		REAL		Z top				* 
C*	THBOT		REAL		THTA (environment) bottom	* 
C*	THTOP		REAL		THTA (environment) top		* 
C*	THPBOT		REAL		THTA (parcel) bottom		* 
C*	THPTOP		REAL		THTA (parcel) top		* 
C*									*
C* Input and Output Parameters:						*
C*	CAPE		REAL		Updated CAPE sum		*
C*	CINS		REAL		Updated CINS sum		*
C************************************************************************
	REAL		adata(3), bdata(3), outdat(3)
	LOGICAL		intflg(3), angflg(3)
C
	adata (1) = zbot
	bdata (1) = ztop
	adata (2) = thbot
	bdata (2) = thtop
	adata (3) = thpbot
	bdata (3) = thptop
	intflg(1) = .true.
	intflg(2) = .true.
	intflg(3) = .true.
	angflg(1) = .false.
	angflg(2) = .false.
	angflg(3) = .false.
C
C*	Integrate using 100m maximum depth
C
	DO WHILE ( zbot .lt. ztop )
	    IF ( ( zbot + 100. ) .lt. ztop ) THEN
		zup = zbot + 100.
	        CALL PC_INTH ( zup, adata, bdata,
     +		      3, intflg, angflg, 1, outdat, ier )
		IF ( ier .ne. 0 ) THEN
		   CALL ER_WMSG ( 'PC', ier, ' ', ier2 )
		END IF
		th_up = outdat(2)
		thp_up = outdat(3)
	    ELSE
		zup = ztop
		th_up = thtop
		thp_up = thptop
	    END IF
C
	    CALL PD_SUML ( zbot, zup, thbot, th_up, 
     +				thpbot, thp_up, cape, cins )
C
	    thbot = th_up
	    thpbot = thp_up
	    zbot = zup
	END DO
C
	RETURN
	END
