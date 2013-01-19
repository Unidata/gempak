	SUBROUTINE PS_PRCL ( datain, nparm, dfdpth, idfcrd, nocc,
     +			     depth, idcord, pavg, tavg, tdavg, uavg,
     +			     vavg, zavg, thavg, rmxavg, iret )
C************************************************************************
C* PS_PRCL								*
C*									*
C* This subroutine computes the pressure weighted average of all the	*
C* station parameters.  THTA and MIXR are added to the list of station	*
C* parameters which include TMPC, DWPC, UWND, VWND and HGHT.  The	*
C* thickness of the layer to be used will be obtained from the user	*
C* inputs.  If there is no user input, default values passed to this	*
C* subroutine will be used.  This subroutine will only compute the	*
C* average values in a layer which starts at the surface of the		*
C* dataset.								*
C*									*
C* PS_PRCL ( DATAIN, NPARM, DFDPTH, IDFCRD, NOCC, DEPTH, IDCORD, PAVG,	*
C*	     TAVG, TDAVG, UAVG, VAVG, ZAVG, THAVG, RMXAVG, IRET )	*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of dataset parameters	*
C*	DFDPTH		REAL		Default layer depth		*
C*	IDFCRD		INTEGER		Default vertical coordinate	*
C*	NOCC		INTEGER		Nth occurrence			*
C*									*
C* Output parameters:							*
C*	DEPTH		REAL		Layer depth			*
C*	IDCORD		INTEGER		Depth vertical coordinate	*
C*	PAVG		REAL		Average PRES			*
C*	TAVG		REAL		Average TMPC			*
C*	TDAVG		REAL		Average DWPC			*
C*	UAVG		REAL		Average UWND			*
C*	VAVG		REAL		Average VWND			*
C*	ZAVG		REAL		Average HGHT			*
C*	THAVG		REAL		Average THTA			*
C*	RMXAVG		REAL		Average MIXR			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-31 = layer not found		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90	GEMPAK 5				*
C* K. Brill/NMC		 1/92	Do not average underground		*
C* K. Brill/NMC		 2/92	Check for psum=0 before dividing by it	*
C* J. Nielsen/TAMU	 5/92	Average each parameter separately	*
C* S. Jacobs/EAI	 3/93	Cleaned up				*
C* T. Lee/GSC		 8/97	Returned base level data for depth = 0;	*
C*				Check missing data at the base level	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datain ( nparm, * )
C*
	REAL		stndl (10), stndb (10), stndt (10),
     +			datlev (MMPARM ), pbot (6), ptop (6),
     +			psum (6)
	LOGICAL		done, use, good (6), first (6)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the user requested depth.
C
	CALL PC_DPTH  ( datain, nparm, 0., 1, dfdpth, idfcrd, nocc,
     +			depth, idcord, stndl, stndb, stndt, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -31
	    RETURN
	  ELSE IF  ( idcord .eq. 1 )  THEN
	    vfinal = stndt (1)
	  ELSE IF  ( idcord .eq. 2 )  THEN
	    vfinal = PR_THTA ( stndt (2), stndt (1) )
	  ELSE
	    vfinal = stndt (6)
	END IF
C
C*	Return surface data if depth = 0.
C
	IF ( depth .eq. 0. ) THEN
	    pavg   = stndl (1)
	    tavg   = stndl (2)
	    tdavg  = stndl (3)
	    uavg   = stndl (4)
	    vavg   = stndl (5)
	    zavg   = stndl (6)
	    thavg  = PR_THTA (  tavg, pavg )
	    rmxavg = PR_MIXR ( tdavg, pavg )
	    RETURN
	END IF

C
C*	Get the surface data.  Return for missing pressure, height,
C*	or temperature
C
	DO  i = 1, 6
	    ptop (i)  = stndl (1)
	    first (i) = .true.
	END DO
	ttop   = stndl (2)
	tdtop  = stndl (3)
	utop   = stndl (4)
	vtop   = stndl (5)
	ztop   = stndl (6)
	thtop  = PR_THTA ( stndl (2), stndl (1) )
	rmxtop = PR_MIXR ( stndl (3), stndl (1) )
C*
	IF  ( ( ERMISS ( ptop (1) ) ) .or. ( ERMISS ( ztop ) ) .or.
     +	      ( ERMISS ( ttop ) ) .and. ( idcord .eq. 2 ) )  THEN
	    iret = -31
	    RETURN
	END IF
C
C*	Set the flag for missing data.
C
	DO  i = 1, 6
	    IF  ( ERMISS ( stndl (i) ) ) first (i) = .false.
	END DO

C
C*	Initialize sums.
C
	DO  i = 1, 6
	    psum (i)  = 0.
	END DO
	tsum   = 0.
	tdsum  = 0.
	usum   = 0.
	vsum   = 0.
	zsum   = 0.
	thsum  = 0.
	rmxsum = 0.
	ilev   = 2
	done   = .false.
C
C*	Get data for all levels in the sounding until a level above
C*	the top of the depth is reached.  The data used are the
C*	station parameters.
C
	DO  WHILE  ( .not. done )
	    CALL PC_GLEV  ( ilev, datain, nparm, datlev, ier )
	    CALL PC_COMP  ( 5, datlev, stndl, ier )
C
C*	    Test level according to coordinate system.
C
	    IF  ( idcord .eq. 1 )  THEN
		use = ( stndl (1) .gt. vfinal )
	      ELSE IF  ( idcord .eq. 3 )  THEN
		use = ( stndl (6) .lt. vfinal )
	      ELSE IF  ( idcord .eq. 2 )  THEN
		vvv = PR_THTA ( stndl (2), stndl (1) )
		use = ( vvv .lt. vfinal )
	    END IF
C
C*	    Check if this is above the top of the layer.  If so, use
C*	    data in STNDT.
	    IF  ( .not. use )  THEN
		DO  i = 1, 6
		    stndl (i) = stndt (i)
		END DO
		done = .true.
	    END IF
C
C*	    Set the flag for a valid level (all data present).
C
	    DO i = 1, 6
	        good (i) = .true.
		IF ( ERMISS ( stndl (i) ) ) good (i) = .false.
	    END DO
C
C*		Get the sums for this level.
C*		Pressure needed for all calculations
C
	    IF  ( good (1) )  THEN
C
C*		Temperature, potential temperature
C
	      IF  ( good (2) )  THEN
		pbot (2) = ptop  (2)
		ptop (2) = stndl (1)
		pdiff    = pbot (2) - ptop (2)
		tbot   = ttop
		ttop   = stndl (2)
		tsum   = tsum   + ( (ttop   + tbot)   / 2. ) * pdiff
	        psum (2)  = psum (2) + pdiff
		thbot  = thtop
		thtop  = PR_THTA ( stndl (2), stndl (1) )
		thsum  = thsum  + ( (thtop  + thbot)  / 2. ) * pdiff
	      END IF
C
C*		Dew Point, mixing ratio
C
	      IF  ( good (3) )  THEN
		pbot (3) = ptop  (3)
		ptop (3) = stndl (1)
		pdiff    = pbot (3) - ptop (3)
		tdbot  = tdtop
		tdtop  = stndl (3)
		rmxbot = rmxtop
		rmxtop = PR_MIXR ( stndl (3), stndl (1) )
C
C*		Skip if the first level data is missing.
C
		IF  ( first (3) )  THEN
		    tdsum  = tdsum  + ( (tdtop  + tdbot)  / 2. ) * pdiff
		    rmxsum = rmxsum + ( (rmxtop + rmxbot) / 2. ) * pdiff
	            psum (3)  = psum (3) + pdiff
		  ELSE
		    first (3) = .TRUE.
		END IF
	      END IF
C
C*		U component
C
	      IF  ( good (4) )  THEN
		pbot (4) = ptop  (4)
		ptop (4) = stndl (1)
		pdiff    = pbot (4) - ptop (4)
		ubot   = utop
		utop   = stndl (4)
C
C*		Skip if the first level data is missing.
C
		IF  ( first (4) )  THEN
		    usum  = usum  + ( (utop + ubot) / 2. ) * pdiff
	            psum (4)  = psum (4) + pdiff
		  ELSE
		    first (4) = .TRUE.
		END IF
	      END IF
C
C*		V component
C
	      IF  ( good (5) )  THEN
		pbot (5) = ptop  (5)
		ptop (5) = stndl (1)
		pdiff    = pbot (5) - ptop (5)
		vbot   = vtop
		vtop   = stndl (5)
C
C*		Skip if the first level data is missing.
C
		IF  ( first (5) )  THEN
		    vsum  = vsum  + ( (vtop + vbot) / 2. ) * pdiff
	            psum (5)  = psum (5) + pdiff
		  ELSE
		    first (5) = .TRUE.
		END IF
	      END IF
C
C*		Z (height)
C
	      IF  ( good (6) )  THEN
		pbot (6) = ptop  (6)
		ptop (6) = stndl (1)
		pdiff    = pbot (6) - ptop (6)
		zbot   = ztop
		ztop   = stndl (6)
		zsum   = zsum   + ( (ztop   + zbot)   / 2. ) * pdiff
	        psum (6)  = psum (6) + pdiff
	      END IF
C
C*
	    END IF
	    ilev = ilev + 1
	END DO
C
C*	Calculate averages.
C
	tavg   = RMISSD
	tdavg  = RMISSD
        thavg  = RMISSD
	rmxavg = RMISSD
	uavg   = RMISSD
	vavg   = RMISSD
	IF  ( .not. ERMISS ( ttop ) .and. psum (2) .ne. 0. )  THEN
	    tavg   = tsum   / psum (2)
	    thavg  = thsum  / psum (2)
	END IF
	IF  ( .not. ERMISS ( tdtop ) .and. psum (3) .ne. 0. )  THEN
	    tdavg  = tdsum  / psum (3)
	    rmxavg = rmxsum / psum (3)
	END IF
	IF ( .not. ERMISS (utop) .and. psum (4) .ne. 0. )  THEN
	    uavg   = usum   / psum (4)
	END IF
	IF ( .not. ERMISS (vtop) .and. psum (5) .ne. 0. )  THEN
	    vavg   = vsum   / psum (5)
	END IF
	pavg   = ( stndb (1) + stndt (1) ) / 2.
	IF ( .not. ERMISS (ztop) .and. psum (6) .ne. 0. )  THEN
	    zavg   = zsum   / psum (6)
	END IF
C*
	RETURN
	END
