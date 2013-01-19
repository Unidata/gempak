	FUNCTION PS_CAPE ( datain, nparm, tvflag )
C************************************************************************
C* PS_CAPE								*
C*									*
C* This function computes the Convective Available Potential Energy	*
C* (CAPE) from top of the mixing layer to the equilibruim level (EQLV):	*
C*									*
C*	CAPE = GRAVTY * SUMP ( DELZ * ( THP - TH ) / TH )		*
C*									*
C*	     SUMP = sum of positive arguments				*
C*	     THP  = potential temperature of a parcel from the lowest	*
C*		    500 m of the atmosphere, raised dry adiabatically	*
C*		    to the lcl and moist adiabatically thereafter	*
C*	     TH   = potential temperature of the environment		*
C*									*
C* TVFLAG determines if temperature or virtual temperature is used	*
C* in the calculation.							*
C*									*
C* REAL PS_CAPE ( DATAIN, NPARM, TVFLAG )				*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of dataset parameters	*
C*	TVFLAG		LOGICAL		Temperature flag		*
C*									*
C* Output parameters:							*
C*	PS_CAPE		REAL		Convective Avail. Pot. Energy	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from PS_LIFT			*
C* K. Brill/GSC		 1/90	Corrected PR_TMST call			*
C* M. desJardins/GSFC	 7/90	GEMPAK5					*
C* S. Jacobs/NCEP	 5/96	Added a check for missing top and	*
C*				bottom theta and theta-e		*
C* T. Lee/GSC		 8/97	Cleaned up; Added a temperature flag	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* T. Lee/GSC		 9/98	Handled missing LFC and top level HGHT	*
C* T. Lee/GSC		 1/00	Bug fix in PC_FTOP call			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datain ( nparm, * )
C*
	REAL		stndl (10), stndb (10), stndt (10)
	REAL		outdat (MMPARM)
C*
	LOGICAL		tvflag, found
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PS_CAPE = 0.
	dfdpth  = 500.
	idfcrd  = 3
C
C*	Get surface parcel values.
C
	CALL PS_PRCL  ( datain, nparm, dfdpth, idfcrd, 1, depth, idcord,
     +			pavg, tavg, tdavg, uavg, vavg, zavg, thavg,
     +			rmxavg, iret )
	IF  ( ( iret .ne. 0 ) .or. ( ERMISS (pavg) ) .or.
     +	      ( ERMISS (tavg) ) .or. ( ERMISS (tdavg) ) )  THEN
	    PS_CAPE = RMISSD
	    RETURN
	END IF
C
C*	Find the LCL for the average parcel; translate Kelvin to
C*	Celcius, compute THTA, THTE, and MIXR there and find the
C*	height at the LCL.
C
	tlcl  = PR_TLCL ( tavg, tdavg )
	plcl  = PR_PLCL ( tavg, pavg, tlcl )
	tlclc = PR_TMKC ( tlcl )
	thta  = PR_THTA ( tlclc, plcl )
	thte  = PR_THTE ( plcl,  tlclc, tlclc )
	rmix  = PR_MIXR ( tdavg, pavg )
	CALL PC_CMDT  ( 5, 6, 7, plcl, 1, datain, stndl, ier )
	htlcl = stndl (6)
C
C*	Compute level of free convection (LFC).  Return if missing.
C
	plfc = PS_LFCV ( datain, nparm, thte, tvflag )
	IF  ( ERMISS ( plfc ) )  RETURN
C
C*	Retrieve top level data.
C
	CALL PC_FLVL ( -1., 1, datain, ptop, numlev, nnext, levtyp, ier )
	found = .false.
	DO  WHILE  ( .not. found .and. numlev .gt. 1 ) 
	    CALL PC_FTOP ( datain, nparm, levtop, outdat, ier )
	    IF  ( .not. ERMISS (outdat (6) ) )  THEN
		ptop = outdat (1)
		zzz  = outdat (6)
		found = .true.
	      ELSE
		numlev = numlev - 1
	    END IF 
	END DO 
	IF  ( numlev .le. 1 )  THEN
	    PS_CAPE = RMISSD
	    RETURN
	END IF
C
C*	Get temperature at the top level.
C
	IF  ( tvflag )  THEN
	    ttt = outdat (7)
	  ELSE
	    ttt = outdat (2)
	END IF
C
C*	Compute the equilibrium level.
C
	peq = PS_EQLV  ( nparm, datain, thte, tvflag )
C
C*	Compute the height of the equilibrium level.
C
	IF  ( ERMISS ( peq ) )  THEN
	    hteq = RMISSD
	  ELSE
	    CALL PC_CMDT  ( 5, 6, 7, peq, 1, datain, stndl, ier )
	    hteq  = stndl (6)
	END IF
C
C*	Get the height of top of the mixing layer. Compute the
C*	environmental and parcel temperatures.
C
	CALL PC_DPTH  ( datain, nparm, 0., 1, dfdpth, idfcrd, 1,
     +			depth, idcord, stndl, stndb, stndt, ier )
	IF  ( idcord .eq. 1 )  THEN
	    ztop = stndt (1)
	  ELSE IF  ( idcord .eq. 2 )  THEN
	    ztop = PR_THTA ( stndt (2), stndt (1) )
	  ELSE
	    ztop = stndt (6)
	END IF
	CALL PC_CMDT  ( 5, 6, 7, ztop, idcord, datain, stndt, ier )
	IF  ( tvflag )  THEN
	    tetop = PR_TMCK ( stndt (7) )
	  ELSE
	    tetop = PR_TMCK ( stndt (2) )
	END IF
C
C*	If the parcel is below the LCL, get T, Td along the THTA,
C*	RMIX.  If the parcel is at or above the LCL (parcel is
C*	saturated), find T along the moist adiabat.
C
	IF  ( ztop .lt. htlcl )  THEN
	    ttop  = PR_TMPK ( stndt (1), thta )
	    IF  ( tvflag )  THEN
		ttopc = PR_TMKC ( ttop )
		tdc   = PR_DWPT ( rmix, stndt (1) )
		ttop  = PR_TVRK ( ttopc, tdc, stndt (1) )
	    END IF
	  ELSE
	    ttop  = PR_TMST ( thte, stndt (1), 0. )
	    IF  ( tvflag )  THEN
		ttopc = PR_TMKC ( ttop )
		ttop  = PR_TVRK ( ttopc, ttopc, stndt (1) )
	    END IF
	END IF
C
C*	Set increment, initial values, accumulation register.  Take
C*	trapezoidal integration in the positive areas.  Proceed until
C*	HTEQ is reached.
C
	zinc  = 100.
	tcum = 0.
C
	DO  WHILE ( ( ztop .lt. hteq ) .or. ERMISS ( hteq ) )
	    zbot  = ztop
	    tbot  = ttop
	    tebot = tetop
	    ztop  = zbot + zinc
	    CALL PC_CMDT  ( 5, 6, 7, ztop, 3, datain, stndt, ier )
C
C*	    Missing data or reaching beyond the top of the sounding
C*	    ends the integration.  We assume that we're done if the
C*	    last difference is negative, and return a value.
C
C
	    IF  ( ERMISS ( stndt (1) ) .or.
     +		  ERMISS ( stndt (2) ) .or. ( ztop .gt. zzz ) )  THEN
		IF  ( ( tbot - tebot ) .le. 0. )  THEN
		    PS_CAPE = GRAVTY * tcum
		  ELSE
C
C*		    Otherwise, we are in the middle of positive area.
C*		    Add fractional CAPE and return a value.
C
		    tetop = PR_TMCK ( ttt )
		    IF  ( ztop .lt. htlcl )  THEN
			ttop  = PR_TMPK ( ptop, thta )
			IF  ( tvflag )  THEN
			    ttopc = PR_TMKC ( ttop )
			    tdc   = PR_DWPT ( rmix, ptop )
			    ttop  = PR_TVRK ( ttopc, tdc, ptop )
			END IF
		      ELSE
			ttop  = PR_TMST ( thte, ptop, 0. )
			IF  ( tvflag )  THEN
			    ttopc = PR_TMKC ( ttop )
			    ttop  = PR_TVRK ( ttopc, ttopc, ptop )
			END IF
		    END IF
C
		    IF  ( ERMISS ( ttop ) .or. ERMISS ( tetop ) .or.
     +			  ERMISS ( zzz  ) .or. ERMISS ( zbot  ) )  THEN
			PS_CAPE = GRAVTY * tcum
			RETURN
		      ELSE
			dttop = ttop - tetop
			dz    = zzz - zbot
		    END IF
		    tcum  = tcum + ( dtbot + dttop ) * dz /
     +			    ( tebot + tetop )
		    PS_CAPE = GRAVTY * tcum
		END IF
		RETURN
	    END IF
C
C*	    Compute temperature data.
C
	    IF  ( tvflag )  THEN
		tetop = PR_TMCK ( stndt (7) )
	      ELSE
		tetop = PR_TMCK ( stndt (2) )
	    END IF
	    IF  ( ztop .lt. htlcl )  THEN
		ttop  = PR_TMPK ( stndt (1), thta )
		IF  ( tvflag )  THEN
		    ttopc = PR_TMKC ( ttop )
		    tdc   = PR_DWPT ( rmix, stndt (1) )
		    ttop  = PR_TVRK ( ttopc, tdc, stndt (1) )
		END IF
	      ELSE
		ttop  = PR_TMST ( thte, stndt (1), 0. )
		IF  ( tvflag )  THEN
		    ttopc = PR_TMKC ( ttop )
		    ttop = PR_TVRK ( ttopc, ttopc, stndt (1) )
		END IF
	    END IF
	    IF  ( ERMISS ( tbot ) .or. ERMISS ( tebot ) .or.
     +		  ERMISS ( ttop ) .or. ERMISS ( tetop ) )  THEN
		PS_CAPE = RMISSD
		RETURN
	    END IF
	    dtbot = tbot - tebot
	    dttop = ttop - tetop
C
C*	    CASE I . . . bottom and top positive - add in contribution.
C
	    IF  ( dtbot .ge. 0. )  THEN
		IF  ( dttop .ge. 0. )  THEN
		    tcum = tcum + ( dtbot + dttop ) * zinc /
     +				  ( tebot + tetop )
C
C*		  CASE II . . . bottom positive, top negative - hit an
C*		  equilibrium level (not necessarily HTEQ, yet); take
C*		  partial layer.
C
		  ELSE
		    frac = dtbot / ( dtbot - dttop )
		    zeq  = zbot  + ( zinc * frac )
		    teq  = tebot + ( ( tetop - tebot ) * frac )
		    tcum = tcum  + dtbot * ( zeq - zbot ) /
     +				   ( tebot + teq )
		END IF
C
C*	      CASE III . . . bottom negative, top positive - hit a level
C*	      of free convection, ZFC (not necessarily the first);
C*	      take partial layer.
C
	      ELSE
		IF  ( dttop .ge. 0. )  THEN
		    frac = dttop / ( dttop - dtbot )
		    zfc  = ztop  - ( zinc * frac )
		    tfc  = tetop - ( ( tetop - tebot ) * frac )
		    tcum = tcum  + dttop * ( ztop - zfc ) /
     +			           ( tfc + tetop )
C
C*		CASE IV . . . bottom and top negative - still in CIN;
C*		ignore.
C
		END IF
	    END IF
	END DO
C
	PS_CAPE = GRAVTY * tcum
C*
	RETURN
	END
