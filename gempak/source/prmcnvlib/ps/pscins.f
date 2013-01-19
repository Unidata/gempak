	FUNCTION PS_CINS ( datain, nparm, tvflag )
C************************************************************************
C* PS_CINS								*
C*									*
C* This function computes the Convective Inhibition (CINS) from top of	*
C* the mixing layer to the level of free convection (LFC):		*
C*									*
C*	CINS = GRAVTY * SUMN ( DELZ * ( THP - TH ) / TH )		*
C*									*
C*	     SUMN = sum of negative arguments				*
C*	     THP  = potential temperature of a parcel from the lowest	*
C*		    500 m of the atmosphere, raised dry adiabatically	*
C*		    to the lcl and moist adiabatically thereafter	*
C*	     TH   = potential temperature of the environment		*
C*									*
C* Integration is terminated at the equilibrium level.  TVFLAG		*
C* determines if temperature or virtual temperature is used in the	*
C* calculation.								*
C*									*
C* REAL PS_CINS ( DATAIN, NPARM, TVFLAG )				*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of dataset parameters	*
C*	TVFLAG		LOGICAL		Temperature flag		*
C*									*
C* Output parameters:							*
C*	PS_CINS		REAL		Convective Inhibition		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from PS_CAPE			*
C* M. desJardins/GSFC	 7/90	Reorganized for GEMPAK 5		*
C* T. Lee/GSC		 8/97	Cleaned up; Added a temperature flag	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datain ( nparm, * )
C*
	REAL		stndl (10), stndb (10), stndt (10)
	LOGICAL		tvflag
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PS_CINS = RMISSD
	dfdpth  = 500.
	idfcrd  = 3
C
C*	Get the layer averages in the surface parcel.
C
	CALL PS_PRCL  ( datain, nparm, dfdpth, idfcrd, 1, depth,
     +			idcord, pavg, tavg, tdavg, uavg, vavg, zavg,
     +			thavg, rmxavg, ier )
	IF  ( ( ier .ne. 0 ) .or. ERMISS (pavg) .or. ERMISS (tavg) .or.
     +	        ERMISS (tdavg) )  RETURN
C
C*	Calculate LCL; convert LCL in Kelvin to Celcius, compute THTA,
C*	THTE, and MIXR at the LCL; get the height at the LCL.
C
	tlcl  = PR_TLCL ( tavg, tdavg )
	plcl  = PR_PLCL ( tavg, pavg, tlcl )
	tlclc = PR_TMKC ( tlcl )
	thta  = PR_THTA ( tlclc, plcl )
	thte  = PR_THTE ( plcl,  tlclc, tlclc )
	rmix  = PR_MIXR ( tdavg, pavg )
	CALL PC_CMDT  ( 5, 6, 7, plcl, 1, datain, stndt, ier )
	htlcl = stndt (6)
C
C*	Find the level of free convection (LFC) and compute the height.
C*	Return if LFC is missing.
C
	plfc  = PS_LFCV  ( datain, nparm, thte, tvflag )
	IF  ( ERMISS (plfc) )  THEN
	    PS_CINS = 0.
	    RETURN
	END IF
	CALL PC_CMDT  ( 5, 6, 7, plfc, 1, datain, stndt, ier )
	htlfc  = stndt (6)
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
		ttop = PR_TVRK ( ttopc, tdc, stndt (1) )
	    END IF
	  ELSE
	    ttop  = PR_TMST ( thte, stndt (1), 0. )
	    IF  ( tvflag )  THEN
		ttopc = PR_TMKC ( ttop )
		ttop = PR_TVRK ( ttopc, ttopc, stndt (1) )
	    END IF
	END IF
C
C*	Set increment, initial values, accumulation register.  Take
C*	trapezoidal integration in the negative areas.  Proceed until
C*	HTLFC is reached.
C
	zinc  = 100.
	tcum = 0.
C
	DO  WHILE ( ztop .lt. htlfc )
	    zbot  = ztop
	    tbot  = ttop
	    tebot = tetop
	    ztop  = zbot + zinc
	    CALL PC_CMDT  ( 5, 6, 7, ztop, 3, datain, stndt, ier )
C
C*	    Missing data ends the integration.  We assume that we're
C*	    done if the last difference was positive, and return a
C*	    value; otherwise, we are in the middle of negative area
C*	    and have to terminate with an error.
C
	    IF  ( ERMISS ( stndt (1) ) .or.
     +		  ERMISS ( stndt (2) ) )  THEN
		IF  ( ( tbot - tebot ) .ge. 0. )  THEN
		    PS_CINS = GRAVTY * tcum
		  ELSE
		    PS_CINS = RMISSD
		END IF
		RETURN
	    END IF
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
		    ttop = PR_TVRK ( ttopc, tdc, stndt (1) )
		END IF
	      ELSE
		ttop  = PR_TMST ( thte, stndt (1), 0. )
		IF  ( tvflag )  THEN
		    ttopc = PR_TMKC ( ttop )
		    ttop = PR_TVRK ( ttopc, ttopc, stndt (1) )
		END IF
	    END IF
	    dtbot = tbot - tebot
	    dttop = ttop - tetop
C
C*	    CASE I . . . bottom and top negative - add in contribution.
C
	    IF  ( dtbot .le. 0. )  THEN
		IF  ( dttop .le. 0. )  THEN
		    tcum = tcum + ( dtbot + dttop ) * zinc /
     +				  ( tebot + tetop )
C
C*		  CASE II . . . bottom negative, top positive - hit a
C*		  level of free convection (not necessarily HTLFC);
C*		  take partial layer.
C
		  ELSE
		    frac = dtbot / ( dtbot - dttop )
		    zfc  = zbot  + ( zinc * frac )
		    tfc  = tebot + ( ( tetop - tebot ) * frac )
		    tcum = tcum  + dtbot * ( zfc - zbot ) /
     +				   ( tebot + tfc )
		END IF
C
C*	      CASE III . . . bottom positive, top negative - hit an
C*	      equilibrium level (not necessarily HTEQ); take a
C*	      partial layer.
C
	      ELSE
		IF  ( dttop .le. 0. )  THEN
		    frac = dttop / ( dttop - dtbot )
		    zeq  = ztop  - ( zinc * frac )
		    teq  = tetop - ( ( tetop - tebot ) * frac )
		    tcum = tcum  + dttop * ( ztop - zeq ) /
     +			           ( teq + tetop )
C
C*		CASE IV . . . bottom and top positive - still in CAPE;
C*		ignore.
C
		END IF
	    END IF
	END DO
C
	PS_CINS = GRAVTY * tcum
C*
	RETURN
	END
