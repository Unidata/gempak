	FUNCTION PS_LFCV ( datain, nparm, thepar, tvflag )
C************************************************************************
C* PS_LFCV								*
C*									*
C* This function returns the level of free convection for a parcel	*
C* with THTE of THEPAR.  TVFLAG determines if temperature or virtual	*
C* temperature is used in the calculation.  If there are multiple	*
C* LFC's, the one closest to the ground is returned.			*
C*									*
C* REAL PS_LFCV ( DATAIN, NPARM, THEPAR, TVFLAG )			*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of dat set parameters	*
C*	THEPAR		REAL		Parcel THTE in Kelvin		*
C*	TVFLAG		LOGICAL		Temperature flag		*
C*									*
C* Output parameters:							*
C*	PS_LFCV		REAL		Level of free convection in mb	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 9/89	Adapted from PS_EQLV			*
C* M. desJardins/GSFC	 7/90	Reorganized for GEMPAK 5		*
C* K. Brill/NMC		01/92	Avoid undergrd man lvls			*
C* K. Brill/EMC		 1/96	Set limit on passes in final loop	*
C* T. Lee/GSC		 8/97	Cleaned up; Added a temperature flag	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( EPSI = .6077 )
	REAL		datain ( nparm, * )
C*
	REAL		datlev ( MMPARM ),  outdat ( MMPARM ),
     +			spt (10), spb (10), stt (10), stb (10)
	LOGICAL		tvflag, indx
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	PS_LFCV = RMISSD
C
	IF  ( ERMISS ( thepar ) )  RETURN
C
C*	Get the surface pressure value.
C
	CALL PC_GLEV ( 1, datain, nparm, datlev, ier )
	psfc = datlev (1)
C
C*	Find the highest level which reports pressure and temperature
C*	data.
C
	CALL PC_FTOP  ( datain, nparm, numlev, outdat, ier )
	IF  ( numlev .le. 1 )  RETURN
C
C*	Get top level pressure and temperature; initialize the
C*	number of crossings.
C
	pbot    = outdat (1)
	tbpark  = PR_TMST ( thepar, pbot , 0. )
	tbpar   = PR_TMKC ( tbpark )
	IF  ( tvflag )  THEN
	    tebot  = outdat (7)
	    tbpark = PR_TVRK ( tbpar, tbpar, pbot )
	    tbpar  = PR_TMKC ( tbpark )
	  ELSE
	    tebot = outdat (2)
	END IF
	ncross = 0
C
C*	Get LCL pressure and temperature.
C
	CALL PS_PRCL  ( datain, nparm, 500., 3, 1, depth,
     +			idcord, pavg, tavg, tdavg, uavg, vavg,
     +			zavg, thavg, rmxavg, ier )
	IF  ( ERMISS ( pavg  ) .or. ERMISS ( tavg ) .or.
     +	      ERMISS ( tdavg ) .or. ( ier .ne. 0 ) )  THEN
	    RETURN
	  ELSE
	    tlcl  = PR_TLCL ( tavg, tdavg )
	    plcl  = PR_PLCL ( tavg, pavg, tlcl )
	    CALL PC_CMDT ( 5, 6, 7, plcl, 1, datain, outdat, ier )
	    IF  ( ier .ne. 0 )  RETURN
	END IF
C
C*	Loop down through the sounding finding plus-to-minus crossings.
C*	Missing PRES or TMPC is an error.
C
	indx = .false.
	DO  WHILE ( (numlev .gt. 1) .and. (pbot .le. plcl) )
	  ptop  = pbot
	  tetop = tebot
	  tupar = tbpar
	  numlev = numlev - 1
	  CALL PC_GLEV  ( numlev, datain, nparm, datlev, ier )
	  CALL PC_COMP  ( 5,    datlev, outdat, ier )
	  IF ( datlev (1) .le. psfc ) THEN
	    IF  ( ERMISS (outdat(1)) .or. ERMISS (outdat(2)) )  RETURN
	    pbot   = outdat (1)
	    tbpark = PR_TMST ( thepar, pbot, 0. )
	    tbpar  = PR_TMKC ( tbpark )
	    IF  ( tvflag )  THEN
		tebot  = outdat (7)
		tbpark = PR_TVRK ( tbpar, tbpar, pbot )
		tbpar  = PR_TMKC ( tbpark )
	      ELSE
		tebot  = outdat (2)
	    END IF
	    IF  ( tupar .gt. tetop )  indx = .true.
C
C*	    If we've hit a crossing and still have room in the storage
C*	    arrays, save useful parameters.
C
	    IF  ( ( tetop .lt. tupar ) .and.
     +		  ( tebot .ge. tbpar ) .and. ( ncross .lt. 10 ) )  THEN
		ncross       = ncross + 1
		spt (ncross) = ptop
		spb (ncross) = pbot
		stt (ncross) = tetop
		stb (ncross) = tebot
	    END IF
	  END IF
	END DO
C
C*      If no plus-to-minus crossings occurred, return PLCL when
C*	the parcel's temperature is warmer than the environment's.
C
        IF  ( ncross .eq. 0 )  THEN
	    IF  ( indx )  PS_LFCV = plcl
	    RETURN
	  ELSE
C
C*	  More than 1 crossing; pick the lowest one.
C
	    ic = ncross
	END IF
C
C*	Interpolation for virtual temperature case:
C*
C*	Assume linear LN(P) variation for the temperature and mixing
C*	ratio (q).  Find the linear equations for T and q, and equate
C*	the virtual temperatures at the intersecting point for the
C*	parcel and the environment. Solve for P.
C
	IF  ( tvflag )  THEN
C
C*	    Get enviromental data at the pressure levels.
C
	    CALL PC_CMDT ( 5, 6, 7, spt (ic), 1, datain, outdat, ier )
	    ptt  =  PR_TMCK ( outdat (2) )
	    IF  ( ERMISS ( outdat (3) ) ) outdat (3) = outdat (2)
	    pmx  =  PR_MIXR ( outdat (3), spt (ic) )
	    CALL PC_CMDT ( 5, 6, 7, spb (ic), 1, datain, outdat, ier )
	    qtt  =  PR_TMCK ( outdat (2) )
	    IF  ( ERMISS ( outdat (3) ) ) outdat (3) = outdat (2)
	    qmx  =  PR_MIXR ( outdat (3), spb (ic) )
C
C*	    Compute linear coefficients for T and q on the
C*	    environmental temperature profile.
C
	    rpb  = ALOG ( spb (ic) )
	    rpt  = ALOG ( spt (ic) )
	    pdf  = ( rpb - rpt )
	    ae   = ( qmx - pmx ) * .001 / pdf
	    be   = ( pmx * rpb - qmx * rpt ) *.001 / pdf
	    ce   = ( qtt - ptt ) / pdf
	    de   = ( ptt * rpb - qtt * rpt ) / pdf
C
C*	    Get parcel data at the pressure levels.
C
	    xtt  =  PR_TMST ( thepar, spt (ic), 0. )
	    xttc =  PR_TMKC ( xtt )
	    xmx  =  PR_MIXR ( xttc, spt (ic) )
	    ytt  =  PR_TMST ( thepar, spb (ic), 0. )
	    yttc =  PR_TMKC ( ytt )
	    ymx  =  PR_MIXR ( yttc, spb (ic) )
C
C*	    Compute linear coefficients for T and q on the parcel
C*	    temperature profile.
C
	    ap   = ( ymx - xmx ) * .001 / pdf
	    bp   = ( xmx * rpb - ymx * rpt ) * .001 / pdf
	    cp   = ( ytt - xtt ) / pdf
	    dp   = ( xtt * rpb - ytt * rpt ) / pdf
C
C*	    Solve for P.
C
	    a    = ae * ce - ap * cp
	    b    = ( be * ce - bp * cp ) + ( ae * de - ap * dp )
     +		   + ( ce - cp ) / EPSI
	    c    = be * de - bp * dp + ( de - dp ) / EPSI
	    pest = ( - b + ( b ** 2 - 4. * a * c ) ** .5 ) / ( 2. * a )
	    pest = EXP ( pest )
	  ELSE
C
C*	    Interpolation for temperature case:
C*
C*	    Do a linear interpolation in LN(P) to get PEST, an estimate
C*	    of the pressure that corresponds to THEPAR.  Iterately get
C*	    the interpolated data at PEST, compute the saturated THTE
C*	    at that level, and do a linear interpolation in LN(P)
C*	    for the new PEST.
C
	    oldest = RMISSD
	    stt (ic ) = PR_THTE ( spt (ic), stt (ic), stt (ic) )
	    stb (ic ) = PR_THTE ( spb (ic), stb (ic), stb (ic) )
	    pest   = spb (ic) *
     +		   ( spt (ic) / spb (ic) ) ** ( (thepar   - stb (ic)) /
     +					      (stt (ic) - stb (ic)) )
C
	    kcnt = 200
	    DO  WHILE ( ABS ( pest - oldest ) .gt. 0.01 .and.
     +			kcnt .gt. 0 )
		oldest = pest
		IF  ( pest .le. spt (ic) .or. pest .ge. spb (ic) )  THEN
		    PS_LFCV = pest
		    RETURN
		END IF
		CALL PC_CMDT ( 5, 6, 7, oldest, 1, datain, outdat, ier )
C
		teest = PR_THTE ( outdat (1), outdat (2), outdat (2) )
		IF  ( teest .gt. thepar )  THEN
		    rtt = stt (ic)
		    rpt = spt (ic)
		  ELSE
		    rtt = stb (ic)
		    rpt = spb (ic)
		END IF
		pest  = oldest * ( rpt / oldest ) **
     +			( ( thepar - teest ) / ( rtt - teest ) )
		kcnt = kcnt - 1
	    END DO
	    IF ( kcnt .eq. 0 ) pest = RMISSD
	END IF
C*
	IF ( pest .gt. plcl ) pest = plcl
	PS_LFCV = pest
	RETURN
	END
