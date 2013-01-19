	FUNCTION PS_EQLV ( nparm, data, thepar, tvflag )
C************************************************************************
C* PS_EQLV								*
C*									*
C* This function returns the lowest pressure along the sounding at	*
C* which the equivalent potential temperature of the sounding is	*
C* equal to THEPAR.  TVFLAG determines if temperature or virtual	*
C* temperature is used in the calculation.				*
C*									*
C* REAL PS_EQLV ( NPARM, DATA, THEPAR, TVFLAG )				*
C*									*
C* Input parameters:							*
C*	NPARM		INTEGER		Number of dat set parameters	*
C*	DATA (NPARM,*)	REAL		Station data			*
C*	THEPAR		REAL		Parcel THTE in Kelvin		*
C*	TVFLAG		LOGICAL		Temperature flag		*
C*									*
C* Output parameters:							*
C*	PS_EQLV		REAL		Equilibrium level in millibars	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from PS_LIFT			*
C* J. Nielsen/TAMU	 5/92	Do not check below surface		*
C* S. Jacobs/EAI	 3/93	Cleaned up				*
C* P. Neilley/NCAR	 6/94	Ignore missing data; changed iterations *
C* T. Lee/GSC		 8/97	Cleaned up; added a temperature flag	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( EPSI = .6077 )
	REAL		data ( nparm, * )
C*
	REAL		datlev ( MMPARM ), outdat ( 10 )
	LOGICAL		good, belowg, tvflag
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PS_EQLV = RMISSD
	iret = 0
C
	IF  ( ERMISS ( thepar ) )  RETURN
C
C*	Find the highest level which reports pressure and temperature
C*	data.  Get the temperature.
C
	CALL PC_FTOP  ( data, nparm, numlev, outdat, ier )
	IF  ( numlev .le. 1 )  RETURN
	pbot  = outdat (1)
	IF  ( tvflag )  THEN
	    tebot = outdat (7)
	  ELSE
	    tebot = outdat (2)
	END IF
C
C*	Compute top level parcel temperature. Return if the environmetal
C*	temperature is less than the parcel's.
C
	tpbotk = PR_TMST ( thepar, pbot, 0. )
	tpbot  = PR_TMKC ( tpbotk )
	IF  ( tvflag )  THEN
	    tpbotk = PR_TVRK ( tpbot, tpbot, pbot )
	    tpbot  = PR_TMKC ( tpbotk )
	END IF
	IF  ( tebot .lt. tpbot )  RETURN
C
C*	Get LFC pressure.  Return if it's missing.
C
	plfc  = PS_LFCV ( data, nparm, thepar, tvflag )
	IF  ( ERMISS ( plfc ) )  RETURN
C
C*	Loop down through the sounding until the first crossing is hit.
C*	Missing PRES or TMPC is an error.
C
	good = .true.
	belowg = .false.
	DO  WHILE ( ( tebot .gt. tpbot ) .and. ( good ) .and.
     +		    ( .not. belowg ) )
	    ptop   = pbot
	    tetop  = tebot
	    numlev = numlev - 1
	    IF  ( numlev .eq. 0 )  THEN
		good = .false.
	      ELSE
		CALL PC_GLEV  ( numlev, data, nparm, datlev, ier )
		CALL PC_COMP  ( 5,    datlev, outdat, ier )
C
C*	        At or below LFC?
C
		IF  ( outdat (1) .ge. plfc )  THEN
		    belowg = .true.
C
C*	        Embedded missing data?  If so, skip this level.
C*              Also skip this level if thte not calculable.
C
		  ELSE IF  ( ( .not. ERMISS ( outdat(1) ) ) .and.
     +			     ( .not. ERMISS ( outdat(2) ) ) )  THEN

		    pbot  = outdat (1)
		    IF  ( tvflag )  THEN
			tebot = outdat (7)
		      ELSE
			tebot = outdat (2)
		    END IF
		    tpbotk = PR_TMST ( thepar, pbot, 0. )
		    tpbot  = PR_TMKC ( tpbotk )
		    IF  ( tvflag )  THEN
			tpbotk = PR_TVRK ( tpbot, tpbot, pbot )
			tpbot  = PR_TMKC ( tpbotk )
		    END IF
		END IF
	    END IF
	END DO
C
C*	If sounding is not complete, return missing.
C
	IF  ( .not. good )  RETURN
C
C*	If we've traversed the whole sounding, return missing.
C
	IF  ( belowg )  RETURN
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
C*	    Get enviromental data at the pressure levels..
C
	    CALL PC_CMDT ( 5, 6, 7, ptop, 1, data, outdat, ier )
	    ptt  =  PR_TMCK ( outdat (2) )
	    IF  ( ERMISS ( outdat (3) ) ) outdat (3) = outdat (2)
	    pmx  =  PR_MIXR ( outdat (3), ptop )
	    CALL PC_CMDT ( 5, 6, 7, pbot, 1, data, outdat, ier )
	    qtt  =  PR_TMCK ( outdat (2) )
	    IF  ( ERMISS ( outdat (3) ) ) outdat (3) = outdat (2)
	    qmx  =  PR_MIXR ( outdat (3), pbot )
C
C*	    Compute linear coefficients for T and q on the
C*	    environmental temperature profile.
C
	    rpb  = ALOG ( pbot )
	    rpt  = ALOG ( ptop )
	    pdf  = ( rpb - rpt )
	    ae   = ( qmx - pmx ) * .001 / pdf
	    be   = ( pmx * rpb - qmx * rpt ) *.001 / pdf
	    ce   = ( qtt - ptt ) / pdf
	    de   = ( ptt * rpb - qtt * rpt ) / pdf
C
C*	    Get parcel data at the pressure levels.
C
	    xtt  =  PR_TMST ( thepar, ptop, 0. )
	    xttc =  PR_TMKC ( xtt )
	    xmx  =  PR_MIXR ( xttc, ptop )
	    ytt  =  PR_TMST ( thepar, pbot, 0. )
	    yttc =  PR_TMKC ( ytt )
	    ymx  =  PR_MIXR ( yttc, pbot )
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
	    a    = ap * cp - ae * ce
	    b    = ( bp * cp - be * ce ) + ( ap * dp - ae * de )
     +		   + ( cp - ce ) / .6078
	    c    = bp * dp - be * de + ( dp - de ) / .6078
	    pest = ( - b + ( b ** 2 - 4. * a * c ) ** .5 ) / ( 2. * a )
	    pest = EXP ( pest )
	    PS_EQLV = pest
	  ELSE
C
C*	    Interpolation for temperature case:
C*
C*	    Do a linear interpolation in LN(P) to get PEST, an estimate
C*	    of the pressure that corresponds to THEPAR.  Iterately get
C*	    the interpolated data at PEST, compute the saturated THTE
C*	    at that level, and do a linear interpolation in LN(P) for
C*	    the new PEST.
C
	    tebot = PR_THTE ( pbot, tebot, tebot )
	    tetop = PR_THTE ( ptop, tetop, tetop )
C
	    oldest = RMISSD
	    pest   = pbot * ( ptop / pbot ) ** ( (thepar - tebot) /
     +					         (tetop  - tebot) )
C
C*	    Iterate until guess is within "close". Changed to 1.0 from
C*	    0.01.  Also iterate only until mxiter is achieved.
C
            close = 1.0
            mxiter = 100
            n = 0
	    DO  WHILE ( ( ABS ( pest - oldest ) .gt. close ) .and.
     +		        ( n .le. mxiter ) )
	        n = n + 1
	        oldest = pest
	        CALL PC_CMDT  ( 5, 6, 7, oldest, 1, data, outdat, ier )
C
	        teest = PR_THTE ( outdat (1), outdat (2), outdat (2) )
	        pest  = oldest * ( ptop / oldest ) **
     +			( (thepar - teest) / (tetop  - teest) )
	    END DO
C*
	    IF  ( n .gt. mxiter )  THEN
	        PS_EQLV = RMISSD
	      ELSE
	        PS_EQLV = pest
	    END IF
	END IF
C*
	RETURN
	END
