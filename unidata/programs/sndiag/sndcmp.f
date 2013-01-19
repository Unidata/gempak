	SUBROUTINE SNDCMP  ( nparms, rlevel, nlev, levtyp, lvert,
     +			     sndata, numlev, parms, wavlen, wavspd, 
     +			     storm, crdrot, tropht, trpint, cldhgt, 
     +			     mxdpth, squall, delz, filtyp, spline, 
     +			     nlun, lun, dattim, nlevel, hdata, iret )
C************************************************************************
C* SNDCMP								*
C*									*
C* This subroutine gets the requested sounding data and computes the	*
C* parameters at all the station levels.				*
C*									*
C* SNDCMP  ( NPARMS, RLEVEL, NLEV, LEVTYP, LVERT, SNDATA, NUMLEV, 	*
C*	     PARMS, WAVLEN, WAVSPD, STORM, CRDROT, TROPHT, TRPINT, 	*
C*	     CLDHGT, MXDPTH, SQUALL, DELZ, FILTYP, SPLINE, NLUN, LUN,	*
C*	     DATTIM, NLEVEL, HDATA, IRET )				*
C*									*
C* Input parameters:							*
C*	NPARMS		INTEGER		Number of parameters		*
C*	RLEVEL (LLMXLV)	REAL		Data levels			*
C*	NLEV		INTEGER		Number of levels		*
C*	LEVTYP		INTEGER		Level type			*
C*	LVERT		INTEGER		Vertical coordinate		*
C*	SNDATA (LLMXLV)	REAL		Original sound data		*
C*	NUMLEV		INTEGER		# of original sounding levels	*
C*	PARMS (NPARMS)	CHAR*		Parameters			*
C*	WAVLEN		CHAR*		Length of wave			*
C*	WAVSPD		CHAR*		Speed of wave			*
C*	STORM		CHAR*		Storm speed and direction	*
C*	CRDROT		CHAR*		Coordinate rotations		*
C*	TROPHT		CHAR*		Tropopause height		*
C*	TRPINT		CHAR*		Interval above and below trop	*
C*	CLDHGT		CHAR*		Height of cloud base		*
C*	MXDPTH		CHAR*		Height of mixed layer depth	*
C*	SQUALL		CHAR*		Length of squall line		*
C*	DELZ		CHAR*		Height interval for new sounding*
C*	FILTYP		CHAR*		Filter type			*
C*	SPLINE		LOGICAL		Whether to use splines or not	*
C*	NLUN		INTEGER		Number of file numbers		*
C*	LUN (NLUN)	INTEGER		Output file number		*
C*	DATTIM		CHAR*		Time				*
C*									*
C* Output parameters:							*
C*	NLEVEL		INTEGER		Number of new sounding levels	*
C*	HDATA (LLMXLV)	REAL		New sounding data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92		Copied from SNOCMP		*
C* J. Whistler/SSAI	 4/93		Cleaned up			*
C* J. Whistler/SSAI	 5/93		Filter all of the data		*
C* J. Whistler/SSAI	 6/93		Added check to see if DWPT is	*
C*					interpolated higher than TEMP	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	PARAMETER	( NUMPTL = 8, NUMPTH = 15 )
	PARAMETER	( NMARL = NUMPTL+1, NMARH = NUMPTH+1 )
C*
	CHARACTER*(*)	wavlen, wavspd, storm, crdrot, tropht, trpint,
     +			cldhgt, mxdpth, squall, delz,  filtyp, parms(*),
     +			dattim
	REAL		rlevel(*), sndata(*), hdata(*)
	LOGICAL		spline
	INTEGER		lun(*)
C*
	CHARACTER	cdata(LLMXDT)
	REAL		hghts(LLMXDT), rdata(LLMXDT),
     +			thtv(LLMXLV), uwnd(LLMXLV), vwnd(LLMXLV),
     +			uabs(LLMXLV), vabs(LLMXLV),
     +			filarl(NMARL), filarh(NMARH)
	LOGICAL		chrflg(MMPARM), cmpflg(MMPARM)
C*
	DATA		filarl /  .3200156553,  .2643210644,
     +				  .1343767864,  .0114831529,
     +				 -.0458900942, -.0377520979,
     +				 -.0064753717,  .0111929411,
     +				  .0087357913 /
C
	DATA		filarh /  .0974726419,  .0954676702,
     +				  .0896329731,  .0804876892,
     +				  .0688283154,  .0556356004,
     +				  .0419626250,  .0288191067,
     +				  .0170670479,  .0073407153,
     +				 0.0000000000, -.0048789007,
     +				 -.0074769889, -.0081766107,
     +				 -.0074864031, -.0059591606 /
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	levout = 0
	knt    = 1
C
C*	Check for list of levels or range.
C
	IF  ( levtyp .eq. 1 )  THEN
C
C*	    Always compute surface data first.
C
	    CALL PC_CMVR  ( 0., lvert, sndata, rdata (knt), 
     +			    cdata (knt), ier )
	    IF  ( rlevel (1) .eq. 0. )  THEN
		istart = 2
	      ELSE
		istart = 1
	    END IF
C
C*	    Loop through the list of levels.
C
	    levout = 1
	    knt    = nparms + 1
	    DO  lev = istart, nlev
		CALL PC_CMVR  ( rlevel (lev), lvert, sndata, 
     +				rdata (knt), cdata (knt), ier )
		IF  ( .not. ERMISS (rdata (knt) ) )  THEN
		    levout = levout + 1
		    knt    = knt + nparms
		END IF
	    END DO
C
C*	    Check for surface as only data.
C
	    IF  ( ( levout .eq. 1 ) .and. ( rlevel (1) .ne. 0. ) )  THEN
		levout = 0
	    END IF
C
C*	    Check for range with increment.
C
	ELSE IF  ( levtyp .eq. 2 )  THEN			
C
C*	    Find bottom level.
C
	    CALL PC_FLVL  ( rlevel (1), lvert, sndata, begin, lvnum1,
     +			    lvnum2, levloc, ier )
	    IF  ( levloc .eq. 1 )  THEN
		ilev = lvnum1
	      ELSE IF  ( levloc .eq. 2 )  THEN
		ilev = lvnum2
	      ELSE IF  ( levloc .eq. 3 )  THEN
		ilev = 1
	      ELSE
		ilev = numlev + 1
	    END IF
C
C*	    Find top level.
C
	    CALL PC_FLVL  ( rlevel (2), lvert, sndata, end, lvnum1,
     +			    lvnum2, levloc, ier )
	    IF  ( levloc .eq. 1 )  THEN
		jlev = lvnum1
	      ELSE IF  ( levloc .eq. 2 )  THEN
		jlev = lvnum1
	      ELSE IF  ( levloc .eq. 4 )  THEN
		jlev = numlev
	      ELSE
		jlev = 0
	    END IF
C
C*	    Always compute surface data first.
C
	    CALL PC_CMVR  ( 0., lvert, sndata, rdata (knt), 
     +			    cdata (knt), ier )
	    IF  ( ilev .eq. 1 )  THEN
		iilev = 2
	      ELSE
		iilev = ilev
	    END IF
C
C*	    Compute data at each level.
C
	    levout = 1
	    knt    = nparms + 1
	    DO  lev = iilev, jlev
		CALL PC_CMLV  ( lev, sndata, rdata (knt), cdata (knt), 
     +				ier )
		IF  ( .not. ERMISS  ( rdata (knt) ) )  THEN
		    levout = levout + 1
		    knt    = knt + nparms
		END IF
	    END DO
C
C*	    Check for surface as only data.
C
	    IF  ( ( levout .eq. 1 ) .and. ( ilev .ne. 1 ) )  THEN
		levout = 0
	    END IF
	END IF
C
C*	Change missing dewpoints to 40 degrees less than the TEMP.
C
	ll = 1
	DO  n = 1, levout
	    IF  ( ERMISS(rdata((n-1)*nparms+ITEMP)) .and.
     +		  ERMISS(rdata((n-1)*nparms+IDWPT)) )  THEN
	    ELSE
		rdata((ll-1)*nparms+IHGHT) = rdata((n-1)*nparms+IHGHT)
		rdata((ll-1)*nparms+IPRES) = rdata((n-1)*nparms+IPRES)
		rdata((ll-1)*nparms+ITEMP) = rdata((n-1)*nparms+ITEMP)
		rdata((ll-1)*nparms+IDWPT) = rdata((n-1)*nparms+IDWPT)
		rdata((ll-1)*nparms+IUWND) = rdata((n-1)*nparms+IUWND)
		rdata((ll-1)*nparms+IVWND) = rdata((n-1)*nparms+IVWND)
		rdata((ll-1)*nparms+ITHTV) = rdata((n-1)*nparms+ITHTV)
c	write(6,'(7f9.2)' ) rdata((ll-1)*nparms+IHGHT),
c     +			rdata((ll-1)*nparms+IPRES),
c     +			rdata((ll-1)*nparms+ITEMP),
c     +			rdata((ll-1)*nparms+IDWPT),
c     +			rdata((ll-1)*nparms+IUWND),
c     +			rdata((ll-1)*nparms+IVWND),
c     +			rdata((ll-1)*nparms+ITHTV)
	    	IF  ( ERMISS(rdata((ll-1)*nparms+IDWPT)) )
     +		    rdata((ll-1)*nparms+IDWPT) =
     +			rdata((ll-1)*nparms+ITEMP) - 40.
		ll = ll + 1
	    END IF
	END DO
	levout = ll - 1
C
C*	Find average DELZ and put data at these levels. 
C*	Also give the user a chance to exit.
C
	CALL SNDARR ( delz, nparms, levout, rdata,
     +		      nlevel, hghts, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Initialize the output array to missing.
C
	DO  i = 1, nlevel*nparms
	    hdata(i) = RMISSD
	END DO
C
	IF  ( spline )  THEN
C
C*	    Fit cubic splines to the data...
C
	    DO  n = 1, ITHTV
		CALL SNDSPL ( nparms, n, levout, rdata,
     +			      nlevel, hghts, hdata, iret )
	    END DO
	    IF  ( nparms .gt. NVAR-2 )  THEN
		CALL SNDSPL ( nparms, ILATI, levout, rdata,
     +			      nlevel, hghts, hdata, iret )
		CALL SNDSPL ( nparms, ILONI, levout, rdata,
     +			      nlevel, hghts, hdata, iret )
	    END IF
	ELSE
C
C*	    ...or interpolate to the new height surfaces.
C
	    CALL SNDINT ( nparms, levout, rdata,
     +			  nlevel, hghts, hdata, iret )
	END IF
C
C*	Check to see if Dewpoint is less than temperature.
C
	DO i = 1, nlevel
	    IF ((( .not. ERMISS ( hdata((i-1)*nparms+IDWPT) )) .and.
     +		 ( .not. ERMISS ( hdata((i-1)*nparms+ITEMP) ))) .and. 
     +		( hdata((i-1)*nparms+IDWPT) .gt. 
     +		  hdata((i-1)*nparms+ITEMP) ) )  
     +		  hdata((i-1)*nparms+IDWPT) = hdata((i-1)*nparms+ITEMP)
	END DO
C
C*	Filter all of the data, if requested. (See below.)
C
	CALL ST_LCUC ( filtyp, filtyp, ier )
	IF  ( filtyp .eq. 'LIGHT' )  THEN
C
C*	    Apply a LIGHT or MESO-PASS filter.
C
C*	    Filter UWND.
C
	    CALL SNDFLT ( NUMPTL, filarl, IUWND, nparms, nlevel,
     +			  hdata, uabs, iret )
C
C*	    Filter VWND.
C
	    CALL SNDFLT ( NUMPTL, filarl, IVWND, nparms, nlevel,
     +			  hdata, vabs, iret )
C
	ELSE IF  ( filtyp .eq. 'HEAVY' )  THEN
C
C*	    Apply a HEAVY or MACRO-PASS filter.
C
C*	    Filter UWND.
C
	    CALL SNDFLT ( NUMPTH, filarh, IUWND, nparms, nlevel,
     +			  hdata, uabs, iret )
C
C*	    Filter VWND.
C
	    CALL SNDFLT ( NUMPTH, filarh, IVWND, nparms, nlevel,
     +			  hdata, vabs, iret )
C
	ELSE
C
C*	    Use NO filter.
C
	    DO  i = 1, nlevel
		uabs(i) = hdata((i-1)*nparms+IUWND)
		vabs(i) = hdata((i-1)*nparms+IVWND)
	    END DO
C
	END IF
	DO  i = 1, nlevel
	    hdata((i-1)*nparms+IUABS) = uabs(i)
	    hdata((i-1)*nparms+IVABS) = vabs(i)
	END DO
C
C*	Calculate the components of the shear for the interpolated
C*	winds, before computing the relative winds.
C
	CALL SNDSHR ( nparms, nlevel, delz, hdata, ier )
C
C*	Calculate relative winds.
C
	CALL SNDWND ( storm, crdrot, nparms, nlevel, hdata, iret )
C
C*	Filter all of the data.
C*	The reference for this procedure is:
C*	    Madden, et al., NCAR Tech note [NCAR-TN/STR-55] (1971).
C
	CALL ST_LCUC ( filtyp, filtyp, ier )
	DO iii = 1, 7
	    IF  ( filtyp .eq. 'LIGHT' )  THEN
C
C*	        Apply a LIGHT or MESO-PASS filter.
C
C*	        Filter THTV.
C
	        CALL SNDFLT ( NUMPTL, filarl, iii, nparms, nlevel,
     +			      hdata, thtv, iret )
	    ELSE IF  ( filtyp .eq. 'HEAVY' )  THEN
C
C*	        Apply a HEAVY or MACRO-PASS filter.
C
C*	        Filter THTV.
C
	        CALL SNDFLT ( NUMPTH, filarh, iii, nparms, nlevel,
     +			      hdata, thtv, iret )
C
	    ELSE
C
C*	        Use NO filter.
C
	        DO  i = 1, nlevel
		    thtv(i) = hdata((i-1)*nparms+iii)
	        END DO
C
	    END IF
C
C*	    Set the data in the large array to the filtered values.
C
	    DO  i = 1, nlevel
	        hdata((i-1)*nparms+iii) = thtv(i)
	    END DO
	END DO
C
C*	Check to see if Dewpoint is less than temperature.
C
	DO i = 1, nlevel
	    IF ((( .not. ERMISS ( hdata((i-1)*nparms+IDWPT) )) .and.
     +		 ( .not. ERMISS ( hdata((i-1)*nparms+ITEMP) ))) .and. 
     +		( hdata((i-1)*nparms+IDWPT) .gt. 
     +		  hdata((i-1)*nparms+ITEMP) ) )  
     +		  hdata((i-1)*nparms+IDWPT) = hdata((i-1)*nparms+ITEMP)
	END DO
C
	jnumlv = nlevel
C
C*	Calculate the:	Intrinsic Frequency		INFQ
C*			Brunt-Vaisala/Intrinsic		BVIN
C*			Scorer Parameter		SCOR
C*			Term 1 of Scorer Parameter	SCR1
C*			Term 2 of Scorer Parameter	SCR2
C
	CALL SNDSCR ( nparms, nlevel, wavlen, wavspd, delz,
     +		      hdata, ier )
C
C*	Compute the mixed layer quantities.
C
	CALL SNDMIX ( nlun, lun, mxdpth, nparms, nlevel, hdata, 
     +		      tlcl, plcl, thtwlcl, hlcl, iret )
C
C*	Compute the air and moisture flux.
C
	CALL SNDFLX ( squall, nparms, nlevel, hdata, iret )
C
C*	Compute the wind and lapse rate above and below the trop.
C
	CALL SNDTRP ( tropht, trpint, delz, nlun, lun, nparms, nlevel,
     +		      hdata, iret )
C
C*	Compute the layer properties of the wind.
C
	CALL SNDLYR ( tropht, delz, cldhgt, hlcl, nlun, lun,
     +		      nparms, nlevel, hdata, uabs, vabs, iret )
C
C*	Compute an estimation for the tropopause temp and hght.
C
	CALL SNDEST ( delz, nlun, lun, nparms, nlevel, hdata, iret )
C
C*	Compute the helicity at each level.
C
	CALL SNDHLC ( nparms, nlevel, hdata, iret )
C
C*	Compute the lid strength index, etc.
C
C	CALL SNDLID ( nlun, lun, dattim, nparms, levout, rdata, iret )
C*
	RETURN
	END
