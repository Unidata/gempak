 	SUBROUTINE RA_PTDA  ( irpntr, ipwbef, ipwaft, iptbef, ipaaft,
     +			      pres, tmpf, dwpf, alti, iret )
C************************************************************************
C* RA_PTDA								*
C*									*
C* This subroutine decodes the pressure, temperature, dewpoint and	*
C* altimeter in an airways report.					*
C*									*
C* RA_PTDA  ( IRPNTR, IPWBEF, IPWAFT, IPTBEF, IPAAFT, PRES, TMPF,	*
C*            DWPF, ALTI, IRET )					*
C*									*
C* Input parameters:							*
C*	IRPNTR		INTEGER		First field after header	*
C*	IPWBEF		INTEGER		Last field before wind		*
C*	IPWAFT		INTEGER		First field after wind		*
C*									*
C* Output parameters:							*
C*	IPTBEF		INTEGER		Last field before pres		*
C*	IPAAFT		INTEGER		First field after altimeter	*
C*	PRES		REAL		Pressure in millibars		*
C*	TMPF		REAL		Temperature in F		*
C*	DWPF		REAL		Dewpoint in F			*
C*	ALTI		REAL		Altimeter in inches		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* B. Doty/RDS		11/87						*
C* I. Graffman/RDS	 4/88						*
C* M. desJardins/GSFC	 9/89	GEMPAK 5				*
C* K. Tyle/GSC		 3/97	Check for estimated altimeter setting	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
	LOGICAL		cont, slash, dmiss, tmiss
C------------------------------------------------------------------------
	iret   = 0 
	tmpf   = RMISSD
	dwpf   = RMISSD
	pres   = RMISSD
	alti   = RMISSD
	ialti  = IMISSD
	ipres  = IMISSD
	iptbef = ipwbef
	IF  ( iptbef .lt. irpntr )  THEN
	    cont = .false.
	  ELSE
	    cont   = .true.
	END IF
C
C*	Now look for a slash and check pointer.
C
	IF  ( cont .and. ( cfield ( iptbef ) .eq. '/' ) )  THEN 
	    iptbef = iptbef - 1
	    slash  = .true.
	    IF  ( iptbef .lt. irpntr )  cont = .false.
C*
	  ELSE IF  ( ( iftype (iptbef) .eq. 2 ) .or.
     +		     ( cfield (iptbef) .eq. 'M' ) )  THEN
	    slash = .false.
C
C*	    Do nothing if this is a number or an "M" indicating the 
C*	    slash was missing.
C
	  ELSE
	    cont = .false.
	END IF
C
C*	The next field is the dewpoint.  If the field is non-numeric,
C*	leave dewpoint as missing.
C
	IF  ( cont .and. ( iftype ( iptbef ) .eq. 2 ) )  THEN
	    dwpf   = FLOAT ( ifintg (iptbef) )
	    dmiss  = .false.
	    iptbef = iptbef - 1
	  ELSE IF  ( cfield ( iptbef ) .eq. 'M' )  THEN
	    dmiss  = .true.
	    iptbef = iptbef - 1
	  ELSE IF  ( cfield ( iptbef ) .ne. '/' )  THEN
	    dmiss  = .true.
	    cont   = .false.
	END IF
	IF  ( iptbef .lt. irpntr )  cont = .false.
C
C*	Now look for a slash and check pointer.
C
	IF  ( cont .and. ( cfield ( iptbef ) .eq. '/' ) )  THEN 
	    iptbef = iptbef - 1
	    IF  ( iptbef .lt. irpntr )  cont = .false.
C*
C--	  ELSE IF  ( ( iftype (iptbef) .eq. 2 ) .or.
C--     +		     ( cfield (iptbef) .eq. 'M' ) )  THEN
C
C*	    Do nothing if this is a number or an "M" indicating the 
C*	    slash was missing.
C
	  ELSE
	    cont = .false.
C
C*	    If there is no temperature and the dewpoint was found
C*	    without a slash, it was probably visibility.  Reset
C*	    pointers.
C
	    IF  ( .not. slash )  THEN
		dwpf   = RMISSD
		iptbef = iptbef + 1
	    END IF
	END IF
C
C*	The next field is the temperature.  If the field is non-numeric,
C*	leave temperature as missing.
C
	IF  ( cont .and. ( iftype ( iptbef ) .eq. 2 ) )  THEN
	    tmpf   = FLOAT ( ifintg (iptbef) )
	    tmiss  = .false.
	    iptbef = iptbef - 1
	  ELSE IF  ( cfield ( iptbef ) .eq. 'M' )  THEN
	    iptbef = iptbef - 1
	    tmiss  = .true.
	  ELSE IF  ( cfield ( iptbef ) .ne. '/' )  THEN
	    cont   = .false.
	    tmiss  = .true.
	END IF
	IF  ( iptbef .lt. irpntr )  cont = .false.
C
C*	Now look for a slash and check pointer.
C
	IF  ( cont .and. ( cfield ( iptbef ) .eq. '/' ) )  THEN 
	    iptbef = iptbef - 1
	    IF  ( iptbef .lt. irpntr )  cont = .false.
C*
	  ELSE IF  ( ( iftype (iptbef) .eq. 2 ) .or.
     +		     ( cfield (iptbef) .eq. 'M' ) )  THEN
C
C*	    Do nothing if this is a number or an "M" indicating the 
C*	    slash was missing.
C
	  ELSE
	    cont = .false.
	END IF
C
C*	The next field is the pressure. If the field is non-numeric,
C*	leave it as missing.  Make sure the size is length 3.
C
	IF  ( ( iftype ( iptbef ) .eq. 2 ) .and.
     +	      ( ifsize ( iptbef ) .eq. 3 ) )  THEN
	    ipres = ifintg (iptbef)
	    iptbef = iptbef - 1
	  ELSE IF  ( cfield ( iptbef ) .eq. 'M' )  THEN
	    iptbef = iptbef - 1
	END IF
C
C*	Look for the altimeter setting after the wind.
C
	ipaaft = ipwaft
C
C*	The altimeter setting is a 3 character numeric value following
C*	a slash.  Look for the slash first.
C
	IF  ( ( ipaaft .lt. nfield ) .and. 
     +	      ( cfield ( ipaaft ) .eq. '/' ) )  THEN
	    ipaaft = ipaaft + 1
C
C*	    If there is an 'E' following the slash (estimated value),
C*	    skip over it.
C
	    IF ( cfield ( ipaaft ) .eq. 'E' ) ipaaft = ipaaft + 1
	    IF  ( ( iftype (ipaaft) .eq. 2 ) .and.
     +		  ( ifsize (ipaaft) .eq. 3 ) )  THEN
		ialti  = ifintg (ipaaft) 
		ipaaft = ipaaft + 1
	    END IF
	END IF
C
C*	Compute altimeter.
C
	IF  ( ialti .ne. IMISSD )  THEN
	    IF  ( ialti .le. 300 )  THEN
		alti = 30. + ( ialti / 100. )
	      ELSE
		alti = 20. + ( ialti / 100. )
	    END IF
	END IF
C
C*	Compute pressure using altimeter in intermediate range.
C
	IF  ( ipres .ne. IMISSD )  THEN
C
C*	    If altimeter is missing, make best guess.
C
	    IF  ( ialti .eq. IMISSD )  THEN
		IF  ( ipres .lt. 500 )  THEN
		    pres = ( 10000. + ipres ) / 10.
		  ELSE
		    pres = (  9000. + ipres ) / 10.
		END IF
	      ELSE
		IF  ( ipres .le. 250 )  THEN
		    pres = ( 10000. + ipres ) / 10.
		  ELSE IF  ( ipres .ge. 750 )  THEN
		    pres = (  9000. + ipres ) / 10.
		  ELSE IF  ( alti .le. 30. )  THEN
		    pres = (  9000. + ipres ) / 10.
		  ELSE
		    pres = ( 10000. + ipres ) / 10.
		END IF
	    END IF
	END IF
C
C*	If temperature is missing, be sure dewpoint is also.
C
	IF  ( tmiss .and. ( .not. dmiss ) )  dwpf = RMISSD
C*
	RETURN
	END
