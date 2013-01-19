	SUBROUTINE SHN_BUFR  ( iret )
C************************************************************************
C* SHN_BUFR								*
C*									*
C* This subroutine retrieves data from the interface arrays, converts	*
C* it into BUFR output, and then writes the BUFR output to the BUFR	*
C* output stream.							*
C*									*
C* SHN_BUFR  ( IRET )							*
C*									*
C* Input parameters:							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C* J. Ator/NCEP		07/06	Set BUFR types/subtypes based upon STYP	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C*
	PARAMETER	( MAXTD = 10 )
C*
	CHARACTER	ctds(MAXTD)*2, ctd*2, cimn*8, bfrstr*4, iqual,
     +			bfstyp*8, timctd*2
C*
	INTEGER		itds(MAXTD)
C*
	LOGICAL		newtd
C*
        REAL*8          r8ary (4), UT_RIBM, PKFTBV
C*
	INCLUDE		'ERMISS.FNC'
C*-----------------------------------------------------------------------
	iret = 0
C
C*	Divide the interface values into groups based upon their
C*	associated time durations.  Then, encode a separate BUFR
C*	report for each such group of values.
C
	ntd = 0
	DO WHILE ( .true. )
C
C*	    Determine the next time duration for which to encode
C*	    a BUFR report.
C
	    irptr = 9 + ( ntd * 3 )
	    newtd = .false.
	    DO WHILE  ( ( .not. newtd ) .and.
     +		       ( ( irptr + 2 ) .le. nimn ) )
		ctd = rimnem ( irptr )(5:6)
		itd = INT ( rivals ( irptr ) )
                timdur = rivals (irptr)
                timctd = ctd
C
C*		Is this a "new" time duration (for which we have not
C*		yet encoded a BUFR report)?
C
		i = 1
		newtd = .true.
		DO WHILE  ( ( i .le. ntd ) .and. ( newtd ) )
		    IF  ( ( ctd .eq. ctds (i) ) .and.
     +			  ( itd .eq. itds (i) ) )  THEN
			newtd = .false.
		        irptr = irptr + 3
		    ELSE
			i = i + 1
		    END IF
		END DO
	    END DO
	    IF  ( .not. newtd )  THEN
C
C*		We are done!
C
		RETURN
	    END IF
C
C*	    Initialize a BUFR report for this time duration.
C
	    year = rivals (1)
	    rmth = rivals (2)
	    days = rivals (3)
	    hour = rivals (4)
	    rmin = rivals (5)
	    IF  ( ( ERMISS ( year ) ) .or. ( ERMISS ( rmth ) ) .or.
     +		  ( ERMISS ( days ) ) .or. ( ERMISS ( hour ) )  )  THEN
		RETURN
	    END IF
C 
	    ibfdt = ( INT ( year ) * 1000000 ) +
     +		    ( INT ( rmth ) * 10000 )  +
     +		    ( INT ( days ) * 100 ) + INT ( hour )
C 
	    IF  ( ( civals (2)(1:4) .eq. 'COOP' ) .or.
     +		  ( civals (2)(1:5) .eq. 'UCOOP' ) )  THEN
		bfstyp = 'NC255102'
	    ELSE IF ( civals (2)(1:5) .eq. 'C-MAN' ) THEN
		bfstyp = 'NC001010'
	    ELSE IF ( civals (2)(1:4) .eq. 'BUOY' ) THEN
		bfstyp = 'NC001011'
	    ELSE IF ( ( civals (2)(1:4) .eq. 'CTGN' ) .or.
     +		      ( civals (2)(1:3) .eq. 'HTG' ) )  THEN
		bfstyp = 'NC001012'
	    ELSE
		bfstyp = 'NC000010'
	    END IF
C 
	    CALL OPENMB  ( iunbfo, bfstyp, ibfdt )
C 
C*	    Report ID.
C
            IF ( (timdur .eq. 24.0) .and. (timctd .eq. 'HR') ) then
	    CALL UT_CIBF  ( iunbfo, 'RPID', civals (1), 8, iercbf )
C
C*	    Report date-time.
C
	    CALL UT_RIBF  ( iunbfo, 'YEAR', year, ierrbf )
	    CALL UT_RIBF  ( iunbfo, 'MNTH', rmth, ierrbf )
	    CALL UT_RIBF  ( iunbfo, 'DAYS', days, ierrbf )
	    CALL UT_RIBF  ( iunbfo, 'HOUR', hour, ierrbf )
	    CALL UT_RIBF  ( iunbfo, 'MINU', rmin, ierrbf )
C
C*	    Time duration.
C
	    IF  ( .not. ERMISS ( rivals ( irptr ) ) )  THEN
		CALL UT_RIBF  ( iunbfo, 'TP' // ctd ,
     +				( rivals ( irptr ) * (-1) ) , ierrbf )
	    END IF
C
C*	    Latitude, longitude and elevation.
C
	    CALL UT_RIBF  ( iunbfo, 'CLAT' , rivals (6), ierrbf )
	    CALL UT_RIBF  ( iunbfo, 'CLON' , rivals (7), ierrbf )
	    CALL UT_RIBF  ( iunbfo, 'HSMSL', rivals (8), ierrbf )
C
C*	    Receipt date-time.
C
	    CALL UT_RIBF ( iunbfo, 'RCYR', FLOAT ( irundt(1) ), ierrbf )
	    CALL UT_RIBF ( iunbfo, 'RCMO', FLOAT ( irundt(2) ), ierrbf )
	    CALL UT_RIBF ( iunbfo, 'RCDY', FLOAT ( irundt(3) ), ierrbf )
	    CALL UT_RIBF ( iunbfo, 'RCHR', FLOAT ( irundt(4) ), ierrbf )
	    CALL UT_RIBF ( iunbfo, 'RCMI', FLOAT ( irundt(5) ), ierrbf )
	    CALL UT_RIBF ( iunbfo, 'RCTS', 0., ierrbf )
C
C*	    Bulletin ID information.
C
	    CALL UT_CIBF  ( iunbfo, 'SEQNUM', seqnum, 8, iercbf )
	    CALL UT_CIBF  ( iunbfo, 'BUHD', buhd, 8, iercbf )
	    CALL UT_CIBF  ( iunbfo, 'BORG', cborg, 8, iercbf )
	    CALL UT_CIBF  ( iunbfo, 'BULTIM', bulldt, 8, iercbf )
	    CALL UT_CIBF  ( iunbfo, 'BBB', bbb, 8, iercbf )
C
C*	    Locate all other interface values which have this same time
C*	    duration and encode them into this BUFR report.
C
	    nst = 0
	    nmxst = 0
	    nmist = 0
	    DO WHILE  ( ( irptr + 2 ) .le. nimn )
		IF  ( ( rimnem ( irptr )(5:6) .eq. ctd ) .and.
     +		      ( INT ( rivals ( irptr ) ) .eq. itd ) )  THEN
C
		    rvalu = rivals ( irptr + 1 )
C
C*		    Encode this interface value, along with its
C*		    associated SHEF revision and qualifier flags,
C*		    into the BUFR report.
C*		    
		    cimn  = rimnem ( irptr + 1 )
		    IF  ( ( cimn(1:2) .eq. 'MX' ) .or.
     +			  ( cimn(1:2) .eq. 'MN' ) )  THEN
			ipt = 3
		    ELSE
			ipt = 1
		    END IF
C
C*		    SHEF revision flag.
C
		    r8ary (3) = UT_RIBM ( rivals ( irptr + 2 ) )
C
C*		    SHEF qualifier flag.
C
		    iqual = civals ( irptr + 1 )(1:1)
		    r8ary (4) =
     +			UT_RIBM ( FLOAT ( MOVA2I ( iqual ) - 64 ) )
C
		    IF  ( cimn(1:4) .eq. 'TPCI' )  THEN
C
C*			Total precipitation.
C
			r8ary (2) = UT_RIBM ( PR_INMM ( rvalu ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'TOPC', ierfsq )
C
		    ELSE IF  ( cimn .eq. 'SHPT    ' )  THEN
C
C*			Type of precipitation.
C
			ishpt = INT ( rvalu )
			IF  ( ( ishpt .ge. 0 ) .and.
     +			      ( ishpt .le. 9 ) ) THEN
			  IF  ( ishpt .eq. 0 )  THEN
			    r8ary (2) = PKFTBV ( 30, 19 )
			  ELSE IF  ( ishpt .eq. 1 )  THEN
			    r8ary (2) = PKFTBV ( 30,  5 )
			  ELSE IF  ( ishpt .eq. 2 )  THEN
			    r8ary (2) = PKFTBV ( 30,  3 ) +
     +					PKFTBV ( 30,  5 )
			  ELSE IF  ( ishpt .eq. 3 )  THEN
			    r8ary (2) = PKFTBV ( 30,  4 )
			  ELSE IF  ( ishpt .eq. 4 )  THEN
			    r8ary (2) = PKFTBV ( 30,  3 ) +
     +					PKFTBV ( 30,  4 )
			  ELSE IF  ( ishpt .eq. 5 )  THEN
			    r8ary (2) = PKFTBV ( 30,  7 )
			  ELSE IF  ( ishpt .eq. 6 )  THEN
			    r8ary (2) = PKFTBV ( 30,  9 )
			  ELSE IF  ( ishpt .eq. 7 )  THEN
			    r8ary (2) = PKFTBV ( 30,  8 )
			  ELSE IF  ( ishpt .eq. 8 )  THEN
			    r8ary (2) = PKFTBV ( 30, 10 )
			  ELSE
			    r8ary (2) = PKFTBV ( 30, 14 )
			  END IF
			  CALL SHN_BFSQ  ( r8ary, 3, 'PRTP', ierfsq )
			END IF
C
		    ELSE IF  ( cimn .eq. 'RSHF    ' )  THEN
C
C*			River stage height.
C
			r8ary (2) = UT_RIBM ( PR_HGFM ( rvalu ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'RSHM', ierfsq )
C
		    ELSE IF  ( ( cimn(1:2) .eq. 'SN' ) .and.
     +			       ( cimn(4:4) .eq. 'W' ) )  THEN
C
C*			Snow depth (fresh or total).
C
			r8ary (2) =
     +			    UT_RIBM ( PR_HGMK ( PR_INMM ( rvalu ) ) )
			IF  ( cimn(3:3) .eq. 'E' )  THEN
			  bfrstr = 'DOFS'
			ELSE
			  bfrstr = 'TOSD'
			END IF
			CALL SHN_BFSQ  ( r8ary, 3, bfrstr, ierfsq )
C
		    ELSE IF  ( cimn .eq. 'WEQS    ' )  THEN
C
C*			Snow water equivalent.
C
			r8ary (2) = UT_RIBM ( PR_INMM ( rvalu ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'SWEM', ierfsq )
C
		    ELSE IF  ( cimn(ipt:ipt+3) .eq. 'TMPF' )  THEN
C
C*			Temperature.
C
			r8ary (2) = UT_RIBM ( PR_TMFK ( rvalu ) )
			IF  ( cimn(2:2) .eq. 'X' )  THEN
			  bfrstr = 'MXTM'
			ELSE IF  ( cimn(2:2) .eq. 'N' )  THEN
			  bfrstr = 'MITM'
			ELSE
			  bfrstr = 'TMDB'
			END IF
			CALL SHN_BFSQ  ( r8ary, 3, bfrstr, ierfsq )
C
		    ELSE IF  ( cimn .eq. 'TMWF    ' )  THEN
C
C*			Wet bulb temperature.
C
			r8ary (2) = UT_RIBM ( PR_TMFK ( rvalu ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'TMWB', ierfsq )
C
		    ELSE IF  ( cimn .eq. 'DWPF    ' )  THEN
C
C*			Dew point temperature.
C
			r8ary (2) = UT_RIBM ( PR_TMFK ( rvalu ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'TMDP', ierfsq )
C
		    ELSE IF  ( cimn .eq. 'WTMF    ' )  THEN
C
C*			Water temperature.
C
			r8ary (2) = UT_RIBM ( PR_TMFK ( rvalu ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'WATM', ierfsq )
C
		    ELSE IF  ( ( cimn(ipt:ipt+3) .eq. 'CSTD' ) .or.
     +			       ( cimn(ipt:ipt+3) .eq. 'STMF' ) )  THEN
C
C*			Soil temperature (with or without depth).
C
			IF  ( cimn (ipt:ipt) .eq. 'C' )  THEN
			  icstdn = NINT ( rvalu * 1000. )
			  dblsi = FLOAT ( icstdn / 1000 )
			  stmf = ( FLOAT ( MOD ( icstdn, 1000 ) ) )
			ELSE
			  dblsi = RMISSD
			  stmf = rvalu
			END IF
C
			r8ary (1) =
     +			    UT_RIBM ( PR_HGMK ( PR_INMM ( dblsi ) ) )
			r8ary (2) = UT_RIBM ( PR_TMFK ( stmf ) )
C
			IF  ( cimn(2:2) .eq. 'X' )  THEN
			  nmxst = nmxst + 1
			  IF  ( nmxst .eq. 1 )  THEN
			    bfrstr = 'XST1'
			  ELSE
			    bfrstr = 'XST2'
			  END IF
			ELSE IF  ( cimn(2:2) .eq. 'N' )  THEN
			  nmist = nmist + 1
			  IF  ( nmist .eq. 1 )  THEN
			    bfrstr = 'IST1'
			  ELSE
			    bfrstr = 'IST2'
			  END IF
			ELSE
			  nst = nst + 1
			  IF  ( nst .eq. 1 )  THEN
			    bfrstr = 'STM1'
			  ELSE
			    bfrstr = 'STM2'
			  END IF
			END IF
C
			CALL SHN_BFSQ  ( r8ary, 4, bfrstr, ierfsq )
C
		    ELSE IF  ( cimn(ipt:ipt+3) .eq. 'RELH' )  THEN
C
C*			Relative humidity.
C
			r8ary (2) = UT_RIBM ( rvalu )
			IF  ( cimn(2:2) .eq. 'X' )  THEN
			  bfrstr = 'MXRH'
			ELSE IF  ( cimn(2:2) .eq. 'N' )  THEN
			  bfrstr = 'MIRH'
			ELSE
			  bfrstr = 'REHU'
			END IF
			CALL SHN_BFSQ  ( r8ary, 3, bfrstr, ierfsq )
C
		    ELSE IF  ( ( cimn .eq. 'DRCT    ' ) .or.
     +			       ( cimn .eq. 'PWDR    ' ) )  THEN
C
C*			Wind direction (normal or peak).
C
			r8ary (2) = UT_RIBM ( rvalu )
			IF  ( cimn(1:1) .eq. 'D' )  THEN
			  bfrstr = 'WDIR'
			ELSE
			  bfrstr = 'PKWD'
			END IF
			CALL SHN_BFSQ  ( r8ary, 3, bfrstr, ierfsq )
C
		    ELSE IF  ( cimn(2:4) .eq. 'MPH' )  THEN
C
C*			Wind speed (normal, peak or gusts).
C
			r8ary (2) =
     +			    UT_RIBM ( PR_KNMS ( PR_MHKN ( rvalu ) ) )
			IF  ( cimn(1:1) .eq. 'S' )  THEN
			  bfrstr = 'WSPD'
			ELSE IF  ( cimn(1:1) .eq. 'P' )  THEN
			  bfrstr = 'PKWS'
			ELSE
			  bfrstr = 'MXGS'
			END IF
			CALL SHN_BFSQ  ( r8ary, 3, bfrstr, ierfsq )
C
		    ELSE IF  ( cimn .eq. 'CWDS    ' )  THEN
C
C*			Combined wind direction and speed.
C
			icwdsn = NINT ( rvalu * 10000. )
C
			r8ary (2) =
     +			    UT_RIBM ( FLOAT ( MOD ( icwdsn, 1000 ) ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'WDIR', ierfsq )
C
			smph = FLOAT ( icwdsn / 1000 ) / 10.
			r8ary (2) =
     +			    UT_RIBM ( PR_KNMS ( PR_MHKN ( smph ) ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'WSPD', ierfsq )
C
		    ELSE IF  ( ( cimn(1:1) .eq. 'P' ) .and.
     +			       ( cimn(3:4) .eq. 'SI' ) )  THEN
C
C*			Pressure (normal or reduced to MSL).
C
			IF  ( rvalu .lt. 900. )  THEN
C
C*			  The value is in inches, so convert to mb.
C
			  prmb = PR_ALTM ( rvalu )
			ELSE
			  prmb = rvalu
			END IF
C
			r8ary (2) = UT_RIBM ( PR_M100 ( prmb ) )
			IF  ( cimn(2:2) .eq. 'R' )  THEN
			  bfrstr = 'PRES'
			ELSE
			  bfrstr = 'PMSL'
			END IF
			CALL SHN_BFSQ  ( r8ary, 3, bfrstr, ierfsq )
C
		    ELSE IF  ( INDEX ( cimn, 'WWM' ) .ne. 0 )  THEN
C
C*			Past or present weather.
C
			r8ary (2) = UT_RIBM ( rvalu )
			IF  ( cimn(1:1) .eq. 'P' )  THEN
			  bfrstr = 'PSW1'
			ELSE
			  bfrstr = 'PRWE'
			END IF
			CALL SHN_BFSQ  ( r8ary, 3, bfrstr, ierfsq )
C
		    ELSE IF  ( cimn .eq. 'VSBY    ' )  THEN
C
C*			Horizontal visibility.
C
			r8ary (2) =
     +			    UT_RIBM ( PR_HGFM ( PR_HGSF ( rvalu ) ) )
			CALL SHN_BFSQ  ( r8ary, 3, 'HOVI', ierfsq )
C
		    ELSE IF  ( ( cimn .eq. 'SOGR    ' ) .or.
C
C*				State of the ground.
C
     +			       ( cimn .eq. 'CLAM    ' ) .or.
C
C*				Cloud amount.
C
     +			       ( cimn .eq. 'ALBD    ' ) .or.
C
C*				Albedo.
C
     +			       ( cimn .eq. 'TOSH    ' ) )  THEN
C
C*				Total sunshine.
C
			r8ary (2) = UT_RIBM ( rvalu )
			CALL SHN_BFSQ  ( r8ary, 3, cimn(1:4), ierfsq )
C
		    ELSE
C
			logmsg = '  unable to create BUFR output for '
     +				// cimn
			CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg)
C
		    END IF
C
		END IF
C
		irptr = irptr + 3
C
	    END DO
C
C*	    Store the BUFR report for this time duration.
C
	    CALL UT_WBFR  ( iunbfo, 'shef', .false., ierwbf )

C
C* TIME DURATION END IF
C
            END IF   
C
C*	    Update the ctds and itds arrays, then go back and determine
C*	    the next time duration for which to encode a BUFR report.
C
	    ntd = ntd + 1
	    IF  ( ntd .gt. MAXTD )  THEN
		logmsg = '  MAXTD overflow when generating BUFR output.'
		CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg)
		RETURN
	    END IF
	    ctds ( ntd ) = ctd
	    itds ( ntd ) = itd
	END DO
C*
	RETURN
	END
