	SUBROUTINE SHN_GEMP ( iret ) 
C************************************************************************
C* SHN_GEMP								*
C*									*
C* This subroutine retrieves data from the interface arrays, converts	*
C* it into GEMPAK output as a surface file                      	*
C*									*
C* SHN_GEMP  (  iret )	                                                *
C*									*
C* Input parameters: NONE						*
C                                                                       *
C*      LUNF            INTEGER         Surface file number             *
C*      CPRMS (*)       CHAR*           GEMPAK parms chosen for output  *
C*      PARMS (*)       CHAR*           Parameter list                  *
C*      NPARM           INTEGER         Number of parameters            *
C*      NUMPRM          INTEGER         Count of chosen GEMPAK parms    *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**									*
C* Log:									*
C* V. K. KUMAR/NCEP	12/06           Based on SHN_BUFR		*
C*                                      Units are in the comments       *
C*                                      GEMPAK parameters have units    * 
C*                                      different from BUFR parameters  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C*
	PARAMETER	( MAXTD = 10 )
C*
	CHARACTER	ctds(MAXTD)*2, ctd*2, cimn*8, iqual, timctd*2
C*
	CHARACTER	dattim*12, cyear*4, cmth*2, cdays*2
	CHARACTER	chour*2, crmin*2, slash*1, stid*8
C*
C*      Maintain the order of the following parameter list.  Add new
C*      parameters at the end.
C
	REAL		rdata (MMPARM), slat, slon, selv
C*
	INTEGER		itds(MAXTD)
C*
	LOGICAL		newtd, cirflg, datflg, addstn
C*
        REAL*8		r8ary (4)
C*
	INCLUDE		'ERMISS.FNC'
C*-----------------------------------------------------------------------
	iret = 0
	cirflg = .false.
	addstn = .true.
C
C*      Initialize the GEMPAK array first.
C
        DO ii = 1, MMPARM
            rdata ( ii ) = RMISSD
        END DO
C
C*	Divide the interface values into groups based upon their
C*	associated time durations.  Then, encode a separate GEMPAK 
C*	report for each such group of values.
C
	ntd = 0
	DO WHILE ( .true. )
C
C*	    Determine the next time duration for which to encode
C*	    a GEMPAK report.
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
C*		yet encoded a GEMPAK report)?
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
C*	    Initialize a GEMPAK report for this time duration.
C           Use the two digit year for GEMPAK needed

	    year = rivals (1)
	    rmth = rivals (2)
	    days = rivals (3)
	    hour = rivals (4)
	    rmin = rivals (5)

	    IF  ( ( ERMISS ( year ) ) .or. ( ERMISS ( rmth ) ) .or.
     +		  ( ERMISS ( days ) ) .or. ( ERMISS ( hour ) )  )  THEN
		RETURN
	    END IF
****
C* make a string of "dattime" for GEMPAK YYMMDD/HHMM for this observation
C* from the above
****
            WRITE ( cyear, fmt = '( i4 )') int ( year ) 
            WRITE ( cmth, fmt = '( i2 )')  int ( rmth )
            WRITE ( cdays, fmt = '( i2 )') int ( days ) 
            WRITE ( chour, fmt = '( i2 )') int ( hour )
            WRITE ( crmin, fmt = '( i2 )') int ( rmin )

            IF ( cyear(1:1) .eq. ' ') cyear(1:1) = '0'
            IF ( cmth(1:1) .eq. ' ') cmth(1:1) = '0'
            IF ( cdays(1:1) .eq. ' ') cdays(1:1) = '0'
            IF ( chour(1:1) .eq. ' ') chour(1:1) = '0'
            IF ( crmin(1:1) .eq. ' ') crmin(1:1) = '0'
            slash = '/'
            dattim = cyear(3:4)//cmth//cdays//slash//chour//crmin
C
C*                  Set the station and time in the output file.
C
            stid = civals(1)
C
C*      Set the station time (HHMM) for the SF_WDAT
C
            ihhmm = int( hour ) * 100 + int ( rmin )
            CALL RA_TMST ( iungem, dattim, stid, addstn,
     +                     cirflg, datflg, iret )
            slat = rivals (6)
            slon = rivals (7)
            selv = rivals (8) 
            
C
C*	    Report date-time.
C
C
C*	    Time duration. (include in gempak)
C
            IF ( (timdur .eq. 24.0) .and. (timctd .eq. 'HR') ) then
C
C*	    Locate all other interface values which have this same time
C*	    duration and encode them into this GEMPAK report.
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
C*		    into the GEMPAK report.
C*		    
                    cprms ( 26 ) = 'HR24'
                    rdata ( 26 ) = timdur
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
C*			Total precipitation in inches.
C
                        cprms (1) = 'PR24'
			r8ary (2) = UT_RIBM ( PR_INMM ( rvalu ) )
                        rdata (1) = rvalu 
C
		    ELSE IF  ( cimn .eq. 'SHPT    ' )  THEN
C
C*			Type of precipitation.
C
                        cprms (2) = 'SHPT'
                        rdata (2) = rvalu
C
		    ELSE IF  ( cimn .eq. 'RSHF    ' )  THEN
C
C*			River stage height in feet.
C
			r8ary (2) = UT_RIBM ( PR_HGFM ( rvalu ) )
                        cprms (3) = 'SHHG'
                        rdata (3) = rvalu
C
		    ELSE IF  ( ( cimn(1:2) .eq. 'SN' ) .and.
     +			       ( cimn(4:4) .eq. 'W' ) )  THEN
C
C*			Snow depth (fresh or total) in inches.
C
			r8ary (2) =
     +			    UT_RIBM ( PR_HGMK ( PR_INMM ( rvalu ) ) )
			IF  ( cimn(3:3) .eq. 'E' )  THEN
c			  bfrstr = 'DOFS'
                          cprms (4) = 'SF24'
                          rdata (4) = rvalu
			ELSE
c			  bfrstr = 'TOSD'
                          cprms (5) = 'SNOW'
                          rdata (5) = rvalu
			END IF
C
		    ELSE IF  ( cimn .eq. 'WEQS    ' )  THEN
C
C*			Snow water equivalent in inches.
C
			r8ary (2) = UT_RIBM ( PR_INMM ( rvalu ) )
                        cprms (6) = 'WEQS'
                        rdata (6) = rvalu
C
		    ELSE IF  ( cimn(ipt:ipt+3) .eq. 'TMPF' )  THEN
C
C*			Temperature in Fahrenheit.
C
			r8ary (2) = UT_RIBM ( PR_TMFK ( rvalu ) )
			IF  ( cimn(2:2) .eq. 'X' )  THEN
c			  bfrstr = 'MXTM'
                          cprms (8) = 'TDXF'
                          rdata (8) = rvalu
			ELSE IF  ( cimn(2:2) .eq. 'N' )  THEN
c			  bfrstr = 'MITM'
                          cprms (9) = 'TDNF'
                          rdata (9) = rvalu
			ELSE
c			  bfrstr = 'TMDB'
                          cprms (7) = 'TMPF'
                          rdata (7) = rvalu
			END IF
C
		    ELSE IF  ( cimn .eq. 'TMWF    ' )  THEN
C
C*			Wet bulb temperature in Fahrenheit.
C
			r8ary (2) = UT_RIBM ( PR_TMFK ( rvalu ) )
                        cprms (10) = 'TMWF'
                        rdata (10) = rvalu
C
		    ELSE IF  ( cimn .eq. 'DWPF    ' )  THEN
C
C*			Dew point temperature in Fahrenheit.
C
			r8ary (2) = UT_RIBM ( PR_TMFK ( rvalu ) )
                        cprms (11) = 'DWPF'
                        rdata (11) = rvalu
C
		    ELSE IF  ( cimn .eq. 'WTMF    ' )  THEN
C
C*			Water temperature in Fahrenheit.
C
			r8ary (2) = UT_RIBM ( PR_TMFK ( rvalu ) )
                        cprms (12) = 'TWAF'
                        rdata (12) = rvalu
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
c			    bfrstr = 'XST1'
			  ELSE
c			    bfrstr = 'XST2'
			  END IF
			ELSE IF  ( cimn(2:2) .eq. 'N' )  THEN
			  nmist = nmist + 1
			  IF  ( nmist .eq. 1 )  THEN
c			    bfrstr = 'IST1'
			  ELSE
c			    bfrstr = 'IST2'
			  END IF
			ELSE
			  nst = nst + 1
			  IF  ( nst .eq. 1 )  THEN
c			    bfrstr = 'STM1'
			  ELSE
c			    bfrstr = 'STM2'
			  END IF
			END IF
C
		    ELSE IF  ( cimn(ipt:ipt+3) .eq. 'RELH' )  THEN
C
C*			Relative humidity in %.
C
			r8ary (2) = UT_RIBM ( rvalu )
			IF  ( cimn(2:2) .eq. 'X' )  THEN
c			  bfrstr = 'MXRH'
			ELSE IF  ( cimn(2:2) .eq. 'N' )  THEN
c              		  bfrstr = 'MIRH'
			ELSE
c			  bfrstr = 'REHU'
                          cprms ( 13 ) = 'RELH'
                          rdata ( 13 ) = rvalu
			END IF
C
		    ELSE IF  ( ( cimn .eq. 'DRCT    ' ) .or.
     +			       ( cimn .eq. 'PWDR    ' ) )  THEN
C
C*			Wind direction (normal or peak).
C
			r8ary (2) = UT_RIBM ( rvalu )
			IF  ( cimn(1:1) .eq. 'D' )  THEN
c			  bfrstr = 'WDIR'
                          cprms( 14 ) = 'DRCT'
                          rdata ( 14 ) = rvalu
			ELSE
c			  bfrstr = 'PKWD'
                          cprms ( 15 ) = 'PKWD'
                          rdata ( 15 ) = rvalu
			END IF
C
		    ELSE IF  ( cimn(2:4) .eq. 'MPH' )  THEN
C
C*			Wind speed (normal, peak or gusts) in miles/hour.
C
			r8ary (2) =
     +			    UT_RIBM ( PR_KNMS ( PR_MHKN ( rvalu ) ) )
			IF  ( cimn(1:1) .eq. 'S' )  THEN
c			  bfrstr = 'WSPD'
                          cprms ( 16 ) = 'SMPH'
                          rdata ( 16 ) = rvalu
			ELSE IF  ( cimn(1:1) .eq. 'P' )  THEN
c			  bfrstr = 'PKWS'
                          cprms ( 17 ) = 'GMPH'
                          rdata ( 17 ) = rvalu
			ELSE
c			  bfrstr = 'MXGS'
                          cprms ( 18 ) = 'WSGU'
                          rdata ( 18 ) = rvalu
			END IF
C
		    ELSE IF  ( cimn .eq. 'CWDS    ' )  THEN
C
C*			Combined wind direction and speed in deg and miles/hour.
C
			icwdsn = NINT ( rvalu * 10000. )
C
			r8ary (2) =
     +			    UT_RIBM ( FLOAT ( MOD ( icwdsn, 1000 ) ) )
                        cprms ( 19 ) = 'WDIR'
                        rdata ( 19 ) = r8ary (2)
C
			smph = FLOAT ( icwdsn / 1000 ) / 10.
			r8ary (2) =
     +			    UT_RIBM ( PR_KNMS ( PR_MHKN ( smph ) ) )
                        cprms ( 20 ) = 'WSPD' 
                        rdata ( 20 ) = smph
C
		    ELSE IF  ( ( cimn(1:1) .eq. 'P' ) .and.
     +			       ( cimn(3:4) .eq. 'SI' ) )  THEN
C
C*			Pressure (normal or reduced to MSL) in millibars.
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
c			  bfrstr = 'PRES'
                          cprms ( 21 ) = 'PRES'
                          rdata ( 21 ) = r8ary (2)
			ELSE
c			  bfrstr = 'PMSL'
                          cprms ( 22 ) = 'PMSL'
                          rdata ( 22 ) = r8ary (2)
			END IF
C
		    ELSE IF  ( INDEX ( cimn, 'WWM' ) .ne. 0 )  THEN
C
C*			Past or present weather.
C
			r8ary (2) = UT_RIBM ( rvalu )
			IF  ( cimn(1:1) .eq. 'P' )  THEN
c			  bfrstr = 'PSW1'
                          cprms ( 23 ) = 'PWWM'
                          rdata ( 23 ) = r8ary (2)
			ELSE
c			  bfrstr = 'PRWE'
                          cprms ( 24 ) = 'WWMO'
                          rdata ( 24 ) = r8ary (2)
			END IF
C
		    ELSE IF  ( cimn .eq. 'VSBY    ' )  THEN
C
C*			Horizontal visibility (Height in feet).
C
			r8ary (2) =
     +			    UT_RIBM ( PR_HGFM ( PR_HGSF ( rvalu ) ) )
                        cprms ( 25 ) = 'VSBY'
                        rdata ( 25 ) = r8ary (2)
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
C*      Write the data to a surface data file.
C
            CALL SF_WDAT ( iungem, ihhmm, rdata, iret )
            IF ( iret .ne. 0 ) THEN
                CALL DC_WLOG ( 2, 'SF', iret, ' ', ierwlg )
            END IF
C
C* TIME DURATION END IF
C
            END IF
C*
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
