	SUBROUTINE SHN_TDIM  ( idur, cxtcod, celcod, cimn, td, ctdu,
     +			      iret )
C************************************************************************
C* SHN_TDIM								*
C*									*
C* This subroutine translates the SHEF duration, extremum and element	*
C* codes associated with a particular observation into an actual time	*
C* duration and interface mnemonic.					*
C*									*
C* SHN_TDIM ( IDUR, CXTCOD, CELCOD, CIMN, TD, CTDU, IRET )		*
C*									*
C* Input parameters:							*
C*	IDUR		INTEGER		SHEF duration code		*
C*	CXTCOD		CHAR*		SHEF extremum code		*
C*	CELCOD		CHAR*		SHEF element code		*
C*									*
C* Output parameters:							*
C*	CIMN		CHAR*		Interface mnemonic		*
C*	TD		REAL		Time duration applying to CIMN	*
C*	CTDU		CHAR*2		Units of TD:			*
C*					 'MO' = months			*
C*					 'HR' = hours			*
C*					 'MI' = minutes			*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C*					 -1 = could not determine CIMN	*
C*					      and/or TD			*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C*
	CHARACTER*(*)	cxtcod, celcod, cimn, ctdu
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = -1 
C
	cimn = ' '
	td = RMISSD
	ctdu = 'HR'
C
	IF ( cxtcod .eq. 'Z' ) THEN
C
C*	    Check for a duration within the SHEF duration code.
C
	    IF ( idur .lt. 2000 ) THEN
		td = FLOAT ( MOD ( idur, 1000 ) )
		IF ( idur .lt. 1000 ) THEN
		    ctdu = 'MI'
		END IF
	    ELSE IF ( idur .lt. 3000 ) THEN
		td = FLOAT ( MOD ( idur, 2000 ) ) * 24.
	    ELSE IF ( idur .lt. 4000 ) THEN
		td = FLOAT ( MOD ( idur, 3000 ) )
		ctdu = 'MO'
	    ELSE IF ( idur .eq. 5004 ) THEN
		CALL SHN_5004 ( nmin, ier504 )
		IF ( ier504 .ne. 0 ) THEN
		    RETURN
		END IF
		td = FLOAT ( nmin )
		ctdu = 'MI'
	    END IF
	ELSE
C
C*	    Check for a duration within the SHEF extremum code.
C
	    IF ( cxtcod .eq. 'M' ) THEN
		td = 168.
		cimn(1:2) = 'MN'
	    ELSE IF ( cxtcod .eq. 'N' ) THEN
		td = 24.
		cimn(1:2) = 'MN'
	    ELSE IF ( cxtcod .eq. 'F' ) THEN
		td = 1.
		cimn(1:2) = 'MN'
	    ELSE IF ( cxtcod .eq. 'G' ) THEN
		td = 3.
		cimn(1:2) = 'MN'
	    ELSE IF ( cxtcod .eq. 'H' ) THEN
		td = 6.
		cimn(1:2) = 'MN'
	    ELSE IF ( cxtcod .eq. 'P' ) THEN
		td = 12.
		cimn(1:2) = 'MN'
	    ELSE IF ( cxtcod .eq. 'I' ) THEN
		td = 18.
		cimn(1:2) = 'MN'
	    ELSE IF ( cxtcod .eq. 'L' ) THEN
		td = 1.
		ctdu = 'MO'
		cimn(1:2) = 'MN'
	    ELSE IF ( cxtcod .eq. 'W' ) THEN
		td = 168.
		cimn(1:2) = 'MX'
	    ELSE IF ( cxtcod .eq. 'X' ) THEN
		td = 24.
		cimn(1:2) = 'MX'
	    ELSE IF ( cxtcod .eq. 'D' ) THEN
		td = 1.
		cimn(1:2) = 'MX'
	    ELSE IF ( cxtcod .eq. 'E' ) THEN
		td = 3.
		cimn(1:2) = 'MX'
	    ELSE IF ( cxtcod .eq. 'R' ) THEN
		td = 6.
		cimn(1:2) = 'MX'
	    ELSE IF ( cxtcod .eq. 'Y' ) THEN
		td = 12.
		cimn(1:2) = 'MX'
	    ELSE IF ( cxtcod .eq. 'S' ) THEN
		td = 18.
		cimn(1:2) = 'MX'
	    ELSE IF ( cxtcod .eq. 'V' ) THEN
		td = 1.
		ctdu = 'MO'
		cimn(1:2) = 'MX'
	    END IF
C
	END IF
C
	IF ( ERMISS ( td ) ) THEN
	    WRITE ( UNIT = logmsg, FMT = '( 3A, I4 )' )
     +		'  unable to determine duration from cxtcod ',
     +		cxtcod, ' and idur ', idur
	    CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Determine the interface mnemonic from the element code.
C
	IF ( cimn(1:1) .eq. 'M' ) THEN
	    ipt = 3
	ELSE
	    ipt = 1
	END IF
	IF ( celcod .eq. 'PP' ) THEN
C
C*	    Total precipitation (in inches).
C
	    cimn(ipt:ipt+3) = 'TPCI'
C
	ELSE IF ( celcod .eq. 'PC' ) THEN
C
C*	    Total precipitation (in inches) that has accumulated in
C*	    the gauge (i.e. current gauge reading).  Since such data
C*	    values are unaccompanied by a true duration, it will be
C*	    up to the end-user to locate the previous corresponding
C*	    report in the database and then subtract in order to
C*	    determine the incremental precipitation amount and
C*	    corresponding duration.
C
	    cimn(ipt:ipt+4) = 'TPCIG'
	    td = RMISSD
C
	ELSE IF ( celcod .eq. 'PT' ) THEN
C
C*	    Type of precipitation (SHEF Code Table 17).
C
	    cimn(ipt:ipt+3) = 'SHPT'
C
	ELSE IF ( celcod .eq. 'HG' ) THEN
C
C*	    River stage height (in feet).
C
	    cimn(ipt:ipt+3) = 'RSHF'
C
	ELSE IF ( celcod .eq. 'GS' ) THEN
C
C*	    State of the ground (SHEF Code Table 18).
C
	    cimn(ipt:ipt+3) = 'SOGR'
C
	ELSE IF ( celcod .eq. 'SD' ) THEN
C
C*	    Total snow depth (in inches).
C
	    cimn(ipt:ipt+3) = 'SNOW'
C
	ELSE IF ( celcod .eq. 'SF' ) THEN
C
C*	    Depth of fresh snow (in inches).
C
	    cimn(ipt:ipt+3) = 'SNEW'
C
	ELSE IF ( celcod .eq. 'SW' ) THEN
C
C*	    Water equivalent of snow (in inches).
C
	    cimn(ipt:ipt+3) = 'WEQS'
C
	ELSE IF ( celcod .eq. 'TA' ) THEN
C
C*	    Dry bulb temperature (in Fahrenheit).
C
	    cimn(ipt:ipt+3) = 'TMPF'
C
	ELSE IF ( celcod .eq. 'TM' ) THEN
C
C*	    Wet bulb temperature (in Fahrenheit).
C
	    cimn(ipt:ipt+3) = 'TMWF'
C
	ELSE IF ( celcod .eq. 'TD' ) THEN
C
C*	    Dew point temperature (in Fahrenheit).
C
	    cimn(ipt:ipt+3) = 'DWPF'
C
	ELSE IF ( celcod .eq. 'TW' ) THEN
C
C*	    Water temperature (in Fahrenheit).
C
	    cimn(ipt:ipt+3) = 'WTMF'
C
	ELSE IF ( celcod .eq. 'TS' ) THEN
C
C*	    Soil temperature (in Fahrenheit).
C
	    cimn(ipt:ipt+3) = 'STMF'
C
	ELSE IF ( ( celcod .eq. 'TB' ) .or.
     +		  ( celcod .eq. 'TV' ) ) THEN
C
C*	    Combined soil temperature and depth (in format DDD.TTT,
C*	    where TTT is soil temperature in degrees Fahrenheit at
C*	    depth DDD inches below the surface).
C
	    cimn(ipt:ipt+3) = 'CSTD'
C
	ELSE IF ( celcod .eq. 'XR' ) THEN
C
C*	    Relative humidity (in percent).
C
	    cimn(ipt:ipt+3) = 'RELH'
C
	ELSE IF ( celcod .eq. 'UD' ) THEN
C
C*	    Wind direction (in degrees).
C
	    cimn(ipt:ipt+3) = 'DRCT'
C
	ELSE IF ( celcod .eq. 'US' ) THEN
C
C*	    Wind speed (in miles per hour).
C
	    cimn(ipt:ipt+3) = 'SMPH'
C
	ELSE IF ( celcod .eq. 'UG' ) THEN
C
C*	    Wind gust speed at time of observation (in miles per hour).
C
	    cimn(ipt:ipt+3) = 'GMPH'
C
	ELSE IF ( celcod .eq. 'UR' ) THEN
C
C*	    Peak wind direction (in degrees).  Note that such data
C*	    values are supposed to be reported in tens of degrees
C*	    according to the documentation; however, in reality,
C*	    they are almost always reported in whole degrees!(?)
C
	    cimn(ipt:ipt+3) = 'PWDR'
C
	ELSE IF ( celcod .eq. 'UP' ) THEN
C
C*	    Peak wind speed (in miles per hour).
C
	    cimn(ipt:ipt+3) = 'PMPH'
C
	ELSE IF ( celcod .eq. 'UQ' ) THEN
C
C*	    Combined wind direction and speed (in format SSS.SDDD,
C*	    where SSS.S is speed in miles per hour and DDD is direction
C*	    in degrees).
C
	    cimn(ipt:ipt+3) = 'CWDS'
C
	ELSE IF ( celcod .eq. 'PA' ) THEN
C
C*	    Pressure.  These data values are supposed to be reported
C*	    in inches (of mercury); however, in reality, they are often
C*	    reported in millibars.
C
	    cimn(ipt:ipt+3) = 'PRSI'
C
	ELSE IF ( celcod .eq. 'PL' ) THEN
C
C*	    MSL pressure.  These data values are supposed to be reported
C*	    in inches (of mercury); however, in reality, they are often
C*	    reported in millibars.
C
	    cimn(ipt:ipt+3) = 'PMSI'
C
	ELSE IF ( celcod .eq. 'XW' ) THEN
C
C*	    Present weather.
C
	    cimn(ipt:ipt+3) = 'WWMO'
C
	ELSE IF ( celcod .eq. 'XP' ) THEN
C
C*	    Past weather.
C
	    cimn(ipt:ipt+3) = 'PWWM'
C
	ELSE IF ( celcod .eq. 'XC' ) THEN
C
C*	    Total sky cover amount (in tenths).
C
	    cimn(ipt:ipt+3) = 'CLAM'
C
	ELSE IF ( celcod .eq. 'XV' ) THEN
C
C*	    Horizontal visiblity (in miles).
C
	    cimn(ipt:ipt+3) = 'VSBY'
C
	ELSE IF ( celcod .eq. 'RA' ) THEN
C
C*	    Albedo.
C
	    cimn(ipt:ipt+3) = 'ALBD'
C
	ELSE IF ( celcod .eq. 'RT' ) THEN
C
C*	    Total sunshine (in hours).
C
	    cimn(ipt:ipt+3) = 'TOSH'
C
	ELSE
	    logmsg = '  unknown element code ' // celcod
	    CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
	iret = 0
C*
	RETURN
	END
