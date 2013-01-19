	SUBROUTINE RA_DECD  ( irpntr, coun, asoflg, maxcld, ighr,
     +			      cldtyp, cldhgt, ncld, vsby, wcod, wnum,
     +			      pres, tmpf, dwpf, sknt, drct, gust,
     +			      alti, p03d, p03i, p06i, hsun, csyl,
     +			      csym, csyh, snow, weqs, t06x, t12x,
     +			      t24x, tmdx, t06n, t12n, t24n, tmdn,
     +			      p24i, iret )
C************************************************************************
C* RA_DECD								*
C*									*
C* This subroutine decodes a surface airways report.  RA_GFLD must	*
C* be called before this subroutine is called.  IRPNTR must point	*
C* to the first field after the header.					*
C*									*
C* RA_DECD  ( IRPNTR, COUN, ASOFLG, MAXCLD, IGHR, CLDTYP, CLDHGT, NCLD, *
C*	      VSBY, WCOD, WNUM, PRES, TMPF, DWPF, SKNT, DRCT, GUST, 	*
C*            ALTI, P03D, P03I, P06I, HSUN, CSYL, CSYM, CSYH, SNOW, 	*
C*	      WEQS, T06X, T12X, T24X, TMDX, T06N, T12N, T24N, TMDN, 	*
C*	      P24I, IRET )						*
C*									*
C* Input parameters:							*
C*	IRPNTR		INTEGER		First field after header	*
C*	COUN		CHAR*		Country name			*
C*	ASOFLG		LOGICAL		ASOS station flag		*
C*	MAXCLD		INTEGER		Maximum number of clouds	*
C*	IGHR		INTEGER		Integer GEMPAK report hour	*
C*									*
C* Output parameters:					                *
C*	CLDTYP (NCLD)	REAL		GEMPAK cloud numeric types	*
C*	CLDHGT (NCLD)	REAL		Cloud heights			*
C*	NCLD		INTEGER		Number of cloud reports		*
C*	VSBY		REAL		Visibility in miles		*
C*	WCOD		CHAR*		GEMPAK weather code		*
C*	WNUM		REAL		GEMPAK weather number		*
C*	PRES		REAL		Pressure in millibars		*
C*	TMPF		REAL		Temperature in F		*
C*	DWPF		REAL		Dewpoint temp in F		*
C*	SKNT		REAL		Wind speed in knots		*
C*	DRCT		REAL		Wind direction in degrees	*
C*	GUST		REAL		Wind gusts in knots		*
C*	ALTI		REAL		Altimeter in inches		*
C*	P03D		REAL		Pres. tend (sym+amnt) tenths mb	*
C*	P03I		REAL		3-hr precip in inches		*
C*	P06I		REAL		6-hr precip in inches		*
C*	HSUN		REAL		Minutes of sunshine		*
C*	CSYL		REAL		Low level cloud symbol		*
C*	CSYM		REAL		Mid level cloud symbol		*
C*	CSYH		REAL		High level cloud symbol		*
C*	SNOW		REAL		Snow accumulation in inches	*
C*	WEQS		REAL		Water equiv of snow depth inches*
C*	T06X		REAL		6 Hr Max temperature in F 	*
C*	T12X		REAL		12 Hr Max temperature in F	*
C*	T24X		REAL		24 Hr Max temperature in F	*
C*	TMDX		REAL		Prev day max temperature in F	*
C*	T06N		REAL		6 Hr Min temperature in F 	*
C*	T12N		REAL		12 Hr Min temperature in F	*
C*	T24N		REAL		24 Hr Min temperature in F	*
C*	TMDN		REAL		Prev day min temperature in F	*
C*	P24I		REAL		24-hr precip in in		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = report not decoded	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/89	GEMPAK 5				*
C* J. Walker/FSU	11/90	Added ceiling codes			*
C* J. Walker/FSU	12/90	Added additive data			*
C* J. Whistler/SSAI	 2/91	Cleaned up FSU changes			*
C* P. Bruehl/Unidata	 4/93	Added ptsy pres. tendency symbol	*
C* P. Bruehl/Unidata		Fixed pt03 to be real p03d(sym+amnt)	*
C* P. Bruehl/Unidata	2/94	Added synflg for 0,6,12,18Z report times*
C* P. Bruehl/Unidata	2/94	Added cloud symbols			*
C* P. Bruehl/Unidata   10/94    Fixed reset of snow & cl.sym. to -9999	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
        INTEGER         remain
	REAL		cldhgt (*), cldtyp (*) 
	CHARACTER*(*)	wcod, coun
        LOGICAL         sunflg, synflg, asoflg
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	synflg = .false.
C
C*	Get the winds.
C
	CALL RA_WIND ( irpntr, sknt, drct, gust, ipwbef, ipwaft, iret )
C
C*	Decode pressure, temperature, dewpoint and altimeter.
C
	CALL RA_PTDA  ( irpntr, ipwbef, ipwaft, iptbef, ipaaft, pres, 
     +			tmpf, dwpf, alti, ier )
C
C*	Convert Canadian station temperatures from C to F.
C
	IF  ( ( coun .ne. 'US' ) .and. ( coun .ne. 'UE' ) .and.
     +	      ( coun .ne. 'UM' ) .and. ( coun .ne. 'UW' ) .and.
     +	      ( coun .ne. 'AK' ) .and. ( coun .ne. 'HW' ) )  THEN
	    tmpf = PR_TMCF ( tmpf )
	    dwpf = PR_TMCF ( dwpf )
	END IF
C
C*	Get the cloud information.
C
	CALL RA_CLDS  ( irpntr, iptbef, maxcld, ipcaft, cldtyp, cldhgt, 
     +			ncld, ier )
C
C*	Get the visibility and weather.
C
	CALL RA_VSBY  ( ipcaft, iptbef, ipvaft, vsby, ier )
	CALL RA_WTHR  ( ipvaft, iptbef, wcod, wnum, ier )
C
C*      Check to see if this is 3 hour report. If so check for
C*	extra data.
C
	remain = MOD (ighr,3)
	IF ( ( ( remain .eq. 0 ) .or. ( ighr .eq. 0 ) .or.
     +	       ( ighr.eq.8 ) ) .and. ( coun .eq. 'US' ) ) THEN
C
C*	    Set synflg for main synoptic hours of 0, 6, 12, 18 Z
C
	    IF ( ( ighr .eq.  0 ) .or. ( ighr .eq. 6 ) .or.
     +		 ( ighr .eq. 12 ) .or. ( ighr .eq. 18 ) )
     +		synflg = .true.
C
C*          Get the data that comes after the remarks (pressure 
C*          tendency, precipitation(3-hr & 24-hr), max/min
C*          temp, snowfall accumulation, and minutes of sunshine)
C
	    CALL RA_PTND  ( ipaaft, synflg, asoflg, ipnaft, p03d,
     +			    p03i, p06i, hsun, sunflg, ier )
	    IF  ( sunflg )  THEN
C
C*		If sunshine data is present, nothing else will be
C*		so skip over code
C
		snow = RMISSD 
		weqs = RMISSD
		tmdx = RMISSD
		t06x = RMISSD
		t12x = RMISSD
		t24x = RMISSD
		tmdn = RMISSD
		t06n = RMISSD
		t12n = RMISSD
		t24n = RMISSD
		p24i = RMISSD 
		csyl = RMISSD
		csym = RMISSD
		csyh = RMISSD
	    ELSE
C
C*	    Keep going for more data
C
		IF  ( .not. asoflg )  THEN
		    CALL RA_CSYB  ( ipnaft, ipoaft, csyl, csym, csyh,
     +				    ier )
		    CALL RA_SNOW  ( ipoaft, ipnaft, snow, weqs, ier )
                ELSE
                   snow = RMISSD
                   weqs = RMISSD
                   csyl = RMISSD
                   csym = RMISSD
                   csyh = RMISSD
		ENDIF
		CALL RA_TMPX  ( ipnaft, ighr, asoflg, ipxaft, 
     +				t06x, t12x, t24x, tmdx,
     +				t06n, t12n, t24n, tmdn, ier )
		CALL RA_P24I  ( ipxaft, asoflg, p24i, ier )
C
C*		Check for encoded temps that correspond to max/min
C*		temps below 0F or above 100F. These are encoded as
C*		thhx+100 and thhx-100, respectivly.
C*		Examples: -15 is reported as -15 + 100 = 85;
C*			  105 as 105 - 100 = 5.
C*		Test for this by seeing if the reported max/min temp
C*		differs from the current temp by more than 50. If so
C*		correct it.
C
		IF  ( .not. asoflg )  THEN
C
		    isign = 1
		    IF  ( ( .not. ERMISS ( tmpf ) ) .and.
     +			  ( tmpf .lt. 50. ) )
     +			isign = -1
C
		    IF  ( ( .not. ERMISS ( t12x ) ) .and.
     +			  ( ABS ( tmpf - t12x ) .gt. 50. ) ) 
     +			t12x = t12x + isign * 100.
C
		    IF  ( ( .not. ERMISS ( tmdx ) ) .and.
     +			  ( ABS ( tmpf - tmdx ) .gt. 50. ) )
     +			tmdx = tmdx + isign * 100.
C
		    IF  ( ( .not. ERMISS ( t24x ) ) .and.
     +			  ( ABS ( tmpf - t24x ) .gt. 50. ) )
     +			t24x = t24x + isign * 100.
C
		    IF  ( ( .not. ERMISS ( t12n ) ) .and.
     +			  ( ABS ( tmpf - t12n ) .gt. 50. ) )
     +			t12n = t12n + isign * 100.
C
		    IF  ( ( .not. ERMISS ( t24n ) ) .and.
     +			  ( ABS ( tmpf - t24n ) .gt. 50. ) )
     +			t24n = t24n + isign * 100.
C
		ENDIF
	    ENDIF
	ELSE
C
C*	    Not a three hour report; so no more data
C
	    p03d = RMISSD
	    p03i = RMISSD
	    p06i = RMISSD
	    snow = RMISSD
	    weqs = RMISSD
	    hsun = RMISSD
	    t06x = RMISSD
	    t12x = RMISSD
	    t24x = RMISSD
	    tmdx = RMISSD
	    t06n = RMISSD
	    t12n = RMISSD
	    t24n = RMISSD
	    tmdn = RMISSD
	    p24i = RMISSD
	    csyl = RMISSD
	    csym = RMISSD
	    csyh = RMISSD
	END IF
C
C*	If there is no information other than winds, return an error.
C
	IF  ( ( .not. ERMISS (tmpf) ) .or. ( .not. ERMISS (dwpf) ) .or.
     +	      ( .not. ERMISS (pres) ) .or. ( .not. ERMISS (vsby) ) .or.
     +	      ( .not. ERMISS (alti) ) .or. ( ncld .gt. 0 ) )  THEN
C*
	ELSE
	    iret = -5
	END IF
C*
	RETURN 
	END
