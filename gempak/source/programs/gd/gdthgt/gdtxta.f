	SUBROUTINE GDTXTA ( taxis, maxlbl, npts, timfnd, x, xstrt,
     +			    xstop, xtlbl, ctlbl, nxlbl, xmndst,
     +                      ilbfrq, iglfrq, itmfrq, iret )
C************************************************************************
C* GDTXTA 								*
C*									*
C* This subroutine determines the values to use for the time axis in	*
C* a time series program.  TAXIS must be in the form:			*
C*                                                                      *
C*            tstart-tstop-tinc;lblfrq;glnfrq;ticfrq			*
C*									*
C* where the last three are the frequencies for labels, grid lines	*
C* and tick marks.							*
C*                                                                      *
C* Defaults will be set for all values not supplied explicitly.		*
C*									*
C* GDTXTA ( TAXIS, MAXLBL, NPTS, TIMFND, X, XSTRT, XSTOP, XTLBL,	*
C*          CTLBL, NXLBL, XMNDST, ILBFRQ, IGLFRQ, ITMFRQ, IRET )	*
C*									*
C* Input parameters:							*
C*	TAXIS		CHAR*		User input for T axis		*
C*	MAXLBL          INTEGER         Maximum number of labels	*
C*	NPTS		INTEGER		Number of times			*
C*	TIMFND (NPTS)	CHAR*		GEMPAK times			*
C*									*
C* Output parameters:							*
C*	X     (NPTS)	REAL		X positions of times in days	*
C*	XSTRT		REAL		Left value of x			*
C*	XSTOP		REAL		Right value of x		*
C*	XTLBL (NXLBL)	REAL		X axis label positions		*
C*	CTLBL (NXLBL)	CHAR*		X axis labels			*
C*	NXLBL		INTEGER		Number of x axis labels		*
C*	XMNDST          REAL            Min time separation in days 	*
C*	ILBFRQ          INTEGER         Label frequency			*
C*	IGLFRQ          INTEGER         Grid line frequency		*
C*	ITMFRQ          INTEGER         Tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -7 = time range is size zero	*
C*					  -8 = too many labels		*
C**									*
C* Log:									*
C* K. Brill/GSC          5/90   					*
C* S. Schotz/GSC	 7/90	Change xmndst to min time difference	*
C* M. desJardins/GSFC	 8/90	Change range separator to -		*
C* T.W.Barker/WR/SSD	 8/91	Created from in_taxs			*
C* K. Brill/NMC		11/92	Removed print *,			*
C* T. Lee/GSC		 3/99	Initialized nskp to 1			*
C* D. Kidwell/NCEP	 4/99	Fixed for Y2K                           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	taxis, timfnd (*), ctlbl (*)
	REAL		x (*), xtlbl (*)
C*
	LOGICAL		min, taxrev, notdon, hrs
	CHARACTER	tpart1*20, tpart2*20
	CHARACTER*20	tarr (3), tstrt, tstop, temp, lbl1, cstrt, cstop
	INTEGER		ifrq (3), ifrstt (5), ilastt (5), itnow (5),
     +			iarr (3)
C
C*	These data values are in min; starting with 60, these represent
C*	1 hr, 3 hr, 6 hr, 12 hr, 1 d, 2 d, 4 d, 7 d .
C
	INTEGER		kintvl (16)
	DATA		kintvl / 1, 2, 3, 5, 10, 15, 20, 30, 60, 180,
     +			         360, 720, 1440, 2880, 5760, 10080 /
C------------------------------------------------------------------------
	iret = 0
C
C*	Break the input for TAXIS into two parts at the semicolon.
C
	isemic = INDEX ( taxis, ';' )
	IF ( isemic .eq. 0 ) THEN
	    tpart1 = taxis
	    tpart2 = ' '
	ELSE IF ( isemic .eq. 1 ) THEN
	    tpart1 = ' '
	    tpart2 = taxis ( 2: )
	ELSE
	    tpart1 = taxis ( : isemic - 1 )
	    tpart2 = taxis ( isemic + 1 : )
	END IF
C
C*	Get the frequencies for labels, grid lines and tick marks.
C
	CALL ST_ILST ( tpart2, ';', IMISSD, 3, iarr, niarr, ier )
	ilbfrq = iarr (1)
	IF ( ilbfrq .eq. IMISSD ) ilbfrq = 1
	iglfrq = iarr (2)
	IF ( iglfrq .eq. IMISSD ) iglfrq = 0
	itmfrq = iarr (3)
	IF ( itmfrq .eq. IMISSD ) itmfrq = 1
C
C*	Process the time information.
C
	CALL ST_CLST ( tpart1, '-', ' ', 3, tarr, ntarr, ier )
C
C*	TAXREV is initialized to .FALSE., except when TARR (1) (1:1) is
C*	'R' (for 'reverse'), in which case TARR (1) is blanked and 
C*	TAXREV set to .TRUE. .
C
	IF ( ( tarr (1) ( 1:1 ) .eq. 'r' ) .or.
     +	     ( tarr (1) ( 1:1 ) .eq. 'R' ) ) THEN
	    taxrev   = .true.
	    temp = tarr (1) (2:)
	    tarr(1) = temp
	ELSE
	    taxrev   = .false.
	END IF
C
C*	For the start|stop values, substitute the first|last time
C*	found if no input, else put the start|stop in standard form
C*	and substitute characters from the first|last time found for
C*	blanks.  TIMFND holds valid date/times.  
C
	IF ( tarr (1) .eq. ' ' ) THEN
	    tstrt = timfnd (1) ( 1:11 )
	ELSE
	    CALL TI_STAN ( tarr (1), timfnd (1), tstrt, ier )
	    IF ( ier .ne. 0 ) THEN
		tstrt = timfnd (1) ( 1:11 )
	    END IF
	END IF
C
	IF ( tarr (2) .eq. ' ' ) THEN
	    tstop = timfnd (npts) ( 1:11 )
	ELSE
	    CALL TI_STAN ( tarr (2), timfnd (npts), tstop, ier )
	    IF ( ier .ne. 0 ) THEN
		tstop = timfnd (npts) ( 1:11 )
	    END IF
	END IF
C
C*	Use 4-digit year for date comparison.
C
	CALL TI_DTM4 ( tstrt, cstrt, ier )
	CALL TI_DTM4 ( tstop, cstop, ier )
C
C*	If CSTRT is later than CSTOP set TAXREV and interchange the
C*	values; find the difference between them in minutes.
C
	IF ( cstrt .gt. cstop ) THEN
	    temp   = tstrt 
	    tstrt  = tstop
	    tstop  = temp
	    taxrev = .true.
	END IF
C
C*	Find range of data in days.
C
	CALL TI_DIFD ( tstop, tstrt, days, ier )
	xstrt = 0.
	xstop = days
	ndays = INT ( days )
	IF ( days .eq. 0 ) THEN
	    iret = -7
	    CALL ER_WMSG ( 'IN', iret, ' ', ier )
	    RETURN
	END IF
C
C*	If TARR (3) is not blank, extract the hour and minute values
C*	and compute the increment in minutes.
C
	ndeltt = 0
	itest = 0
C*
	IF ( tarr (3) .ne. ' ' ) THEN
	    CALL ST_LSTR ( tarr (3), lt3, ier1 )
	    IF ( lt3 .ge. 3 ) THEN
	        CALL ST_INTG ( tarr (3) ( 1:lt3-2 ),   nhr,  ier2 )
	        CALL ST_INTG ( tarr (3) ( lt3-1:lt3 ), nmin, ier3 )
	    ELSE
		CALL ST_INTG ( tarr (3) ( 1:lt3 ),     nhr,  ier2 )
		nmin = 0
		ier3 = 0
	    END IF
	    IF ( ( ier1 + ier2 + ier3 ) .ne. 0 ) THEN
		tarr (3) = ' '
		IF ( ier2 .ne. 0 )
     +		    CALL ER_WMSG ( 'TI', ier2, ' ',         ier )
		IF ( ier3 .ne. 0 )
     +		    CALL ER_WMSG ( 'TI', ier3, ' ',         ier )
	    END IF
C*
	    ndeltt = nmin + nhr * 60
	    dayinc = FLOAT (ndeltt) / 1440.
	    IF ( dayinc .ne. 0. ) itest = days /dayinc
	END IF
C*
	IF ( tarr (3) .eq. ' ' .or. itest .gt. maxlbl ) THEN
C
C*	    Dr. Huffman's original scheme with some modifications is
C*	    used to get a default label increment, but it will only
C*	    be used for time periods of less than 120 days.
C
C*	    Use minutes as basis for label determination.
C
	    ntrang = INT ( days * 1440. + 1. )
C
C*	    If the increment is blank (or was reset to blank because it
C*	    was invalid), pick a test interval of NTRANG / 7 and then
C*	    take the closest "nice" value by sorting through KINTVL
C*	    (the default is 7 days).
C
	    kmin = ntrang / 7
	    IF ( kmin .eq. 0 ) kmin = 1
	    ipnt = 16
	    DO i = 1, 15
		IF ( kintvl (i) .le. kmin ) ipnt = i
	    END DO
C*
	    IF ( ipnt .lt. 16 ) THEN
		IF ( ( kmin - kintvl (ipnt) ) .lt. 
     +		     ( 2 * ( kintvl ( ipnt + 1 ) - kmin ) ) ) THEN
		    ndeltt = kintvl (ipnt)
		ELSE
		    ndeltt = kintvl (ipnt + 1)
		END IF
	    ELSE
		ndeltt = kintvl ( 16 )
	    END IF
C
C*	    Convert minutes of increment to HHHMM.
C
	    nhr    = ndeltt / 60
	    nhhhmm = ndeltt - nhr * 60 + nhr * 100
	    dayinc = FLOAT ( ndeltt ) / 1440.
	    CALL ST_INLN ( nhhhmm, tarr (3), len, ier )
C
C*	    Since TG_RINC expects leading zeroes, add them.
C
	    temp = '00000'
	    ic1 = 5 - len 
	    IF ( ic1 .gt. 0 ) THEN
		temp = temp (1:ic1) // tarr (3) (1:len)
		tarr (3) = temp
	    END IF
	END IF
C*
	IF ( ndays .lt. 120 ) THEN
C
C*	  Generate the labels; The first label is set at 00 for minutes
C*        or hours depending on the range.
C
	    mininc = dayinc * 1440.
	    lbl1 = tstrt
	    IF ( mininc .lt. 11 ) THEN
		lbl1 = tstrt ( 1:10 ) // '0'
	    ELSE IF ( mininc .lt. 120 ) THEN
		lbl1 = tstrt ( 1:9 ) // '00'
	    ELSE
		ihrinc = mininc / 60
		CALL TI_CTOI ( tstrt, ifrstt, ier )
		ihh = ifrstt (4) / ihrinc
		ifrstt (4) = ihh * ihrinc
		ifrstt (5) = 0
		CALL TI_ITOC ( ifrstt, lbl1, ier )
	    END IF
	    CALL TI_DIFD ( tstop, lbl1, tdays, ier )
	    IF ( dayinc .ne. 0. ) itest = tdays / dayinc
	    IF ( itest .gt. maxlbl ) THEN
		iret = -8
		CALL ER_WMSG ( 'IN', iret, ' ', ier )
		RETURN
	    END IF
	    CALL TG_RINC ( lbl1, tstop, tarr (3), 2, nxlbl, ctlbl,
     +			    ier )
C*
	ELSE IF ( ndays .ge. 120 .and. ndays .lt. 360 ) THEN
C
C*	    Default interval is months, but input increment is not
C*	    ignored.
C
	    IF ( itest .gt. 0 .and. itest .lt. maxlbl ) THEN
C*	    
		lbl1 = tstrt ( 1:7 ) // '00' // tstrt ( 10: )
		CALL TG_RINC ( lbl1, tstop, tarr (3), 2, nxlbl, ctlbl,
     +				ier )
C*
	    ELSE
C
C*		Generate default labels.
C
		CALL TI_CTOI ( tstrt, ifrstt, ier )
		CALL TI_CTOI ( tstop, ilastt, ier )	
		imonth = ifrstt (2)
		iyear  = ifrstt (1)
		itnow (5) = 0
		itnow (4) = 0
		itnow (3) = 1
		notdon = .true.	
		il = 0
		DO WHILE ( notdon )
		    il = il + 1
		    imonth = imonth + 1
		    IF ( imonth .gt. 12 ) THEN
			imonth = 1
			iyear = iyear + 1
		    END IF
		    IF ( iyear .gt. ilastt (1) .or.
     +			 imonth .gt. ilastt (2) .and.
     +			 iyear .eq. ilastt (1) .or.
     +			 il .eq. LLAXIS ) notdon = .false.
		    itnow (1) = iyear
		    itnow (2) = imonth
C
C*		    Make the label.
C
		    CALL TI_ITOC ( itnow, ctlbl (il), ier )
		END DO
		nxlbl = il
	    END IF
C*
	ELSE IF ( ndays .ge. 360 .and. ndays .lt. 720 ) THEN
C

C*	    Default interval is quarters; input increment is not
C*	    ignored.
C
	    IF ( itest .gt. 0 .and. itest .lt. maxlbl ) THEN
		lbl1 = tstrt ( 1:7 ) // '00' // tstrt ( 10: )
		CALL TG_RINC ( lbl1, tstop, tarr (3), 2, nxlbl, ctlbl,
     +				ier )
	    ELSE
C
C*		Generate default labels.
C
		CALL TI_CTOI ( tstrt, ifrstt, ier )
		CALL TI_CTOI ( tstop, ilastt, ier )	
		imonth = ifrstt (2)
		imonth = imonth / 3
		imonth = imonth * 3 + 1
		IF ( imonth .ge. 12 ) imonth = 10
		iyear  = ifrstt (1)
		itnow (5) = 0
		itnow (4) = 0
		itnow (3) = 1
		notdon = .true.	
		il = 0
		DO WHILE ( notdon )
		    il = il + 1
		    imonth = imonth + 3
		    IF ( imonth .ge. 12 ) THEN
			imonth = 1
			iyear = iyear + 1
		    END IF
		    IF ( iyear .gt. ilastt (1) .or.
     +			 imonth .gt. ilastt (2) .and.
     +			 iyear .eq. ilastt (1) .or. il .eq. LLAXIS )
     +			 notdon = .false.
		    itnow (1) = iyear
		    itnow (2) = imonth
C
C*		    Make the label.
C
		    CALL TI_ITOC ( itnow, ctlbl (il), ier )
		END DO
		nxlbl = il
	    END IF
C*
	ELSE IF ( ndays .ge. 720 .and. ndays .lt. 1825 ) THEN
C
C*	    Interval is best labeled in units of half years.  The
C*	    user input increment is ignored.
C
	    CALL TI_CTOI ( tstrt, ifrstt, ier )
	    CALL TI_CTOI ( tstop, ilastt, ier )	
	    imonth = ifrstt (2)
	    imonth = imonth / 6
	    imonth = imonth * 6 + 1
	    iyear  = ifrstt (1)
	    IF ( imonth .ge. 12 ) imonth = 7
	    itnow (5) = 0
	    itnow (4) = 0
	    itnow (3) = 1
	    notdon = .true.	
	    il = 0
	    DO WHILE ( notdon )
		il = il + 1
		imonth = imonth + 6
		IF ( imonth .ge. 12 ) THEN
		    imonth = 1
		    iyear = iyear + 1
		END IF
		IF ( iyear .gt. ilastt (1) .or.
     +		     imonth .gt. ilastt (2) .and.
     +		     iyear .eq. ilastt (1) .or. il .eq. LLAXIS )
     +		    notdon = .false.
		itnow (1) = iyear
		itnow (2) = imonth
C
C*		Make the label and determine the x position of it.
C
		CALL TI_ITOC ( itnow, ctlbl (il), ier )
	    END DO
	    nxlbl = il
C*
	ELSE
C
C*	    Label the axis in years; ignore the specified increment.
C
	    CALL TI_CTOI ( tstrt, ifrstt, ier )
	    CALL TI_CTOI ( tstop, ilastt, ier )	
	    dmin = FLOAT ( ifrstt (1) )
	    sav = FLOAT ( ilastt (1) ) 
	    dmax = AMAX1 ( dmin, sav )
	    dmin = AMIN1 ( dmin, sav )
	    start = dmin
	    stop = sav
	    CALL GR_AXLV ( dmin, dmax, start, stop, RMISSD, .false.,
     +			   .false., xtlbl, nxlbl, iret )
	    itnow (5) = 0
	    itnow (4) = 0
	    itnow (3) = 1
	    itnow (2) = 1
	    DO il = 1, nxlbl
		itnow (1) = INT ( xtlbl (il) )
C
C*		Make the label and determine the x position of it.
C
		CALL TI_ITOC ( itnow, ctlbl (il), ier )
	    END DO
C*
        END IF
C
C*	Set the x positions of the labels in days.
C
	DO il = 1, nxlbl
	    CALL TI_DIFD ( ctlbl (il), tstrt, xtlbl (il), ier )
	END DO
C
C*      Set the x positions of the data times in days.
C
        DO i = 1, npts
	    CALL TI_DIFD ( timfnd (i), tstrt, x (i), ier )
        END DO
C
C*	Find the minimum time separation in the data range.
C
	xmndst = xstop
	IF ( npts .gt. 1 ) THEN
	    DO i = 2, npts
		CALL TI_DIFD ( timfnd (i), timfnd (i-1), test, ier )
		test = ABS ( test )
		IF ( test .lt. xmndst ) xmndst = test
	    END DO
	END IF
C
C*	Abbreviate the time labels.  First, detect whether label
C*	minutes and hours are ever non-zero.
C
	min = .false.
	hrs = .false.
	DO i = 1, nxlbl
	    IF ( ctlbl (i) ( 10:11 ) .ne. '00' ) min = .true.
	    IF ( ctlbl (i) (  8:9  ) .ne. '00' ) hrs = .true.
	END DO
C
C*	Remove minutes if they are always zero.
C
	IF ( .not. min ) THEN
	    DO i = 1, nxlbl
		ctlbl (i) ( 10: ) = ' '
	    END DO
	END IF
C
C*	Remove hours if they are always zero.
C
	IF ( .not. hrs .and. .not. min ) THEN
	    DO i = 1, nxlbl
		ctlbl (i) ( 8: ) = ' '
	    END DO
	END IF
C
C*	Remove date elements which are duplicated in all the labels.
C
	nskp = 1
	IF ( nxlbl .gt. 1 ) THEN
	    IF ( ctlbl (1) ( 1:2 ) .eq. ctlbl (nxlbl) ( 1:2 ) ) THEN
		IF ( ctlbl (1) ( 3:4 ) .eq.
     +		     ctlbl (nxlbl) ( 3:4 ) ) THEN
		    IF ( ctlbl (1) ( 5:6 ) .eq.
     +			 ctlbl (nxlbl) ( 5:6 ) ) THEN
			nskp = 7
		    ELSE
			nskp = 5
		    END IF
		ELSE
		    nskp = 3
		END IF
	    ELSE
		nskp = 1
	    END IF
	END IF
C
	DO i = 1, nxlbl
	    ctlbl (i) = ctlbl (i) ( nskp: )
	END DO
C
C*	To reduce crowded labels, change label frequency default.
C
	IF ( ifrq (1) .eq. IMISSD .and. nxlbl .gt. 11 ) 
     +	    ilbfrq = nxlbl / 11 + 1
C
C*	Now that the times are set, reverse the order of XSTRT and
C*	XSTOP (used in defining the horizontal axis) if needed.
C
	IF ( taxrev ) THEN
	    xtemp  = xstrt 
	    xstrt  = xstop
	    xstop  = xtemp
	END IF
C*
	RETURN
	END
