	SUBROUTINE RA_PTND ( ipaaft, synflg, asoflg, ipnaft, p03d, p03i,
     +			     p06i, hsun, sunflg, iret )  
C************************************************************************
C*  RA_PTND								*
C*									*
C*  This subroutine decodes pressure tendency and 3-hr rainfall every	*
C*  three hours and the minutes of sunshine at 0800Z from an airways or	*
C*  ASOS report.							*
C*									*
C*  RA_PTND  ( IPAAFT, SYNFLG, ASOFLG, IPNAFT, P03D, P03I, P06I, HSUN,  *
C*		SUNFLG, IRET )						*
C*									*
C*  Input parameters:							*
C*	IPAAFT		INTEGER		First field after altimeter	*
C*	SYNFLG		LOGICAL		Flag for 0,6,12,18Z		*
C*	ASOFLG		LOGICAL		Flag for ASOS stations		*
C*									*
C*  Output parameters:							*
C*	IPNAFT		INTEGER		First field after pressure	*
C*					tendency			*
C*	P03D		REAL		Pressure tendency in mb+symbol	*
C*	P03I		REAL		Rainfall amount in inches (3hr)	*
C*	P06I		REAL		Rainfall amount in inches (6hr)	*
C*	HSUN		REAL		Hours of sunshine		*
C*	SUNFLG		LOGICAL		Flag indicating sunshine data	*
C*	IRET		INTEGER		Return code			*
C*					    0 =  normal			*
C*					   -1 =  error in decoding	*
C**									*
C*  Log:								*
C*  K. Miles/FSU	 6/90						*
C*  J. Walker/FSU	 1/91   Rewrote					*
C*  J. Whistler/SSAI	 7/91	Modified internal read			*
C*  P. Bruehl/Unidata	 4/93	Added pressure tendency symbol PTSY	*
C*  P. Bruehl/Unidata		Fixed so P03D is symbol+amount(mb)	*
C*  P. Bruehl/Unidata	 2/94	Added chk for 99ppp & up to 12in P03i 	*
C*  P. Bruehl/Unidata	 2/94	Added p06i for synflg (0,6,12,18Z rprts)*
C*  P. Bruehl/Unidata 	 3/94	Added ASOS flag & decoding		*
C*  P. Bruehl/Unidata	10/94	PK WND treated as remark field		*
C*  P. Bruehl/Unidata	11/94	Added all to RA_RMKS & fixed ovrrun	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
	LOGICAL		sunflg, remark, done, ovrrun
	LOGICAL		good, synflg, asoflg
	REAL		hsun
	CHARACTER	sss*2
	INTEGER		amount, rain
C-----------------------------------------------------------------------
	iret     = 0
	p03d     = RMISSD
	p03i     = RMISSD
	p06i	 = RMISSD
	hsun     = RMISSD
	ipnaft   = ipaaft
	inches   = 0
	sunflg   = .false.
	done     = .false.
	ovrrun   = .false.
	good     = .false.
	remark   = .false.
C
C*	Check next field for '/' and see if remarks or numbers present
C
	IF  ( ipnaft .ge. nfield )  THEN
	    RETURN
	ELSE IF ( iftype (ipnaft) .eq. 3 )  THEN
	    ipnaft = ipnaft + 1
	    IF  ( iftype (ipnaft) .eq. 1 )  THEN
		remark = .true.
	    ELSE IF ( iftype (ipnaft) .eq. 2 )  THEN
		remark = .false.
	    ENDIF
	ELSE IF ( iftype (ipnaft) .eq. 1 ) THEN
C
C*	Character w/no leading slash--treat as REM field
C
	    remark = .true.
	ENDIF
C
C*	If remarks are present, call RA_RMKS to skip them.
C
	IF ( remark )  THEN
		CALL RA_RMKS ( ipnaft, remark, ovrrun, ipraft, iret )
		ipnaft = ipraft
	ENDIF	

	IF ( (iftype (ipnaft) .eq. 1) .and. 
     +			(cfield(ipnaft) .eq. "PK") ) THEN
C
C*	Special PK WND notation--done decoding.  Send back sunflag as 
C*	true so no other 3 & 6 hourly decoding is performed
C
		sunflg = .true.
		hsun = RMISSD	  
		ipnaft = nfield
		RETURN
	ENDIF 

C
C*	Look for either sun group or apprr group
C
	IF  ( .not. ovrrun )  THEN
	    IF ( ( ifsize (ipnaft) .eq. 5 ) .and.
     + 		 ( ifintg (ipnaft) .ge. 98000 ) )  THEN
		sunflg = .true.
	    END IF
C
C*	    Decode sun group if present
C
	    IF  ( sunflg )  THEN
		hsun = ifintg (ipnaft) - 98000
		ipnaft = nfield
		RETURN
	    ELSE IF  ( asoflg )  THEN
C
C*	 	OTHERWISE--If present, decode ASOS report
C
		IF ( ( ifsize (ipnaft) .eq. 5 ) .and.
     +		     ( iftype (ipnaft) .eq. 2 ) .and.
     +		     ( ( ifintg (ipnaft) / 10000 ) .eq. 5 ) ) THEN
C
C*		    5appp pressure tendency group
C
		    good = .true.
		    indcat = ( ifintg (ipnaft) / 1000 ) - 50
		    amount = MOD ( ifintg (ipnaft), 1000 ) 
		    ipnaft = ipnaft + 1
		ENDIF
		IF ( ( ifsize (ipnaft) .eq. 4 ) .and.
     +		     ( iftype (ipnaft) .eq. 2 ) .and.
     +		     ( ( ifintg (ipnaft) / 1000 ) .eq. 6 ) .and.
     +		     ( iftype (ipnaft+1) .eq. 3 ) ) THEN
C
C*		    6RRR/ precip group
C
		    rain = MOD ( ifintg (ipnaft),  1000 )
		    ipnaft = ipnaft + 2
		    IF ( synflg ) THEN
			p06i =  rain / 100.
		    ELSE 
			p03i =  rain / 100.
		    ENDIF
		ENDIF
	    ELSE IF ( ( ifsize (ipnaft) .eq. 3 ) .and.
     +		      ( iftype (ipnaft) .eq. 2 ) )  THEN
C
C*		If present, decode app group only (3 digits)
C
		indcat = ifintg (ipnaft) / 100
		good = .true.
C
C*		Check if 1st number in group is 9 (snow codes)
C
		IF  ( indcat .eq. 9 )  THEN
		    p03d = RMISSD
		    RETURN 
		ENDIF
		amount = MOD ( ifintg (ipnaft), 100 )
		ipnaft = ipnaft + 1
C
C*		If amount = 99, then more data in next group (99ppp)
C
		IF  ( ( amount .eq. 99 ) .and. 
     +		      ( ( ifsize (ipnaft) .eq. 5 ) .and.
     +			( iftype (ipnaft) .eq. 2 ) ) ) THEN
		    amount = MOD ( ifintg (ipnaft), 10000 )
		    ipnaft = ipnaft + 1
		ENDIF
	    ELSE IF ( ( ifsize (ipnaft) .eq. 5 ) .and. 
     + 		      ( iftype (ipnaft) .eq. 2 ) )  THEN
C
C*		If present, decode apprr group (5 digits)
C
		indcat = ifintg (ipnaft) / 10000
		good = .true.
C
C*		Check if 1st number in group is 9 (snow codes)
C
		IF  ( indcat .eq. 9 )  THEN
		    p03d = RMISSD
		    RETURN 
		ENDIF
C
C*		Get the amount of change
C
		sss = cfield(ipnaft)(2:3)
		READ ( sss, 1000, IOSTAT=ios ) amount
1000		FORMAT ( BN, I2 )
		IF ( ios .ne. 0 ) RETURN
C
C*		Get the rainfall 
C
		rain = MOD ( ifintg (ipnaft), 100 )
		ipnaft = ipnaft + 1
		IF  ( ( ipnaft .le. nfield) .and. 
     +		      ( iftype (ipnaft) .eq. 1 ) )  THEN
C
C*		    Look for the written out rainfall in inches 
C*		    to add to 'rain'
C
		    IF ( cfield (ipnaft) .eq. 'ONE'          )  THEN
			inches = 100
		      ELSE IF ( cfield (ipnaft) .eq. 'TWO'   )  THEN
			inches = 200
		      ELSE IF ( cfield (ipnaft) .eq. 'THREE' )  THEN
			inches = 300
		      ELSE IF ( cfield (ipnaft) .eq. 'FOUR'  )  THEN
			inches = 400
		      ELSE IF ( cfield (ipnaft) .eq. 'FIVE'  )  THEN
			inches = 500
		      ELSE IF ( cfield (ipnaft) .eq. 'SIX'   )  THEN
			inches = 600
		      ELSE IF ( cfield (ipnaft) .eq. 'SEVEN' )  THEN
			inches = 700
		      ELSE IF ( cfield (ipnaft) .eq. 'EIGHT' )  THEN
			inches = 800
		      ELSE IF ( cfield (ipnaft) .eq. 'NINE'  )  THEN
			inches = 900
		      ELSE IF ( cfield (ipnaft) .eq. 'TEN'   )  THEN
			inches = 1000
		      ELSE IF ( cfield (ipnaft) .eq. 'ELEVEN' )  THEN
			inches = 1100
		      ELSE IF ( cfield (ipnaft) .eq. 'TWELVE'  )  THEN
			inches = 1200
		    ENDIF
		END IF
	        IF  ( synflg ) THEN 
		    p06i = ( rain + inches ) / 100.
		    IF  ( p06i .gt. 0.99 ) ipnaft = ipnaft + 1 
		    IF  ( ( p06i .lt. 0.0 ) .or. ( p06i .gt. 12.99 ) )
     +			p06i = RMISSD
		ELSE 
		    p03i = ( rain + inches ) / 100.
		    IF  ( p03i .gt. 0.99 ) ipnaft = ipnaft + 1 
		    IF  ( ( p03i .lt. 0.0 ) .or. ( p03i .gt. 12.99 ) )
     +			p03i = RMISSD
		ENDIF
		IF ( iftype (ipnaft) .eq. 1 ) ipnaft = ipnaft +1
C
C*		If press change amount = 99, then more data in
C*		next group (99ppp)
C
		IF  ( ( amount .eq. 99 ) .and. 
     +		      ( ( ifsize (ipnaft) .eq. 5 ) .and.
     +			( iftype (ipnaft) .eq. 2 ) ) ) THEN
		    amount = MOD ( ifintg (ipnaft), 10000 )
		    ipnaft = ipnaft + 1
		ENDIF
	    ENDIF
C
C*	    Compute the tendency
C
	    IF ( indcat .ge. 5 )  THEN 
		isign = -1       
	    ELSE
		isign =  1
	    ENDIF
	    IF ( good ) p03d = float (indcat) * 1000. + amount
	END IF
C
C*	Handle over run in REMARKS field
C
	IF ( ovrrun ) THEN
		sunflg = .true.
		hsun = RMISSD	  
		ipnaft = nfield
	ENDIF	
	RETURN
	END
