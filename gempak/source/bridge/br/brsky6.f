	SUBROUTINE BR_SKY6 ( strsky, strht, autof, isky, vrtv, cmtn,
     +			     nsky, ctyl, idecd, twofld, iret )
C************************************************************************
C* BR_SKY6                                                              *
C*                                                                      *
C* This subroutine will decode up to six METAR or TAF-formatted sky     *
C* condition groups, and the cloud height/vertical visibility.  The     *
C* groups are translated into their GEMPAK cloud numbers by the         *
C* function BR_CMTN. 		         				*
C* 								        *
C* BR_SKY6 ( STRSKY, STRHT, AUTOF, ISKY, VRTV, CMTN, NSKY, CTYL,	*
C*	     IDECD, TWOFLD, IRET )					*
C*								        *
C* Input parameters: 						        *
C*      STRSKY		CHAR*		Sky condition field		*
C*      STRHT		CHAR*		Cloud height field		*
C*	AUTOF		REAL		Automatic station flag		*
C*								        *
C* Input and output parameters: 				        *
C*	ISKY		INTEGER		Relative number of sky group    *
C*								        *
C* Output parameters:						        *
C*      VRTV		REAL		Vertical visibility (feet)      *
C*      CMTN		REAL		GEMPAK sky cover code           *
C*      NSKY		INTEGER		Number of sky condition groups  *
C*      CTYL		REAL		Low-level cloud type WMO 0513   *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  2 = auto report with field	*
C*						decoded having slashes	*
C*					  1 = field decoded		*
C*					  0 = field not decoded		*
C*	TWOFLD		LOGICAL		Two field flag			*
C*	IRET		INTEGER		Return code                     *
C*	      		    		   0 = normal return		*
C*	                                  11 = invalid vert. visibility	*
C*					  -1 = no field found		*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	 9/02	Code taken from MT_SKY6, no interface   *
C* Yen&Caruso Magee/NCEP 3/04   Added check for 'NSC' and AUTO reports	*
C*                              sending '///' for sky field. (CSC)	*
C* Caruso Magee/NCEP     9/05   Added check for 'NCD'.                	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	strsky, strht
	LOGICAL 	twofld
C*
	CHARACTER 	str*9, string*3
	LOGICAL 	fndsky, vverr
C------------------------------------------------------------------------
	iret   = 0
	idecd  = 0
	twofld = .false.
	vrtv   = RMISSD
	cmtn   = RMISSD
	nsky   = IMISSD
	ctyl   = RMISSD
C
	vverr  = .false.
	fndsky = .true.
	skyc   = RMISSD
	string = strsky ( 1:3 )
C
C*      Look for 'NSC' - no sky cover or 'NCD' - no clouds detected.
C	
        IF ( string .eq. 'NSC' .or. string .eq. 'NCD' ) THEN
C
C*	    nsky and cmtn already have missing values, so just return
C
	    RETURN
C
C*        Look for AUTO report encoding '///' for sky field.
C*	  (Note that the length of string is 3, so test is true even
C*	  when sky field has more than 3 slashes.  MT_DROP already
C*	  removed fields beginning with at least 5 slashes.)
C
          ELSE IF ( autof .eq. 0. .and. string .eq. '///' ) THEN
	    idecd = 2
C
C*	    nsky and cmtn already have missing values
C
	    RETURN
C
C*        Look for vertical visibility - 'VV'.
C
	  ELSE IF ( string ( 1:2 ) .eq. 'VV' ) THEN
	    CALL ST_INTG ( strsky ( 3:5 ), ivertv, jret )
	    IF ( jret .ne. 0 ) THEN
C
C*	        Check for VV09 - common exception for US stations.
C		    
		IF ( strsky ( 3:5 ) .eq. '09 ' ) THEN
		    ivertv = 9
		  ELSE IF ( strsky ( 3:5 ) .ne. '///' ) THEN
		    vverr = .true. 
		END IF
	    END IF
C
C*	    Convert vertical visiblity to feet.
C
	    IF ( ivertv .ne. IMISSD ) THEN
                ivertv = ivertv * 100
		IF ( ivertv .ge. 50000 ) vverr = .true.
	    END IF
C
	    IF ( .not. vverr ) THEN
		vrtv = FLOAT ( ivertv )
	      ELSE
	        iret = 11
	    END IF
C
C*	    Get GEMPAK sky cover code for VV.
C
	    cmtn = BR_CMTN ( strsky )
C
C*	    Force end of check for sky condition if VV found.
C
	    isky = 7
C
C*	  Look for 'SKC' or 'CLR'.
C
	  ELSE IF ( ( string .eq. 'SKC' ) .or.
     +		  ( string .eq. 'CLR' ) ) THEN
	    cmtn = BR_CMTN ( string )
C
C*	    Force end of check for sky condition if SKC or CLR found.
C
	    isky = 7
C
C*	  Check for valid cloud amount.
C
	  ELSE IF ( ( string .eq. 'FEW' ) .or.
     +	          ( string .eq. 'SCT' ) .or.
     +	          ( string .eq. 'BKN' ) .or.
     +	          ( string .eq. 'OVC' ) ) THEN
	    lens = INDEX ( strsky, ' ' ) - 1
C
C*	    Check to see if cloud height is separated from cloud amount
C*	    by a blank.
C
	    IF ( lens .eq. 3 ) THEN
	       	lensh = INDEX ( strht, ' ' ) - 1
	        IF ( ( lensh .eq. 3 ) .or. 
     +		     ( ( lensh .eq. 5 ) .and. 
     +		       ( strht (4:5) .eq. 'CB' ) ) .or.
     +		     ( ( lensh .eq. 6 ) .and.
     +		       ( strht (4:6) .eq. 'TCU' ) ) ) THEN
		    str = string // strht ( 1:lensh )
		    twofld = .true.
		  ELSE
		    str = string
		END IF
	      ELSE
		str = strsky
	    END IF
	    IF ( str ( 4:6 ) .eq. 'OOO' ) str ( 4:6 ) = '000'
	    skyc = BR_CMTN ( str )
	  ELSE 
	    fndsky = .false.
	END IF
	IF ( fndsky ) THEN
C
C*	    Sky condition found.
C
	    idecd = 1
	    IF ( ( isky .lt. 7 ) .and. ( skyc .eq. 0 ) ) THEN
	      ELSE IF ( isky .lt. 7 ) THEN
     	        cmtn = skyc
		nsky = isky
C
C*		Check for special cases of CB and TCU, and set CTYL
C*		as needed.
C
		IF ( skyc .gt. 30000. ) THEN
		    ctyl = 9.
		  ELSE IF ( skyc .gt. 20000. ) THEN
		    ctyl = 2.
		END IF
	      ELSE
		nsky = 1
	    END IF
	  ELSE
	    iret = -1
	END IF
C*
	RETURN
	END
