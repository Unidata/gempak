	SUBROUTINE RA_SNOW ( ipoaft, ipnaft, snow, weqs, iret )
C************************************************************************
C*  RA_SNOW								*
C*									*
C*  This subroutine decodes the snow accumulation amount and the water	*
C*  equivalence of snow depth on the ground in an airways report.	*
C*									*
C*  RA_SNOW  ( IPOAFT, IPNAFT, SNOW, WEQS, IRET )			*
C*									*
C*  Input parameters: 							*
C*	IPOAFT		INTEGER		First field after the cloud	*
C*					code group			*
C*									*
C*  Output parameters:							*
C*	IPNAFT		INTEGER		First field after the snow	*
C*					accumulation group		*
C*	SNOW		REAL		Snow accumulation amount in	*
C*					inches				*
C*	WEQS		REAL		Water equivalent of depth of	*
C*					snow on ground			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal			*
C*					  -1 = end of SAO		*
C*					  -2 = no snow groups		*
C**									*
C*  Log:								*
C*  K. MILES/FSU	 6/90						*
C*  J. Walker/FSU	 1/91		Rewrote				*
C*  P. Bruehl/Unidata	 2/94		Rewrote & Added 902(3) grp WEQS	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
	LOGICAL		done, fin
	REAL		snow, weqs
C-----------------------------------------------------------------------
	iret   = 0
	snow   = RMISSD
	weqs   = RMISSD
	ipnaft = ipoaft
	done   = .false.
	fin    = .false.
C
	DO WHILE  ( .not. done )
	    IF  ( ipnaft .gt. nfield )  THEN
		ipnaft = ipoaft
		iret = -1
		RETURN	
C
C*	    Look for 5 digit integer group starting with 90X
C
	    ELSE IF ( ( ifsize (ipnaft) .eq. 5 ) .and. 
     +		      ( iftype (ipnaft) .eq. 2 ) ) THEN
		indcat = ifintg (ipnaft) / 100
		IF  ( indcat .eq. 902 )  THEN
C
C*		    Water equivalent of depth of snow on ground
C*		    in inches.
C
		    weqs = ( MOD ( ifintg (ipnaft), 100 ) ) / 10.
		    ipnaft = ipnaft + 1
C
C*		    Check for 903 group
C			
		    IF  ( ( ifsize (ipnaft) .eq. 5 ) .and. 
     +		          ( iftype (ipnaft) .eq. 2 ) .and.
     +		          ( ( ifintg (ipnaft) / 100 ) .eq. 903 ) ) THEN
			weqs = weqs + ( MOD ( ifintg (ipnaft), 100 ) )
			ipnaft = ipnaft + 1
		    ENDIF
C
		ELSE IF  ( indcat .eq. 904 )  THEN
C
C*		    Depth of Snow on ground (sum over all 904 groups)
C
		    fin = .false.
		    snow = 0.0
		    DO WHILE  ( .not. fin )
			indcat2 = ifintg (ipnaft) / 100
			IF  ( ( indcat2 .ne. 904 ) .or. 
     +			      ( ifsize (ipnaft) .ne. 5 ) ) THEN
			    fin = .true.
			ELSE 
			    snows = MOD ( ifintg (ipnaft), 100 )
			    IF  ( snows .eq. 99. )  snows = 100.
			    snow = snow + snows
			    ipnaft = ipnaft + 1
			    snows = 0.
			ENDIF	
		    END DO
C
		ELSE 
C
C*		    Not a snow or water equivalent group
C
		    done = .true.
		    iret = -2
		    RETURN
		ENDIF	  
	    ELSE
C
C*		Not a 5 digit group
C
		done = .true.
		iret = -2
		RETURN
	    ENDIF
C
	END DO
C*
	RETURN
	END
