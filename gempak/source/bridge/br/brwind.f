	SUBROUTINE BR_WIND ( strwnd, drct, sknt, sped, gust, gums,
     +			     idecd, iret )               
C************************************************************************
C* BR_WIND                                                              *
C*                                                                      *
C* This subroutine will decode the wind direction, speed, and gust     	*
C* speed from a METAR or TAF-formatted wind field.  Wind speeds are     *
C* stored in the units in which they are reported.     		        *
C* 								        *
C* BR_WIND ( STRWND, DRCT, SKNT, SPED, GUST, GUMS, IDECD, IRET )        *
C*								        *
C* Input parameters: 						        *
C*      STRWND		CHAR*		Wind field			*
C*								        *
C* Output parameters:						        *
C*      DRCT   		REAL		Wind direction                  *
C*      SKNT   		REAL		Wind speed (kts)                *
C*      SPED   		REAL		Wind speed (m/sec)              *
C*      GUST   		REAL		Gust wind speed (kts)           *
C*      GUMS   		REAL		Gust wind speed (m/sec)         *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  1 = field decoded	        *
C*					  0 = field not decoded     	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return		*
C*	                                  2 = units in wrong position	*
C*					  3 = presumed in KTS		*
C*					  4 = wind direction miscoding	*
C*					  5 = wind speed miscoding	*
C*					  6 = excessive variable wind	*
C*					  7 = wind gust miscoding	*
C*					 -1 = no wind field found	*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	 9/02   Code taken from MT_WIND, no interface   *
C* C. Caruso Magee/NCEP  3/04	Added check for max gust indicator 'GP'	*
C*				and max wind indicator 'P'.		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	strwnd
C*
	LOGICAL 	igust, imaxn, imaxg, wnderr
C-----------------------------------------------------------------------
	iret  = 0
	idecd = 0
	drct  = RMISSD
	sknt  = RMISSD
	sped  = RMISSD
	gust  = RMISSD
	gums  = RMISSD
C
	wnderr = .false.
	iwspun = IMISSD
C
C*	First determine units of wind speed.
C
	CALL BR_VALD ( strwnd, 6, 12, 'KT', 1, iloc,  iret  )
	IF ( iret .lt. 0 ) THEN
C
C*	    Check for a miscoded wind group ending in 'KZ'.
C
	    CALL BR_VALD ( strwnd, 6, 12, 'KZ', 1, iloc, ier ) 
	    IF ( ier .eq. 0 ) iret = 3
	END IF
	CALL BR_VALD ( strwnd, 6, 12,  'G', 1, ilocg, iretg )
	IF ( iret .ge. 0 ) THEN
C
C*	    Set units to knots.
C
	    iwspun = 0
 	    idecd  = 1
	    IF ( ( (  iret .eq. 1  .or.   iret .eq. 3  )  
     +             .and.  ( iloc .eq. 5 ) )    .or. 
     +		 (  iretg .eq. 1  .and.  ilocg .eq. 5  ) ) THEN 
C
C*		String missing a zero, so insert one.
C
		IF ( strwnd ( 2:2 ) .ne. '0' ) THEN
		    strwnd = strwnd ( 1:2 ) // '0' // strwnd (3:4)
		  ELSE 
		    strwnd = '0' // strwnd (1:4)
		END IF
		iret = 0
		iloc = 6
	      ELSE IF ( iret .eq. 1 ) THEN
		iret = 2
	    END IF
	  ELSE  
            CALL BR_VALD ( strwnd, 6, 12, 'MPS', 1, iloc, iret )
	    IF ( iret .eq. 0 )  THEN
C
C*		Set units to meters per second.
C
		iwspun = 1
		idecd  = 1
	      ELSE IF ( iret .gt. 0 ) THEN
C
C*		MPS found elsewhere in string.
C
		iwspun = 1
		idecd  = 1
		iret   = 2
	      ELSE 
C
C*		KMH was removed as a wind unit in 2010. However,
C*		it is still here so that we can decode old data.
C
		CALL BR_VALD ( strwnd, 6, 12, 'KMH', 1, iloc, iret )
		IF ( iret .eq. 0 ) THEN
C
C*		Set units to kilometers per hour.
C
		    iwspun = 2
		    idecd  = 1
		  ELSE IF ( iret .gt. 0 ) THEN
C
C*		    KMH found elsewhere in string.
C
		    iwspun = 2
		    idecd  = 1
		    iret   = 2
		  ELSE
C
C*		    Unable to determine units; check for miscoding.
C
	            CALL BR_VALD ( strwnd, 6, 12, 'K', 0, iloc2, iret2 )
	            CALL BR_VALD ( strwnd, 6, 12, 'T', 0, iloc3, iret3 )
C
C*		    If 'T', look for preceding 'F' - probably runway.
C
		    IF ( iret3 .eq. 0 ) THEN
		        IF ( strwnd (iloc3-1:iloc3-1) .eq. 'F' ) 
     +			  iret3 = -1
		    END IF
	            IF ( ( iret2 .eq. 0 ) .or. (iret3 .eq. 0 ) ) THEN
		        lens = INDEX ( strwnd, ' ' ) - 1
		        IF ( ( lens .eq. iloc2 ) .or. 
     +			     ( lens .eq. iloc3) ) THEN
		            iret   = 3
			    iwspun = 0
			    idecd  = 1
			    IF ( iret2 .eq. iret3 ) THEN
			        iloc = MIN ( iloc2, iloc3 )
			      ELSE
			        iloc = lens
			        IF ( lens .eq. 7 ) iloc = 6
			    END IF
		         END IF   
	            END IF
		END IF
	    END IF
	END IF
C
	IF ( iret .ge. 0 ) THEN
C
C*          Decode wind group.
C
C*	    First check for and remove compass direction.
C
	    IF ( ( strwnd ( 1:1 ) .eq. 'N' ) .or.
     +		 ( strwnd ( 1:1 ) .eq. 'S' ) .or.
     +		 ( strwnd ( 1:1 ) .eq. 'E' ) .or.
     +		 ( strwnd ( 1:1 ) .eq. 'W' ) ) THEN
      		strwnd = strwnd ( 2: )
		iloc = iloc - 1
	    END IF
C
C*	    Check for gust indicator.
C
	    igust = .false.
	    ieloc = INDEX ( strwnd, 'G' )
C
C*	    Check for max gust wind indicator letters. 
C
	    imaxg = .false.
            igploc = INDEX ( strwnd, 'GP' )
	    IF ( igploc .gt. 4 ) imaxg = .true.
	    IF ( ieloc .gt. 4 ) THEN
		iglen = iloc - ieloc
		IF ( ( ( .not. imaxg ) .and. ( ( iglen .eq. 3 ) .or. 
     +                 ( iglen .eq. 4 ) ) ) .or. 
     +               ( ( imaxg ) .and. ( ( iglen .eq. 4 ) .or.
     +                 ( iglen .eq. 5 ) ) ) )
     +             igust = .true.
	    END IF
	    IF ( .not. igust ) ieloc = iloc
C
C*	    Check for max wind indicator (indicates that wind is at least
C*          the speed given, but highest wind isn't encoded).
C*          If iploc = 4, max wind indicator found.  Wind speed is found 
C*          in columns 5 and 6.   
C
	    imaxn = .false.
	    iploc = INDEX ( strwnd, 'P' )
	    IF ( iploc .eq. 4 ) imaxn = .true.
C
C*	    Decode wind direction.
C
	    CALL ST_INTG ( strwnd ( 1:3 ), iwdir, jret )
	    IF ( jret .eq. 0 ) THEN
		IF ( ( iwdir .lt. 0 ) .or. ( iwdir .gt. 360 ) .or. 
     +		     ( MOD ( iwdir, 10) .ne. 0 ) ) wnderr=.true.
	      ELSE
	        IF ( strwnd ( 1:3 ) .eq. 'VRB' ) THEN
		    iwdir = -99
		  ELSE
		    wnderr = .true.
		END IF
	    END IF
C
	    IF ( wnderr ) THEN
	        iret = 4
	      ELSE
	        drct = FLOAT ( iwdir )
	    END IF
C
C*	    Decode wind speed.
C
	    IF ( .not. imaxn ) THEN
                istartws = 4
            ELSE
                istartws = 5
            END IF
	    IF ( ieloc - 1 .ge. istartws ) THEN
		CALL ST_INTG ( strwnd ( istartws:ieloc-1 ), iwspd, jret )
		IF ( ( iwspd .lt. 0 ) .or. ( iwspd .gt. 550 ) .or.
     +		   ( ( iwspun .eq. 0 ) .and. ( iwspd .gt. 300 ) ) .or.
     +		   ( ( iwspun .eq. 1 ) .and. ( iwspd .gt. 150 ) ) )
     +	           iret = 5
		IF ( ( strwnd(1:3) .eq. 'VRB') .and. ( ( iwspd .gt. 6 )
     +		   .or. ( ( iwspun .eq. 1 ) .and. ( iwspd .gt. 2 ) ) ) )
     +             iret = 6
	      ELSE
		iret = 5
	    END IF
	    IF ( iret .ne. 5 ) THEN
		IF ( iwspun .eq. 0 ) THEN
	    	    sknt = FLOAT ( iwspd )
		  ELSE IF ( iwspun .eq. 1 ) THEN
		    sped = FLOAT ( iwspd )
		  ELSE IF ( iwspun .eq. 2 ) THEN
		    sknt = FLOAT ( iwspd ) * .54
		END IF
	    END IF
C
C*	    Decode gust speed.
C
	    IF ( igust ) THEN
		IF ( .not. imaxg ) THEN
                    istartgs = ieloc + 1
                ELSE
                    istartgs = ieloc + 2
                END IF
	        IF ( ( istartgs ) .le. ( iloc -1 ) ) THEN
		    CALL ST_INTG (strwnd (istartgs:iloc-1), igstsp, jret)
		    IF ( ( iwspd .lt. 0 ) .or. ( iwspd .gt. 550 ) .or.
     +		       ( ( iwspun .eq. 0 ).and.( iwspd .gt. 300 ) ) .or.
     +		       ( ( iwspun .eq. 1 ).and.( iwspd .gt. 150 ) ) )
     +		       iret = 7
		  ELSE
		    iret = 7
		END IF
		IF ( iret .ne. 7 ) THEN
		    IF ( iwspun .eq. 0 ) THEN
	     		gust = FLOAT ( igstsp )
		      ELSE IF ( iwspun .eq. 1 ) THEN
	     		gums = FLOAT ( igstsp )
		      ELSE IF ( iwspun .eq. 2 ) THEN
	     		gust = FLOAT ( igstsp ) * .54
		    END IF
		END IF
	    END IF
C
	END IF
C*
	RETURN
	END
