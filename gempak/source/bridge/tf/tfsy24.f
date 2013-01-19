	SUBROUTINE TF_SY24 ( jrtarr, ntimes, jvehr, jstarr, ndxstr,
     +			     ndxend, nrtarr, iret )
C************************************************************************
C* TF_SY24								*
C*									*
C* This routine returns the forecast synoptic time from the beginning	*
C* valid time and if necessary determines it from the initial forecast	*
C* time and valid time period.	If the valid time period is not 24	*
C* hours, the beginning valid time to use will be obtained from the	*
C* ending valid time based on a 24-hour valid time period.  Then if the	*
C* beginning valid time (which should have the same hour as the ending	*
C* forecast time) is not a valid cycle hour (00, 06, 12, or 18), then	*
C* the closest hour will be used. If it is equally close to two of the	*
C* cycle hours, then the earlier hour is used.  The new initial	forecast*
C* time array nrtarr is the forecast time at index ndxstr.		*
C*									*
C* TF_SY24 ( JRTARR, NTIMES, JVEHR, JSTARR, NDXSTR, NDXEND, NRTARR,	*
C*	     IRET )			                		*
C*									*
C* Input parameters:							*
C*	JRTARR (5)	INTEGER		Initial forecast time array	*
C*	NTIMES		INTEGER		Number of forecast times	*
C*	JVEHR		INTEGER		Hour of ending valid period	*
C*									*
C* Output parameters:							*
C*	JSTARR (5)	INTEGER		Initial synoptic fcst time array*
C*	NDXSTR		INTEGER		Starting index of forecast data	*
C*	NDXEND		INTEGER		Ending index of forecast data	*
C*	NRTARR (5)	INTEGER		New initial forecast time array *
C*	IRET		INTEGER		Return code			*
C*					-1 = Invalid forecast times     *
C*					1 = Invalid ending valid hour   *
C*					2 = No forecast times to write	*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	7/07	Created                                 *
C* L. Lin/NCEP    	4/08	Allows to process fcst hours up to 30   *
C* L. Lin/NCEP    11/08	Return a proper output file for TAF 30  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	INTEGER         jrtarr (*), jstarr (*), nrtarr (*)
C*
	INTEGER		lvalhr (4), netarr (5)
	LOGICAL		valsyn
        LOGICAL         plus6
C*
        INTEGER         maxhr
C*
	DATA		lvalhr / 0, 6, 12, 18 /
C------------------------------------------------------------------------
	iret = 0
C
C*      The number of forecast times should be up to 30 hours for now
C
        IF ( ( ntimes .le. 24 ) .and. ( ntimes .ge. 0 ) ) THEN
           maxhr = 24
        ELSE IF ( ( ntimes .le. 30 ) .and. ( ntimes .ge. 0 ) ) THEN
           maxhr = 30
        ELSE IF ( ntimes .gt. 30 ) THEN
           maxhr = 30
           ntimes = 30
        END IF
C
C*	Check for a 24 hour valid period.  jvehr should be greater than
C*	and equal to 0 and less than 24 due to processing in tf_decd.
C
	IF ( jvehr .ge. 24 .or. jvehr .lt. 0 ) THEN
	    jvehr = mod ( jvehr, 24 )
	END IF
C
	IF ( ntimes .eq. 30 .or. ntimes .eq. 24 ) THEN
C
C*	    It is a 24/30-hour valid time period, so jrtarr has the
C*	    beginning valid time desired.
C
	    DO i = 1, 5
		jstarr (i) = jrtarr (i)
	    END DO
          ELSE IF ( ntimes .gt. 24) THEN
C
C*	    Since not a 30 hour time period, determine the beginning
C*	    valid time based on a 30 hour valid time period.  First,
C*	    compute the date of the ending valid time in netarr.
C*	    Then subtract 30 hour from netarr.
C
	    minuts = ntimes * 60
	    CALL TI_ADDM ( jrtarr, minuts, netarr, ier )
            minuts = 30 * 60
	    CALL TI_SUBM ( netarr, minuts, jstarr, ier )
          ELSE
C
C*	    Since not a 24 hour time period, determine the beginning
C*	    valid time based on a 24 hour valid time period.  First,
C*	    compute the date of the ending valid time in netarr.
C*	    Then subtract one day from netarr.
C
	    minuts = ntimes * 60
	    CALL TI_ADDM ( jrtarr, minuts, netarr, ier )
	    CALL TI_SUBD ( netarr, jstarr, ier )
	END IF
C
C*	Check jstarr for a valid synoptic time
C
	valsyn = .false.
	i = 1
	DO WHILE ( .not. valsyn .and. i .lt. 5 )
	    IF ( jstarr (4) .eq. lvalhr (i) ) valsyn = .true.
	    i = i + 1
	END DO
C
	IF ( .not. valsyn ) THEN
C
C*	    Since jstarr is not a valid synoptic time, assign a 
C*	    reasonable synoptic time based on ending valid time.
C*	
C	
            plus6 = .false.
C
	    IF ( jvehr .ge. 0. .and. jvehr .le. 3 ) THEN
                isbhr = 0
                 plus6 = .true.
            ELSE IF ( jvehr .gt. 21 .and.  jvehr .le. 24 ) THEN
	        isbhr = 0
	      ELSE IF ( jvehr .gt. 3 .and. jvehr .le. 6 ) THEN
	        isbhr = 6
	      ELSE IF ( jvehr .gt. 7 .and. jvehr .le. 9 ) THEN
	        isbhr = 6
                plus6 = .true.
	      ELSE IF ( jvehr .gt. 9 .and. jvehr .le. 12 ) THEN
	        isbhr = 12
	      ELSE IF ( jvehr .gt. 13 .and. jvehr .le. 15 ) THEN
	        isbhr = 12
                plus6 = .true.
	      ELSE IF ( jvehr .gt. 15 .and. jvehr .le. 18 ) THEN
	        isbhr = 18
	      ELSE IF ( jvehr .gt. 18 .and. jvehr .le. 21 ) THEN
	        isbhr = 18
                plus6 = .true.
	      ELSE
		iret = 1
		RETURN
	    END IF
C
C*          since gempak file has 30 slots available
C*          therefore should not care about losing data
C
            IF ( ntimes .lt. 24 .and. plus6 ) THEN
               isbhr = isbhr + 6
               IF (isbhr .eq. 24) isbhr = 0
            END IF
C
C*	    Offset the beginning time jstarr to closest synoptic time.
C
	    jsyofs = isbhr - jvehr
	    IF ( jsyofs .le. -22 ) jsyofs = jsyofs + maxhr
	    minuts = jsyofs * 60
	    IF ( jsyofs .gt. 0 ) THEN
		CALL TI_ADDM ( jstarr, minuts, jstarr, ier )
	      ELSE
		minuts = -minuts
		CALL TI_SUBM ( jstarr, minuts, jstarr, ier )
	    END IF
	  ELSE
C
C*	    Valid synoptic time: No further adjustment to jstarr.
C*	    Set isbhr to jvehr.
C
	    isbhr = jvehr
	    jsyofs = 0
	END IF
C
C*	Off set the starting and ending indices when necessary for
C*	the forecast data and set the new initial forecast time array
C*	at the new starting index.
C
	IF ( jsyofs .gt. 0 ) THEN
	    js24 = maxhr - jsyofs
	    IF ( ntimes .gt. js24 ) THEN
		misn = ntimes - js24
		ndxstr = 1 + misn
		minuts = misn * 60
		CALL TI_ADDM ( jrtarr, minuts, nrtarr, ier )
	      ELSE
		ndxstr = 1
		DO i = 1, 5
		    nrtarr (i) = jrtarr (i)
		END DO
	    END IF
	    ndxend = ntimes
	  ELSE 
	    ndxstr = 1
	    ndxend = ntimes + jsyofs
	    IF ( ndxend .lt. 1 ) THEN
		iret = 2
	    END IF
	    DO i = 1, 5
		nrtarr (i ) = jrtarr (i)
	    END DO
	END IF
C 
	RETURN
	END
