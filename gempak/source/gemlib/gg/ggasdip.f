	SUBROUTINE GG_ASDIP (lunf, dattm2, datfil, datgrp, ntimes, 
     +			     itminc, icolrs, tlimit, mode, depdest,
     +                       sites, iret )
C************************************************************************
C* GG_ASDIP                                                             *
C*                                                                      *
C* This subroutine reads and plots ASDI tracking data from a single     *
C* file, using specified colors and time and/or height increments.      *
C*                                                                      *
C* GG_ASDIP ( LUNF, DATTM2, DATFIL, DATGRP, NTIMES, ICOLRS, TLIMIT,     *
C*            MODE, DEPDEST, SITES, IRET )                              *
C*                                                                      *
C* Input parameters:                                                    *
C*	LUNF		INTEGER		Logical unit number for input   *
C*	DATTM2		CHAR*		GEMPAK ending time for data     *
C*	DATFIL		CHAR*		GEMPAK time for current data    *
C*	DATGRP(*)	CHAR*		GEMPAK break times for colors   *
C*	NTIMES		INTEGER 	Number of break times           *
C*	ICOLRS(*)	INTEGER		Color for each time increment   *
C*      TLIMIT          INTEGER         Time Length Limit to Tracks     *
C*      MODE            CHARACTER*      T for Time mode, H for Height   *
C*      DEPDEST         CHARACTER*      D for Departure, A for Arrival  *
C*                                      B for both                      *
C*	SITES           CHAR*           The SITE ID, a semi-colon       *
C*                                      separated list of SITE IDs, or  *
C*                                      ALL to plot ALL sites.          *
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* L. Hinson/AWC        05/12                                           *
C************************************************************************ 
	INCLUDE 'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattm2, datfil, datgrp (*)
	INTEGER         itminc (*)
	INTEGER		icolrs (*)
        INTEGER         tlimit
        CHARACTER*(*)   mode
        CHARACTER*(*)   depdest
	CHARACTER*(*)   sites
        INTEGER         ht
C*
	PARAMETER	( MAXSTK = LLMXPT * 2 )
C*
	CHARACTER	datmin*20

	REAL            lats (LLMXPT), lons (LLMXPT)
        INTEGER         ages(LLMXPT), hts (LLMXPT)
	INTEGER		itarr(5), jtarr(5)
	
	CHARACTER       buffer*100
	CHARACTER       carr (11)*20
        CHARACTER       darr (5)*10
	CHARACTER       sarr (30)*5
	CHARACTER*(20)  key
	INTEGER         numpts, age1, age2, numd, nums
	LOGICAL         found
C-----------------------------------------------------------------------
        ier = 0
	found = .false.
	CALL GQCOLR (jcolr, ier)
	CALL GQLINE (jtype, jtyhw, jwidth, jwdhw, ier)
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GQTURB ( szturb, jtuwid, ier )
	CALL GQICNG ( szicng, jicwid, ier )
	CALL GQSPCL ( szsswn, jswwid, ier )
        CALL GQMRKR ( jmark, jmkhw, szmark, jkmwid, ier )
	iostat = 0
	iltyp = 1
	ilwidth = 1
	
C*      Loop on the records in the file...
        DO WHILE (iostat .eq. 0)
	  READ ( lunf, 2, IOSTAT = iostat ) buffer
2	  FORMAT ( A )
	  IF ( iostat .eq. 0) THEN
	    CALL ST_CLST (buffer, '|', ' ', 3, carr, num, ier )
      	    key = carr(1)
            CALL ST_CLST (key, ':', ' ', 3, darr, numd, ier )
C           Break up sites to list
            IF (sites .eq. "ALL" ) THEN
	      found = .true.
	    ELSE 
              CALL ST_CLST (sites, ';', ' ', 30, sarr, nums, ier )
	      found = .false.
	      CALL ST_FIND (darr(1), sarr, 30, ipos, iret )
	      IF ( ipos .eq. 0 ) THEN
	        CALL ST_FIND (darr(1)(2:4), sarr, 30, ipos, iret )
	        IF (ipos .gt. 0) THEN
	          found = .true.
	        END IF
	      ELSE
	        found = .true.
	      END IF
	    END IF
            IF ( (darr(3) .eq. depdest .or. depdest .eq. "B" .or.
     +          darr(3)(1:1) .eq. ' ') .and. found ) THEN
     
	      CALL ST_NUMB ( carr ( 2 ), numpts, ier )
C*            Read the lat/lon coordinates from the file.
              jostat = 0

              DO ii = 1, numpts
	        READ ( lunf, 2, IOSTAT = jostat ) buffer
                IF ( jostat .eq. 0) THEN
                  CALL ST_CLST (buffer, '|', ' ', 4, carr, num, ier )
		  CALL ST_NUMB ( carr (1), ages(ii), ier)
		  CALL ST_CRNM ( carr (2), lats(ii), iret )
		  CALL ST_CRNM ( carr (3), lons(ii), iret )
                  CALL ST_NUMB ( carr (4), hts(ii), iret )
                END IF
              END DO
              CALL GG_ASDIQC(key, numpts, ages, lats, lons, hts, ier)
              IF ( ier .eq. 0 ) THEN
                DO ii = 1, numpts
                  IF (ii .eq. 1) THEN
                    lats(1) = lats(ii)
                    lons(1) = lons(ii)
                    age1 = ages(ii)
                    ht = hts(ii)
                    IF (numpts .eq. 1 .and. age1 .le. tlimit ) THEN
                      ic = 1
                      CALL GSCOLR ( ic , ier )
                      CALL GSMRKR ( 1, 0, 0.8, 1, ier )
                      CALL GMARK ( 'M', numpts, lats(1), lons(1),
     +                             ier )
                    END IF                       
                  ELSE
                    lats(2) = lats(ii)
                    lons(2) = lons(ii)
                    age2 = ages(ii)
                    ht = hts(ii)

                    ic = 1
		    IF (mode .eq. 'T') THEN
                      CALL TI_CTOI (dattm2, itarr, ier)
                      CALL TI_SUBM (itarr, age2, jtarr, ier)
                      CALL TI_ITOC (jtarr, datmin, ier )
		      CALL TI_MTCH ( 2, datmin, datgrp, ntimes, 0, iposn, 
     +			       ier )
		      idx = ntimes - iposn
		      IF ( ( idx .eq. 0 ) .and. 
     +		       ( datmin .le. dattm2 )  ) idx = 1
		      IF ( ( idx .gt. 0 ) .and. ( idx .lt. ntimes ) ) THEN
		         ic = icolrs ( idx )
                         CALL GSCOLR ( ic, ier )
                         CALL GSLINE ( iltyp, 0, ilwidth, 0, ier )
		         CALL GLINE ( 'M', 2, lats, lons, ier )
		      ENDIF
                    ELSE IF (mode .eq. 'H') THEN
                      IF (ht .eq. 0) THEN
                         ic = 1
                      ELSE 
                        IF (ht .gt. 0 .and. ht .lt. itminc(1)) THEN
                          ic = icolrs(1)
                        ELSE 
                          DO jj = 1, ntimes - 1
                            IF (ht .gt. itminc(jj)) THEN
                              ic = icolrs (jj+1)
                            END IF
                          END DO
                        END IF
                      END IF

                      IF (age2 .lt. tlimit) THEN
                        CALL GSCOLR ( ic, ier )
                        CALL GSLINE ( iltyp, 0, ilwidth, 0, ier )
		        CALL GLINE ( 'M', 2, lats, lons, ier )
                      END IF
                    END IF
		    lats(1) = lats(2)
		    lons(1) = lons(2)                    
                  END IF
                END DO
              END IF
            ELSE
              CALL ST_NUMB ( carr ( 2 ), numpts, ier )
              DO ii = 1, numpts
                READ (lunf, 2, IOSTAT = jostat ) buffer
              END DO
            END IF
	  END IF
	END DO
	CALL FL_CLOS (lunf, ier )
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GSTURB ( szturb, jtuwid, ier )
	CALL GSICNG ( szicng, jicwid, ier )
	CALL GSSPCL ( szsswn, jswwid, ier )
        CALL GSMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
C*	
	RETURN
	
	END


	
