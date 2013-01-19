	SUBROUTINE BR_VISB ( strvs1, strvs2, autof, visby, visbk, visfl,
     +			     idecd, twofld, dirvis, iret )
C************************************************************************
C* BR_VISB                                                              *
C*                                                                      *
C* This subroutine will decode up to two METAR or TAF-formatted         *
C* visibility groups.                                                   *
C* 								        *
C* BR_VISB ( STRVS1, STRVS2, AUTOF, VISBY, VISBK, VISFL, IDECD, TWOFLD, *
C*	     DIRVIS, IRET )						*
C*								        *
C* Input parameters: 						        *
C*      STRVS1		CHAR*		Visibility field       		*
C*      STRVS2		CHAR*		Fractional part of visibility	*
C*	AUTOF		REAL		Automatic station flag		*
C*								        *
C* Output parameters:						        *
C*	VISBY 		REAL            Horizontal visibility (sm)      *
C*	VISBK 		REAL            Horizontal visibility (km)      *
C*	VISFL  		REAL            Visibility P or M indicator     *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  2 = auto report with field	*
C*						decoded having slashes	*
C*					  1 = field decoded     	*
C*					  0 = field not decoded		*
C*	TWOFLD		LOGICAL		Two field flag			*
C*	DIRVIS		LOGICAL		Directional visibility flag	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return		*
C*					  9 = miscoded field		*
C*	                                 -1 = no field found		*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	 9/02	Code taken from MT_VISB, no interface   *
C* Yen&Caruso Magee/NCEP 3/04   Handled AUTO reports sending '////' and *
C*				P or M indicator for vis in meters.(CSC)*
C* Caruso Magee/NCEP     9/05   Add code to handle 'VVVVNDV'            *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	strvs1, strvs2
	LOGICAL 	twofld, dirvis
C*
	CHARACTER  	strint*40, strvsb*40, comps*2, compp*1, compr*3
	LOGICAL 	numerc
C*
	INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	idecd  = 0
	twofld = .false.
	dirvis = .false.
	visby  = RMISSD
	visbk  = RMISSD
	visfl  = RMISSD
C
	ivsbun = IMISSD
	ivsflg = IMISSD
	vsby   = RMISSD
C
C
C*	First check for AUTO report which might have '////' for vis field.
C
	IF ( autof .eq. 0. .and. strvs1 (1:4) .eq. '////' ) THEN
            idecd  = 2
            iret = 0
            RETURN
        END IF
	CALL BR_VALD ( strvs1, 2, 6, 'SM', 1, iloc, iret )
	IF ( iret .eq. 0 ) THEN
C
C*	    We do not have a separate fractional part.
C
	    strint = '0'
	    strvsb = strvs1
	  ELSE IF ( iret .eq. -1 ) THEN
C
C*	    Check for possible fractional part - format is 'n n/nSM'.
C
	    strint = strvs1
	    CALL BR_VALD ( strvs2, 2, 6, 'SM', 1, iloc, iret )
	END IF
	IF ( iret .eq. 0 ) THEN
C 
C*	    Decode visibility.
C 
	    IF ( strint .ne. '0' ) THEN
	        strvsb = strvs2
		twofld = .true. 
	    END IF
	    kret = 0
C
	    llocb = 2
C
C*	    Check for preceding 'P' (+) or 'M' (-) and set flag field.
C
	    IF ( ( strint ( 1:1 ) .eq. 'M' ) .or.
     +           ( strvsb ( 1:1 ) .eq. 'M' ) ) THEN
	        ivsflg = 0
	      ELSE IF ( ( strint ( 1:1 ) .eq. 'P' ) .or.
     +                ( strvsb ( 1:1 ) .eq. 'P' ) ) THEN
	        ivsflg = 4
	      ELSE
	        llocb = 1
	    END IF
C
	    IF ( strint .ne. '0' ) THEN
C
C*		Decode presumed integer part of fraction.
C
	        lens = INDEX ( strint, ' ' ) - 1
		IF ( lens .lt. 0 ) lens = 40
	        CALL ST_INTG ( strint ( llocb:lens ), ival, jret )
		vsbyi = FLOAT ( ival )
		IF ( jret .eq. 0 ) THEN
	            llocb = 1
		  ELSE
C 
C*		    Skip field if presumed part is non-numeric.
C
		    vsbyi = 0.
		END IF
	      ELSE
	        vsbyi = 0.
	    END IF
C
	    ilocsl = INDEX ( strvsb, '/' )
	    IF ( ( ilocsl .gt. 0 ) .and. ( ilocsl .lt. iloc ) ) THEN
C
C*		We have a presumed fractional part.
C
C*		Check for misdecoded visibilities that lack a space
C*		between the integer and fractional parts ( e.g. "11/2" ).
C
		IF (  ( ilocsl - 2 ) .gt. 0 ) THEN 
		    CALL ST_INTG ( strvsb ( ilocsl-2:ilocsl-2), inum1,
     +				   jret )
		    IF ( inum1 .gt. 0 .and. inum1 .lt. 10 ) 
     +			vsbyi = FLOAT ( inum1 )
		END IF
C
	        CALL ST_INTG ( strvsb ( ilocsl-1:ilocsl-1), inum, jret )
	        CALL ST_INTG ( strvsb ( ilocsl+1:iloc-1 ), iden, jret )
		rnum = FLOAT ( inum )
		rden = FLOAT ( iden )
	        IF ( ERMISS ( vsbyi ) .or. ERMISS ( rnum ) .or.
     +	             ERMISS ( rden ) ) THEN
	            kret = 1
	          ELSE
C
C*		    Calculate visibility as sum of integer and fraction.
C
	            vsby = vsbyi + rnum / rden
	        END IF
	      ELSE
C
C*		Have no fractional part.
C
	        CALL ST_INTG ( strvsb ( llocb:iloc-1 ), ivsby, jret )
		vsby = FLOAT ( ivsby )
	        IF ( jret .ne. 0 ) kret = 1
	    END IF
C
C*	    Visibility bounds check.
C
	    IF ( kret .eq. 0 ) THEN
		IF ( ( vsby .lt. 0 ) .or. ( vsby .gt. 250. ) ) THEN
		    vsby = RMISSD
		    kret = 1
		END IF
	    END IF
C
	    idecd = 1
	    IF ( kret .eq. 0 ) ivsbun = 0
	    iret = kret
	  ELSE
C
C*	    No 'SM' visibility group found.
C*	    Check for non-U.S. visibility indicators (e.g., meters).
C
C*	    Check first for 'CAVOK'.
C	    
	    CALL BR_VALD ( strvs1, 1, 1, 'CAVOK', 1, iloc, kret )
	    IF ( kret .ge. 0 ) THEN
		ivsflg = 3
		vsby   = 10.
	      ELSE
C  
C*	        Check for visibility value in meters, with optional direc.
C
		lens = INDEX ( strvs1, ' ' ) - 1
		jvsflg = IMISSD
C
C*		Check for possible compass directions.
C
		IF ( lens. eq. 4  ) THEN
		    jvsflg = 2
		    llocb = 1
		  ELSE IF ( lens .eq. 5 ) THEN
C
C*	          Check for preceding 'P' (+) or 'M' (-) and set flag field.
C
	            IF ( strvs1 ( 1:1 ) .eq. 'M' ) THEN
	                jvsflg = 0
                        llocb = 2
	              ELSE IF ( strint ( 1:1 ) .eq. 'P' ) THEN
	                jvsflg = 4
                        llocb = 2
                      ELSE
                        llocb = 1
		        compp = strvs1 ( 5:5 )
		        IF ( compp .eq. 'N' ) THEN
			  jvsflg = 360
		         ELSE IF ( compp .eq. 'E' ) THEN
			  jvsflg = 90
		         ELSE IF ( compp .eq. 'S' ) THEN
			  jvsflg = 180
		         ELSE IF ( compp .eq. 'W' ) THEN
			  jvsflg = 270
		        END IF
		    END IF
		  ELSE IF ( lens .eq. 6 ) THEN
		    llocb = 1
		    comps = strvs1 ( 5:6 )
		    IF ( comps .eq. 'NE' ) THEN
			jvsflg = 45
		      ELSE IF ( comps .eq. 'SE' ) THEN
			jvsflg = 135
		      ELSE IF ( comps .eq. 'SW' ) THEN
			jvsflg = 225
		      ELSE IF ( comps .eq. 'NW' ) THEN
			jvsflg = 315
		    END IF
		  ELSE IF ( lens .eq. 7 ) THEN
		    llocb = 1
		    compr = strvs1 ( 5:7 )
                    IF ( compr .eq. 'NDV' ) THEN
C
C*                    No directional variations (NDV) can be given. Treat
C*                    as if just VVVV had been encoded.
C*
C*		      NDV was removed in 2010. However, it remains here
C*		      so that we can decode old data.
C
                      jvsflg = 2
                    END IF
		END IF
		IF ( jvsflg .ne. IMISSD ) THEN
C
C*             	    Check for numeric string of length 4.
C
        	    numerc = .true.
        	    i = llocb
        	    DO WHILE ( numerc )
            	        CALL ST_ALNM ( strvs1 ( i:i ), ityp, lret )
            	        IF ( ityp .ne. 1 ) THEN
                	    numerc = .false.
            	        ELSE
                	    i = i + 1
                	    IF ( i .gt. ( llocb+3 ) ) numerc = .false.
            	        END IF
        	    END DO
C
C*                  Storing the visibility if reported in KM.
C
		    IF ( strvs1 ( lens-1:lens ) .eq. 'KM' ) THEN
		        CALL ST_INTG ( strvs1 ( 1:lens-2 ), ival, kret )
		        vsby = FLOAT ( ival )
		    END IF
        	    IF ( i .gt. ( llocb+3 ) ) THEN
C
C*          		String is numeric - proceed with decode.
C
			CALL ST_INTG ( strvs1 ( llocb:llocb+3 ), ival, kret )
			val = FLOAT ( ival )
		        IF ( .not. ERMISS ( val ) ) THEN
C
C*			    Store visibility in kilometers and store
C*			    compass direction if found.
C
			    IF ( val .eq. 9999. ) THEN
				vsby = 10.
				jvsflg = 3
			      ELSE
			        vsby = val * .001
			    END IF
			    IF ( jvsflg .ge. 0 ) THEN
			        ivsflg = jvsflg
		                IF ( jvsflg .gt. 10 ) dirvis = .true.
			    END IF
			END IF
		    END IF
		END IF
	    END IF
	    IF ( kret .ge. 0 ) THEN
		ivsbun = 1
		idecd  = 1
	    END IF
	END IF
	iret = kret
	IF ( iret .gt. 0 ) iret = 9
	IF ( ivsbun .eq. 0 ) THEN
	    visby = vsby
	  ELSE IF ( ivsbun .eq. 1 ) THEN
	    visbk = vsby
	END IF
	IF ( ivsflg .ne. IMISSD )  visfl = FLOAT ( ivsflg )
C*
	RETURN
	END
