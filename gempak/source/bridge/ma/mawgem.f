	SUBROUTINE MA_WGEM ( gemfil, stntbl, iadstn, maxtim, cprms,
     +                       imnem, numprm, lunf, iret )
C************************************************************************
C* MA_WGEM                                                              *
C*                                                                      *
C* This subroutine retrieves interface-stored data, converts it into    *
C* GEMPAK output, and then writes the GEMPAK output to the GEMPAK       *
C* output file.                                                         *
C* 								        *
C* MA_WGEM ( GEMFIL, STNTBL, IADSTN, MAXTIM, CPRMS, IMNEM, NUMPRM,      *
C*	     LUNF, IRET)                                                *
C*								        *
C* Input parameters:                                                    *
C*	GEMFIL	       CHAR*	      Output file name template         *
C*	STNTBL	       CHAR*	      Marine station table              *
C*	IADSTN	       INTEGER	      Number of additional stations     *
C*	MAXTIM	       INTEGER	      Number of times allowed           *
C*	CPRMS (*)      CHAR*	      GEMPAK parms chosen for output    *
C*	IMNEM (*)      INTEGER        Subscript mapping, data to GEMPAK *
C*	NUMPRM	       INTEGER	      Count of chosen GEMPAK parameters *
C*								        *
C* Output parameters:						        *
C*	LUNF	       INTEGER	      Unit number for GEMPAK file       *
C*      IRET           INTEGER        Return code                       *
C*                                      0 = Normal return               *
C*                                     -1 = Report time invalid         *
C*                                     -2 = GEMPAK file problem         *
C*                                     -4 = Bad report id               *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP       6/96                                           *
C* D. Kidwell/NCEP	 4/97	Changed interface, cleaned up code      *
C* D. Kidwell/NCEP	 5/97	Added sig wave height computation       *
C* D. Kidwell/NCEP	10/97	Changed interface; added call to MA_GEMP*
C* D. Kidwell/NCEP	 4/99	Included check for bad report id        *
C* D. Kidwell/NCEP	10/99	Converted auto weather to manned equiv. *
C* D. Kidwell/NCEP	 4/00	Added lunf to calling sequence          *
C* F. J. Yen/NCEP	 4/01	Added computations for Coast Guard data *
C* D. Kidwell/NCEP	 3/02	Removed P06M; GUMS->GUST;added ship mvmt*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
	CHARACTER*(*)	gemfil, stntbl, cprms (*)
	INTEGER		imnem (*)
C*
	REAL		rdata (MMPARM), cloudb(0:9), shpdir(0:9),
     +			shpspd(0:9)
        CHARACTER	rpttim*12, parms(MMPARM)*4,
     +			coun*4, state*4, stid*8
	LOGICAL		cgdata, ship
C*
	SAVE		nparm, parms
C*
	INCLUDE 	'ERMISS.FNC'
C*
	DATA 		cloudb /    0.,   50.,  100.,  200.,  300.,
     +				  600., 1000., 1500., 2000., 2500. /
	DATA		shpdir / RMISSD, 225., 270., 315.,   0.,  45.,
     +				    90., 135., 180., RMISSD /
	DATA		shpspd /  0.,  3.,  8., 13., 18., 23., 28., 33.,
     +				 38., 43. /
C-----------------------------------------------------------------------
        iret = 0
C
C*	Open GEMPAK file.
C
	CALL MA_GEMP ( gemfil, stntbl, iadstn, maxtim, rpttim, lunf, 
     +		       nparm, parms, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Initialize GEMPAK output array.
C
        DO i = 1, MMPARM
	    rdata ( i ) = RMISSD
        END DO
C
	iwhgt = IMISSD
	howw  = RMISSD
	hosw  = RMISSD
C
C*	Do not create GEMPAK output if the latitude or longitude is
C*	missing.
C
	IF  (  ( ERMISS ( rivals ( irslat ) ) )  .or.
     +	       ( ERMISS ( rivals ( irslon ) ) )  )  RETURN
	slat = rivals ( irslat )
	slon = rivals ( irslon )
C
C*	Get the elevation.
C
        selv = rivals ( irselv )
C
C*	Set the report identifier and check that it is alphanumeric.
C
	stid = civals ( icstid ) 
	CALL ST_LSTR ( stid, lens, ier )
	IF ( lens .eq. 0 ) RETURN
	DO i = 1, lens
	    CALL ST_ALNM ( stid ( i:i ), ityp, ier )
	    IF ( ityp .eq. 0 ) THEN
		iret = -4
		CALL DC_WLOG ( 2, 'MA', iret, stid, ier ) 
		RETURN
	    END IF
	END DO
C
C*	Determine if this is a ship.
C
	CALL MA_SHIP ( ship, ier )
C
        DO  k = 1, numprm
C
C*	    Check for the parameter in the list.
C
            CALL ST_FIND  ( cprms ( k ), parms, nparm, ilc, ier )
C
            IF ( ilc .eq. 0 )  THEN
		CALL DC_WLOG ( 2, 'DCMSFC', -3, cprms(k), ier )
	      ELSE   
C
C*		Retrieve the requested data and store it into the
C*		GEMPAK array.
C
	   	IF ( imnem ( k ) .ne. 0 ) THEN
      		    rdata ( ilc ) = rivals ( imnem ( k ) )
		    IF ( ibrtyp .eq. 4 ) THEN
C
C*			Do calculations for Coast Guard Data.
C
			IF ( cprms ( k ) .eq. 'SPED' ) THEN
C
C*			    Convert wind speed units from knots
C*			    to m/sec if necessary.
C
			    IF ( ERMISS ( rdata ( ilc ) ) )
     +			      rdata ( ilc ) =
     +					PR_KNMS ( rivals (irsknt) )
			  ELSE IF ( cprms ( k ) .eq. 'TMPC' ) THEN
C
C*			    Convert temperature from Fahrenheit
C*			    to Celsius if necessary.
C
			    IF ( ERMISS ( rdata ( ilc ) ) )
     +			      rdata ( ilc ) =
     +					PR_TMFC ( rivals (irtmpf) )
			  ELSE IF ( cprms ( k ) .eq. 'VSBK' ) THEN
C
C*                          Convert visibility from statute miles
C*                          to kilometers if necessary.
C
			    IF ( ERMISS ( rdata ( ilc ) ) ) THEN
				v1 = PR_HGSF ( rivals (irvsby) )
				v2 = PR_HGFM ( v1 )
				rdata ( ilc ) = PR_HGMK ( v2 )
			    END IF
			  ELSE IF ( cprms ( k ) .eq. 'SSTC' ) THEN
C
C*                          Convert temperature from Fahrenheit 
C*                          to Celsius if necessary.
C
			    IF ( ERMISS ( rdata ( ilc ) ) )
     +			      rdata ( ilc ) =
     +					PR_TMFC ( rivals (irsstf) )
			  ELSE IF ( cprms ( k ) .eq. 'CBAS' ) THEN
C
C*			    HOCB is in meters.  Convert to CBAS
C*			    using WMO Code 1600.
C
			    i = 9
			    DO WHILE ( ERMISS ( rdata ( ilc ) ) .and.
     +				       i .gt. -1 )
				IF ( rivals (irhocb(1)) .ge.
     +				           cloudb (i) ) THEN
				    rdata ( ilc ) = i	
				END IF
				i = i - 1
			    END DO
			END IF
		      ELSE
C
C*			Do calculations for non-Coast Guard Data.
C*		        First, save values required for wave height
C*			calculation.
C
		        IF ( cprms ( k ) .eq. 'WHGT' ) THEN
			    iwhgt =  ilc 
		          ELSE IF ( cprms ( k ) .eq. 'HOWW' ) THEN
			    howw  = rdata ( ilc )
		          ELSE IF ( cprms ( k ) .eq. 'HOSW' ) THEN
			    hosw  = rdata ( ilc ) 
C
C*			  Check for missing wind direction interface
C*			  value.
C
		          ELSE IF ( cprms ( k ) .eq. 'DRCT' ) THEN
			    IF ( rdata ( ilc ) .eq. 99. ) 
     +			         rdata ( ilc ) = RMISSD
C
C*                        Check for automated present or past weather,
C*                        and convert to manned equivalent.
C
                          ELSE IF ( cprms ( k ) .eq. 'WWMO' ) THEN
                            IF ( .not. ERMISS ( rivals ( irwwma ) ) )
     +                        rdata ( ilc ) =
     +					PR_WMAO ( rivals ( irwwma ) )
                          ELSE IF ( cprms ( k ) .eq. 'PWWM' ) THEN
                            IF ( .not. ERMISS ( rivals ( irpwwa ) ) )
     +                        rdata ( ilc ) =
     +					PR_PWAO ( rivals ( irpwwa ) )
C
C*			  Check for wind gust and convert from m/sec
C*			  to knots.
C
			  ELSE IF ( cprms ( k ) .eq. 'GUST' ) THEN
			    rdata ( ilc ) = PR_MSKN ( rivals (irgums) )
C
C*			  Check for ship's movement and convert from 
C*			  WMO code table 0700 (direction) and 4451
C*			  (speed).  Do not store stationary data for
C*			  fixed platforms.  The ship direction will be
C*			  the direction from which the ship is moving.
C
			  ELSE IF ( cprms ( k ) .eq. 'SHPD' ) THEN
			    rdata ( ilc ) = RMISSD
			    itdmp = NINT ( rivals ( irtdmp ) )
      			    IF ( ship .or. ( itdmp .gt. 0 ) ) THEN
			        IF ( ( itdmp .ge. 0 ) .and.
     +				     ( itdmp .le. 9 ) )
     + 				       rdata ( ilc ) = shpdir ( itdmp )
			    END IF
			  ELSE IF ( cprms ( k ) .eq. 'SHPK' ) THEN
			    rdata ( ilc ) = RMISSD
			    iasmp = NINT ( rivals ( irasmp ) )
      			    IF ( ship .or. ( iasmp .gt. 0 ) ) THEN
			        IF ( ( iasmp .ge. 0 ) .and.
     +				     ( iasmp .le. 9 ) )
     + 				       rdata ( ilc ) = shpspd ( iasmp )
			    END IF
		        END IF
		    END IF
		END IF
	    END IF
        END DO
C
C*	Calculate significant wave height from wind wave height and
C*	swell wave height, if not already decoded.
C
	IF ( iwhgt .ne. IMISSD ) THEN
	    IF ( ERMISS ( rdata ( iwhgt ) ) ) THEN
	        IF ( ERMISS ( howw ) .or. ERMISS ( hosw ) ) THEN
	          ELSE
		    rdata ( iwhgt ) = SQRT ( ( howw**2 ) + ( hosw**2 ) )
		END IF
	    END IF
	END IF
C
C*	Check for presence of data (other than TOST) for Coast Guard rpt.
C
	IF ( ibrtyp .eq. 4 ) THEN
	    CALL ST_FIND ( 'TOST', parms, nparm, ilc, ier )
	    IF ( ilc .eq. 0 ) THEN
		CALL DC_WLOG (2, 'DCMSFC', -3, 'TOST', ier )
	    END IF 
	    cgdata = .false.
	    i = 1
	    DO WHILE ( .not. cgdata .and. i .le. numprm )
		IF ( rdata (i) .ne. RMISSD ) THEN
		    IF ( i .ne. ilc ) cgdata = .true. 
		END IF
		i = i + 1
	    END DO 
	    IF ( .not. cgdata ) RETURN
	END IF 
C
C*      Initialize following parameters to not available.
C
        coun   = ' '
        state  = ' '
        istnum = 0
C
C*      Get hours and minutes of report.
C 
        ihhmm = 100 * irptdt (4) + irptdt (5)
C
C*	If all-numeric ID, set istnum = ID so individual
C*	stations can be listed with SFLIST.
C
	CALL ST_NUMB ( stid, istid, ier )
	IF ( ier .eq. 0 ) istnum = istid
C
C*	Write the decoded report data to GEMPAK marine data file.
C
        CALL SF_WSDD  ( lunf, rpttim, stid, istnum, slat, slon,
     +                  selv, state, coun, ihhmm, rdata, ier )
        IF ( ier .ne. 0 ) THEN
C
C*	    Write an error if GEMPAK file write error.
C
           CALL DC_WLOG ( 2, 'SF', ier, ' ', ierr )
        END IF
C*
	RETURN
	END
