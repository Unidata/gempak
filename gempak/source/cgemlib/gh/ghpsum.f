	SUBROUTINE GH_PSUM ( advise, lenw, slat, slon,
     +                       wdir, wsped, wpres, wwnd, gust, iret )
C************************************************************************
C* GH_PSUM 								*
C*									*
C* This subroutine decodes the SUMMARY Section of a single public     *
C* advisory report.      						*
C*                                                                      *
C* GH_PSUM ( ADVISE, LENW,  SLAT, SLON, WDIR, WSPED,                    *
C*           WPRES, WWND, IRET )                                        *
C*									*
C* Input parameters:							*
C*	ADVISE		CHAR*		Public advisory bulletin	*
C*	LENW		INTEGER		Length of bulletin              *
C*									*
C* Output parameters:							*
C*	SLAT 		REAL		Current Latitude                *
C*	SLON 		REAL		Current Longitude               *
C*	WDIR 		CHAR*		Direction of storm movement(deg)*
C*	WSPED 		CHAR*		Speed of movement (knots)	*
C*	WPRES 		CHAR*		Minimum central pressure (mb)   *
C*	WWND		CHAR*		Max sustained wind		*
C*	GUST		CHAR*		Wind gust (knots)		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = could not find REPEAT     *
C*                                            section                   *
C*									*
C**									*
C* Log:									*
C* C. Lauer/NHC		 3/09		Modified from GH_PRPT to read   *
C*                                      new public advisory format      *
C* X. Guo/CWS           02/10           Changed codes to support the    *
C*                                      new public format               *
C* S. Jacobs/NCEP	 3/12		Fixed STATIONARY motion value	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	advise, wdir, wsped, wpres, wwnd, gust
	REAL            slat, slon
C*
	CHARACTER	carr (10)*20 
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
        irpt = 0
C
C*	Search for beginning of REPEAT section
C
	irpt = INDEX( advise( :lenw), 'SUMMARY OF') 
	IF ( irpt .eq. 0 ) THEN
	    iret = -1
	    RETURN
	ENDIF
C
C*	Search for the center location of storm.
C
	ipos = INDEX( advise(irpt:lenw), 'LOCATION...') 
	irpt = irpt + ipos + 10	
	ilaf = INDEX( advise(irpt:lenw), 'N' )
	isgn = 1	
	IF ( (ilaf .gt. 10) .OR. ( ilaf .eq. 0 ) ) THEN
	   ilaf = INDEX (advise(irpt:lenw), 'S' )
	   isgn = -1	   
	ENDIF	
	IF ( ilaf .gt. 10 ) THEN
	    ilaf = 15
	ENDIF	
	ilaf = irpt + ilaf - 2	
	CALL ST_CRNM ( advise(irpt:ilaf), slat, ier)
	IF (ier .eq. 0) THEN
	    slat = slat * isgn	    
	ENDIF	
C
C*      Longitude
C
	irpt = ilaf + 2		
	DO i = 1, 10
	    CALL ST_ALNM( advise(irpt:irpt), ityp, iret )
	    IF ( ityp .eq. 1 ) exit
	    irpt = irpt + 1
	END DO	
	ilaf = INDEX( advise(irpt:lenw), 'W' )
	isgn = -1	
	IF ( (ilaf .gt. 10) .or. ( ilaf .eq. 0 ) ) THEN
	   ilaf = INDEX (advise(irpt:lenw), 'E' )
	   isgn = 1	   
	ENDIF	
	IF ( ilaf .gt. 10 ) THEN
	    ilaf = 10
	ENDIF	
	ilaf = irpt + ilaf - 2	
	CALL ST_CRNM ( advise(irpt:ilaf), slon, ier)
	IF (ier .eq. 0) THEN
	    slon = slon * isgn	    
	ENDIF
C
C*	Search for the max sustained wind speed
C
	ipos = INDEX( advise(irpt:lenw), 'MAXIMUM SUSTAINED WINDS...') 
        irpt = irpt + ipos + 21	
	DO i = 1, 10
	    CALL ST_ALNM( advise(irpt:irpt), ityp, iret )
	    IF ( ityp .eq. 1 ) exit
	    irpt = irpt + 1	    
	END DO	
	ilaf = INDEX( advise(irpt:lenw), 'MPH' )	
	ilaf = irpt + ilaf - 2
	CALL ST_CRNM ( advise(irpt:ilaf), rspd, ier )
	rspd = PR_MHKN ( rspd )
C
C*	Round the wind speed to the nearest 5
C
	irspd = NINT ( rspd / 5.0 ) * 5
	CALL ST_RLCH ( rspd, 0, wwnd, ier )
C
C*	Calculate the wind gust based on the sustained wind speed
C
        IF ( irspd .eq. 20 ) THEN
              igust = 5
           ELSE IF ( irspd .ge. 25  .and. irspd .le. 55  ) THEN 
              igust = 10
           ELSE IF ( irspd .ge. 60  .and. irspd .le. 75  ) THEN
              igust = 15
           ELSE IF ( irspd .ge. 80  .and. irspd .le. 100 ) THEN
              igust = 20
           ELSE IF ( irspd .ge. 105 .and. irspd .le. 120 ) THEN
              igust = 25
           ELSE IF ( irspd .ge. 125 .and. irspd .le. 145 ) THEN
              igust = 30
           ELSE IF ( irspd .ge. 150 .and. irspd .le. 170 ) THEN
              igust = 35
           ELSE IF ( irspd .eq. 170 ) THEN
              igust = 40
           ELSE
              igust = IMISSD
        END IF
        igust = irspd + igust
	CALL ST_INCH ( igust, gust, ier )
C
C*	Search for the storm movement direction
C
	ipos = INDEX( advise(irpt:lenw), 'PRESENT MOVEMENT...') 
	irpt = irpt + ipos + 18
	CALL ST_CLST ( advise (irpt:lenw), ' ', ' ', 10, carr,
     +			   num, ier )
	IF ( carr(5) .EQ. 'NEAR' .OR. carr(5) .EQ. 'AT' ) THEN
	   wdir = ' '
	   IF ( carr(1) .eq. 'N' ) THEN
	       wdir = '0.0'
	   ELSE IF ( carr(1) .eq. 'NNE' ) THEN
	       wdir = '22.5'
	   ELSE IF ( carr(1) .eq. 'NE' ) THEN
	       wdir = '45.0'
	   ELSE IF ( carr(1) .eq. 'ENE' ) THEN
	       wdir = '67.5'
	   ELSE IF ( carr(1) .eq. 'E' ) THEN
	       wdir = '90.0'
	   ELSE IF ( carr(1) .eq. 'ESE' ) THEN
	       wdir = '112.5'
	   ELSE IF ( carr(1) .eq. 'SE' ) THEN
	       wdir = '135.0'
	   ELSE IF ( carr(1) .eq. 'SSE' ) THEN
	       wdir = '157.5'
	   ELSE IF ( carr(1) .eq. 'S' ) THEN
	       wdir = '180.0'
	   ELSE IF ( carr(1) .eq. 'SSW' ) THEN
	       wdir = '202.5'
	   ELSE IF ( carr(1) .eq. 'SW' ) THEN
	       wdir = '225.0'
	   ELSE IF ( carr(1) .eq. 'WSW' ) THEN
	       wdir = '247.5'
	   ELSE IF ( carr(1) .eq. 'W' ) THEN
	       wdir = '270.0'
	   ELSE IF ( carr(1) .eq. 'WNW' ) THEN
	       wdir = '292.5'
	   ELSE IF ( carr(1) .eq. 'NW' ) THEN
	       wdir = '315.0'
	   ELSE IF ( carr(1) .eq. 'NNW' ) THEN
	       wdir = '337.5'
	   ENDIF
	   CALL ST_CRNM ( carr(6), rspd, ier )
	   IF ( carr(7)(1:3) .eq. 'MPH' ) THEN
	       rspd = PR_MHKN ( rspd )
	       CALL ST_RLCH ( rspd, 2, wsped, ier )
	   ELSE IF ( carr(7)(1:5) .eq. 'KM/HR' ) THEN
	       rspd = PR_MSKN ( rspd * 1000. / 3600. )
	       CALL ST_RLCH ( rspd, 2, wsped, ier )
	   ELSE
	       wsped = '-9999'
	   ENDIF

	ELSE
	   IF ( carr(1) .eq. 'STATIONARY' ) THEN
	       wdir = ' '
	       wsped = '0'
	   ELSE
	       wdir = ' '
	       wsped = '-9999'
	   ENDIF
	ENDIF	
C
C*	Search for the central pressure
C
	ipos = INDEX( advise(irpt:lenw), 'MINIMUM CENTRAL PRESSURE...')
	irpt = irpt + ipos + 26
	CALL ST_CLST ( advise (irpt:lenw), ' ', ' ', 2, carr,
     +			   num, ier )
	wpres = carr(1)
C*
	RETURN
	END
