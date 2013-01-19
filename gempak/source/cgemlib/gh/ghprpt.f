	SUBROUTINE GH_PRPT ( advise, lenw, slat, slon,
     +                       wdir, wsped, wpres, wwnd, gust, iret )
C************************************************************************
C* GH_PRPT 								*
C*									*
C* This subroutine decodes the REPEATING Section of a single public     *
C* advisory report.      						*
C*                                                                      *
C* GH_PRPT ( ADVISE, LENW,  SLAT, SLON, WDIR, WSPED,                    *
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
C* S. Gilbert/NCEP	 1/06						*
C* S. Gilbert/NCEP	 6/06		Several fixes decoding section  *
C* m.gamazaychikov/SAIC	10/07		Add rounding up to nearest 5 knt*
C* T. Piper/SAIC	12/07	Removed errant semicolon		*
C* m.gamazaychikov/SAIC	10/07		Removed round up to nearest 5knt*
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
	irpt = INDEX( advise( :lenw), 'REPEATING') 
	IF ( irpt .eq. 0 ) THEN
	    iret = -1
	    RETURN
	ENDIF
C
C*	Search for the center location of storm.
C
	ipos = INDEX( advise(irpt:lenw), 'POSITION...') 
	irpt = irpt + ipos + 10
	CALL ST_CLST ( advise (irpt:lenw), ' ', ' ', 2, carr,
     +			   num, ier )
C
C*      Latitude
C
	CALL ST_CRNM ( carr(1), slat, ier )
	IF ( ier .eq. 0 )  THEN
	    IF ( carr(2) .eq. 'S...' ) slat = -slat
	ENDIF
C
C*      Longitude
C
	ipos = INDEX( advise(irpt:lenw), '...') 
	irpt = irpt + ipos + 2
	CALL ST_CLST ( advise (irpt:lenw), ' ', ' ', 2, carr,
     +			   num, ier )
	CALL ST_CRNM ( carr(1), slon, ier )
	IF ( ier .eq. 0 )  THEN
	    IF ( carr(2) .eq. 'W.' ) slon = -slon
	ENDIF
C
C*	Search for the storm movement direction
C
	ipos = INDEX( advise(irpt:lenw), 'TOWARD...') 
	irpt = irpt + ipos + 8
	CALL ST_CLST ( advise (irpt:lenw), ' ', ' ', 10, carr,
     +			   num, ier )
	IF ( carr(2) .EQ. 'NEAR' .OR. carr(2) .EQ. 'AT' ) THEN
	   wdir = ' '
	   IF ( carr(1) .eq. 'NORTH' ) THEN
	       wdir = '0.0'
	   ELSE IF ( carr(1) .eq. 'NORTH-NORTHEAST' ) THEN
	       wdir = '22.5'
	   ELSE IF ( carr(1) .eq. 'NORTHEAST' ) THEN
	       wdir = '45.0'
	   ELSE IF ( carr(1) .eq. 'EAST-NORTHEAST' ) THEN
	       wdir = '67.5'
	   ELSE IF ( carr(1) .eq. 'EAST' ) THEN
	       wdir = '90.0'
	   ELSE IF ( carr(1) .eq. 'EAST-SOUTHEAST' ) THEN
	       wdir = '112.5'
	   ELSE IF ( carr(1) .eq. 'SOUTHEAST' ) THEN
	       wdir = '135.0'
	   ELSE IF ( carr(1) .eq. 'SOUTH-SOUTHEAST' ) THEN
	       wdir = '157.5'
	   ELSE IF ( carr(1) .eq. 'SOUTH' ) THEN
	       wdir = '180.0'
	   ELSE IF ( carr(1) .eq. 'SOUTH-SOUTHWEST' ) THEN
	       wdir = '202.5'
	   ELSE IF ( carr(1) .eq. 'SOUTHWEST' ) THEN
	       wdir = '225.0'
	   ELSE IF ( carr(1) .eq. 'WEST-SOUTHWEST' ) THEN
	       wdir = '247.5'
	   ELSE IF ( carr(1) .eq. 'WEST' ) THEN
	       wdir = '270.0'
	   ELSE IF ( carr(1) .eq. 'WEST-NORTHWEST' ) THEN
	       wdir = '292.5'
	   ELSE IF ( carr(1) .eq. 'NORTHWEST' ) THEN
	       wdir = '315.0'
	   ELSE IF ( carr(1) .eq. 'NORTH-NORTHWEST' ) THEN
	       wdir = '337.5'
	   ENDIF
	   CALL ST_CRNM ( carr(3), rspd, ier )
	   IF ( carr(4)(1:3) .eq. 'MPH' ) THEN
	       rspd = PR_MHKN ( rspd )
	   ELSE IF ( carr(4)(1:5) .eq. 'KM/HR' ) THEN
	       rspd = PR_MSKN ( rspd * 1000. / 3600. )
	   ENDIF
	   CALL ST_RLCH ( rspd, 2, wsped, ier )
	ELSE
	   wdir = ' '
	   wsped = '-9999'
	ENDIF
C
C*	Search for the max sustained wind speed
C
	ipos = INDEX( advise(irpt:lenw), 'WINDS...') 
        irpt = irpt + ipos + 7
	CALL ST_CLST ( advise (irpt:lenw), ' ', ' ', 2, carr,
     +			   num, ier )
	CALL ST_CRNM ( carr(1), rspd, ier )
	IF ( carr(2)(1:3) .eq. 'MPH' ) THEN
	    rspd = PR_MHKN ( rspd )
	ELSE IF ( carr(2)(1:5) .eq. 'KM/HR' ) THEN
	    rspd = PR_MSKN ( rspd * 1000. / 3600. )
	ENDIF
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
C*	Search for the central pressure
C
	ipos = INDEX( advise(irpt:lenw), 'PRESSURE...') 
	irpt = irpt + ipos + 10
	CALL ST_CLST ( advise (irpt:lenw), ' ', ' ', 2, carr,
     +			   num, ier )
	wpres = carr(1)
C*
	RETURN
	END
