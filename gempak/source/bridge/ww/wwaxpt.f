	SUBROUTINE WW_AXPT ( string, dist, bear, locid, iret )
C************************************************************************
C* WW_AXPT  								*
C*									*
C* This subroutine gets the distance, bearing and location identifier   *
C* from a string having the form DDDBBB LOC or DDD BBB LOC.  LOC will   *
C* be a three character location identifier, while DDD and BBB, if      *
C* present, may each have from one to three characters.                 *
C*                                                                      *
C* WW_AXPT ( STRING, DIST, BEAR, LOCID, IRET )                          *
C*									*
C* Input parameters:							*
C*	STRING 		CHAR*	     String with distance, bearing, loc *
C*									*
C* Output parameters:							*
C*	DIST		REAL	     Distance                           *
C*	BEAR   		REAL	     Bearing (degrees from N)           *
C*      LOCID		CHAR*	     Location identifier                *
C*	IRET		INTEGER	     Return code			*
C*				       0 = normal return                *
C*				      -1 = bad field                    *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/99	                                        *
C* D. Kidwell/NCEP	 8/99	Allowed DDD BBB LOC; removed arg. lens  *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, locid
C*
	CHARACTER	carr (3)*6, compas (16)*3, ddd*3, bbb*3
	INCLUDE		'ERMISS.FNC'
C*
	DATA compas 	/ 'N', 'NNE', 'NE', 'ENE',
     +			  'E', 'ESE', 'SE', 'SSE',
     +			  'S', 'SSW', 'SW', 'WSW',
     +		 	  'W', 'WNW', 'NW', 'NNW' /
C*
C------------------------------------------------------------------------
	iret  = 0
	dist  = RMISSD
	bear  = RMISSD
	locid = ' '
C 
	CALL ST_CLST ( string, ' ', ' ', 3, carr, num, ier )
	IF ( num .eq. 1 ) THEN
C
C*	    Only a location identifier is specified - point is colocated
C*	    with it.
C
	    locid = carr ( 1 ) ( :3 )
	    dist  = 0.
	    bear  = 0.
	  ELSE IF ( num .eq. 2 ) THEN
C
C*	    The format is DDDBBB LOC.
C
	    locid = carr ( 2 ) ( :3 )
	    lens  = INDEX ( carr ( 1 ), ' ' ) - 1
	    IF ( lens .lt. 0 ) lens = 6
	    ibb = 1
	    ied = lens
C
C*	    Find end of distance field and beginning of bearing field.
C
	    DO i = 1, lens
		CALL ST_ALNM ( carr ( 1 ) ( i:i ), ityp, ier )
		IF ( ityp .eq. 1 ) THEN
		    ied = i
		  ELSE IF ( ( ityp .eq. 2 ) .and. ( ibb .eq. 1 ) ) THEN
		    ibb = i
		END IF
	    END DO
C
	    ddd = carr ( 1 ) ( :ied )
	    bbb = carr ( 1 ) ( ibb:lens )
	  ELSE IF ( num .eq. 3 ) THEN
C
C*	    The format is DDD BBB LOC.
C
	    locid = carr ( 3 ) ( :3 )
	    ddd   = carr ( 1 ) ( :3 )
	    lens  = INDEX ( carr ( 2 ), ' ' ) - 1
	    bbb   = carr ( 2 ) ( :lens )
	  ELSE
	    iret  = -1
	    RETURN
	END IF
C
	IF ( num .gt. 1 ) THEN
C
C*	    Get the distance.
C
	    CALL ST_CRNM ( ddd, dist, ier )
C
C*	    Get the bearing, converting from compass direction to
C*	    degrees from north.
C
	    CALL ST_FIND ( bbb, compas, 16, ipos, ier )
	    IF ( ipos .gt. 0 ) bear = ( ipos - 1 ) * 22.5
	END IF
C
C*	Check for good results.
C
	IF ( ( ERMISS ( dist ) ) .or. ( ERMISS ( bear ) ) ) iret = -1
	IF ( INDEX ( locid ( :3 ), ' ' ) .ne. 0 ) iret = -1
C*
	RETURN
	END
