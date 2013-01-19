	SUBROUTINE RU_DROP  ( string, lens, ipoint, xlat, xlon, iret )
C************************************************************************
C* RU_DROP								*
C*									*
C* This subroutine returns the latitude and longitude for a mobile      *
C* platform (dropsonde or ship).                                        *
C*									*
C* RU_DROP  ( STRING, LENS, IPOINT, XLAT, XLON, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*    	String				*
C*	LENS		INTEGER		Length of string		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer in report		*
C*									*
C* Output parameters:							*
C*	XLAT		REAL		Mobile platform latitude        *
C*	XLON		REAL		Mobile platform longitude       *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid group             *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/01						*
C* D. Kidwell/NCEP	 5/01	Updated prolog, dropsonde -> mobile	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C*
	CHARACTER	field*6
C------------------------------------------------------------------------
	iret  = 0
	xlat  = RMISSD
	xlon  = RMISSD
C
C*	Get the first field, which contains 99 followed by the 
C*	latitude.
C
	CALL RU_GFLD ( string, lens, ipoint, field, lenf, ier )
	IF ( ( ier .ne. 0 ) .or. ( lenf .lt. 5 ) ) THEN
	    iret = -3
	    RETURN
	  ELSE
	    IF ( field ( 1:2 ) .ne. '99' ) THEN
	 	iret = -3
		RETURN
	    END IF
	END IF
C
	CALL ST_INTG ( field ( 3:5 ), ilat, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -3
	    RETURN
	END IF
	xlat = FLOAT ( ilat ) * .1
C
C*	Get the second field, which contains the quadrant and the
C*	longitude.
C
	CALL RU_GFLD ( string, lens, ipoint, field, lenf, ier )
	IF ( ( ier .ne. 0 ) .or. ( lenf .lt. 5 ) ) THEN
	    iret = -3
	    RETURN
	  ELSE
	    CALL ST_INTG ( field ( 1:1 ), iquad, ier1 )
	    CALL ST_INTG ( field ( 2:5 ), ilon, ier2 )
	    IF ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) ) THEN
		iret = -3
		RETURN
	      ELSE
		xlon = float ( ilon ) * .1
	    END IF
	END IF
C
C*	Get signs of latitude and longitude based on the quadrant.
C
	IF ( iquad .eq. 7 ) THEN
	    xlon = -xlon
	  ELSE IF ( iquad .eq. 5 ) THEN
	    xlon = -xlon
	    xlat = -xlat
	  ELSE IF ( iquad .eq. 3 ) THEN
	    xlat = -xlat
	  ELSE IF ( iquad .eq. 1 ) THEN
	  ELSE
	    iret = -3 
	    RETURN
	END IF
C
C*	Skip the third field, which contains the Marsden square number.
C
	CALL RU_GFLD ( string, lens, ipoint, field, lenf, ier )
	IF ( ier .lt. 0 ) iret = -3
C*
	RETURN
	END
