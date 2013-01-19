	SUBROUTINE RU_WIND  ( field, wnknot, dir, spd, iret )
C************************************************************************
C* RU_WIND								*
C*									*
C* This subroutine decodes a wind field in the form ddfff where dd	*
C* is the direction in tens of degrees and fff is the wind speed.	*
C* The units value of the direction rounded to the nearest 5 degrees	*
C* has been multiplied by 100 and added to fff.				*
C*									*
C* RU_WIND  ( FIELD, WNKNOT, DIR, SPD, IRET )				*
C*									*
C* Input parameters:							*
C*	FIELD		CHAR*		Encoded field			*
C*	WNKNOT		LOGICAL		Speed in knots flag		*
C*									*
C* Output parameters:							*
C*	DIR		REAL		Direction			*
C*	SPD		REAL		Speed in meters/sec		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/86						*
C* D. Kidwell/NCEP	 3/01	Added check for '31313'                 *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	field
	LOGICAL		wnknot
C------------------------------------------------------------------------
	iret = 0
C*
	dir = RMISSD
	spd = RMISSD
C
C*	The tens and hundreds of the wind direction is encoded in the
C*	first two digits.  Skip processing if direction is missing.
C
	CALL ST_INTG  ( field (1:2), idir, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	  ELSE
	    idir = idir * 10
	END IF
C
C*	The wind speed is encoded in the last three digits.
C
	CALL ST_INTG  ( field (3:5), ispd, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	  ELSE
C
C*	    The units value of the direction rounded to 0 or 5 is
C*	    multiplied by 100 and added to the wind speed.
C
	    IF  ( ispd .ge. 500 )  THEN
		ispd = ispd - 500
		idir = idir + 5
	    END IF
	END IF
C
C*	Set the output values.  Return missing data if direction is
C*	greater than 360 or field is '31313'.
C
	IF ( ( idir .le. 360 ) .and. ( field (1:5) .ne. '31313' ) ) THEN
	    spd = FLOAT ( ispd )
	    IF ( wnknot ) spd = PR_KNMS ( spd )
	    dir = FLOAT ( idir )
	    IF  ( dir .eq. 360. ) dir = 0.
	END IF
C*
	RETURN
	END
