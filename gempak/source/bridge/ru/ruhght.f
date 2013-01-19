	SUBROUTINE RU_HGHT  ( field, above, drop, p, z, iret )
C************************************************************************
C* RU_HGHT								*
C*									*
C* This subroutine decodes a pressure/height field in the form PPhhh.	*
C* For data below 100 mb, PP is the pressure in tens of millibars and	*
C* hhh is the last three digits of the height in meters below 500 mb	*
C* and the last three digits of height in decameters at/above 500 mb.	*
C* For data above 100 mb, PP is the pressure in millibars and hhh is	*
C* the last three digits of the height in decameters.			*
C*									*
C* RU_HGHT  ( FIELD, ABOVE, DROP, P, Z, IRET )				*
C*									*
C* Input parameters:							*
C*	FIELD		CHAR*		Encoded field			*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*	DROP		LOGICAL		Dropsonde flag                  *
C*									*
C* Output parameters:							*
C*	P		REAL		Pressure			*
C*	Z		REAL		Height				*
C*	IRET		INTEGER		Return code			*
C*					  0 = return code		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* K. Brill/NMC		01/92	Added the new 925 mb mandatory level	*
C* D. Blanchard/NSSL	 3/93	Fixed heights above 100mb and below	*
C*				  sea level				*
C* D. Kidwell		 2/05	CSC for drop, added 925mb check         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	LOGICAL		above, drop
	CHARACTER*(*)	field
C------------------------------------------------------------------------
	iret = 0
	p = RMISSD
	z = RMISSD
C
C*	Decode the pressure which is in the first two characters.
C
	CALL ST_INTG  ( field (1:2), ipres, ier )
	IF  ( ier .eq. 0 )  THEN
C
C*	    Encoded pressure was pressure / 10.  00 is 1000.
C*	    Above 100mb, encoded pressure is whole number.
C
	    IF  ( .not. above )  THEN
		ipres = ipres * 10
		IF ( ipres .eq. 0 ) ipres = 1000
	    END IF
C
C*	    Take care of the 925 mb level, which comes in as 920.
C
	    IF ( ipres .eq. 920 ) ipres = 925
	    p = ipres
C
C*	    Get the height which is in the last three characters.
C
	    CALL ST_INTG  ( field (3:5), iz, ier )
C
C*	    For data below 100 mb, use the pressure to decode the 
C*	    height.  This algorithm is from the U. of Wisconsin and 
C*	    differs slightly from the PROFS algorithm.
C
	    IF  ( ( ier .eq. 0 ) .and. ( .not. above ) )  THEN
		z = iz
		IF ( (ipres .eq. 1000) .and. (iz .gt. 500) )  THEN
		    z = 500. - z
		  ELSE IF ( (ipres .eq. 925) .and. (iz .lt. 200) .and.
     +			     .not. drop ) THEN
		    z = z + 1000.
		  ELSE IF ( (ipres .eq. 850) .and. (iz .lt. 900) ) THEN
		    z = z + 1000.
		  ELSE IF ( (ipres .eq. 700) .and. (iz .lt. 500) ) THEN
		    z = z + 3000.
		  ELSE IF (ipres .eq. 700) THEN
		    z = z + 2000.
		  ELSE IF (ipres .le. 500) THEN
		    iz = iz * 10
		    z  = z * 10.
		    IF ( (ipres .eq. 300) .and. (iz .lt. 3000) ) THEN
			z = z + 10000.
		      ELSE IF ((ipres .eq. 250) .and. (iz .lt. 5000))
     +                                                              THEN
			z = z + 10000.
		      ELSE IF ((ipres .eq. 200) .and. (iz .lt. 7000))
     +                                                              THEN
			z = z + 10000.
		      ELSE IF (ipres .le. 150) THEN
			z = z + 10000.
		    END IF
		END IF
	    END IF
C
C*	    Compute the height above 100 mb.  The ten thousands digit
C*	    is added here.  The value may need to be changed in the
C*	    future if it proves incorrect.
C
	    IF  ( ( ier .eq. 0 ) .and. ( above ) )  THEN
		iz = iz * 10
		z  = iz
		IF  ( ipres .eq. 70 ) THEN
		    z = z + 10000.
		  ELSE IF ((ipres .eq. 50) .and. (iz .ge. 8000)) THEN
		    z = z + 10000.
		  ELSE IF ((ipres .eq. 50) .and. (iz .lt. 8000)) THEN
		    z = z + 20000.
		  ELSE IF ( ipres .ge. 20 ) THEN
		    z = z + 20000.
		  ELSE IF ((ipres .eq. 10) .and. (iz .gt. 8000)) THEN
		    z = z + 20000.
		  ELSE IF ((ipres .eq. 10) .and. (iz .lt. 8000)) THEN
		    z = z + 30000.
		  ELSE IF ( ipres .ge. 3 ) THEN
		    z = z + 30000.
		  ELSE IF ((ipres .eq. 2) .and. (iz .gt. 8000)) THEN
		    z = z + 30000.
		  ELSE IF ((ipres .eq. 2) .and. (iz .lt. 8000)) THEN
		    z = z + 40000.
		  ELSE
		    z = z + 40000.
		END IF
	    END IF
	END IF
C*
	RETURN
	END
