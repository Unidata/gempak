        SUBROUTINE NAMCDT ( ihr, iday, ifr, dattim, ifhr, dt, iret )
C************************************************************************
C* NAMCDT								*
C*									*
C* This subroutine computes the valid time of the model profile data.	*
C*									*
C* NAMCDT  ( IHR, IDAY, IFR, DATTIM, IFHR, DT, IRET )			*
C*									*
C* Input parameters:							*
C*	IHR		INTEGER		Starting hour (cycle time)	*
C*	IDAY (3)	INTEGER		Starting month, day, year	*
C*	IFR		INTEGER		Forecast time (seconds)		*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK valid time of profile	*
C*	IFHR		INTEGER		Forecast hour			*
C*	DT		REAL		Time (s) since last sounding	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* K. Brill/NMC		12/93						*
C* D. Kidwell/NCEP	12/98	SNMCDT -> NAMCDT                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		iday (3)
	CHARACTER*(*)	dattim
C*
	INTEGER		itmbeg (5)
	SAVE		ifrlst, itot
C-----------------------------------------------------------------------
	iret = 0
	IF ( ifr .eq. 0 ) THEN
	    ifrlst = 0
	    itot = 0
	    dt = 3600.
	END IF
C
C*	Compute the hour and minute of the forecast.
C
	IF ( ifr .ne. ifrlst ) THEN
	    idif = ifr - ifrlst
	    ifrlst = ifr
	    dt = FLOAT ( idif )
	    IF ( idif .lt. 0 ) THEN
		idif = 0
	     ELSE
C
C*		Round to nearest 15 minutes.
C
		tdif = idif
		idif = 900 * NINT ( tdif / 900. )
		itot = itot + idif
	    END IF
	END IF
C*
	ifhr = itot / 3600
	ifmn = MOD ( itot, 3600 ) / 60
	ntoth = ihr + ifhr
	ndays = ntoth / 24
C
C*	Add ndays to the starting date/time.
C
	itmbeg (1) = iday (3)
	itmbeg (2) = iday (1)
	itmbeg (3) = iday (2)
	itmbeg (4) = 0
	itmbeg (5) = 0
	IF ( ndays .gt. 0 ) THEN
	    DO id = 1, ndays
		CALL TI_ADDD ( itmbeg, itmbeg, ier )
	    END DO
	END IF
	itmbeg (4) = MOD ( ntoth, 24 )
	itmbeg (5) = ifmn
	CALL TI_ITOC ( itmbeg, dattim, ier )
C*
	RETURN
	END
