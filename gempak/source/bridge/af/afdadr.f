	SUBROUTINE AF_DADR  ( report, lenr, iret )
C************************************************************************
C* AF_DADR								*
C*									*
C* This subroutine decodes an AMDAR report.				*
C*									*
C* AF_DADR  ( REPORT, LENR, IRET )					*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		AMDAR report 			*
C*	LENR		INTEGER		Length of REPORT 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRPOAF)	REAL		Phase of aircraft flight	*
C*	CIVALS (ICACID)	CHAR*		Aircraft flight identifier	*
C*	RIVALS (IRPSAL)	REAL		Pressure altitude in feet      	*
C*	RIVALS (IRDWPC)	REAL		Dewpoint temperature in Celsius	*
C*	RIVALS (IRRELH)	REAL		Percent relative humidity	*
C*	RIVALS (IRNTRB)	REAL		Number of turbulence levels	*
C*	RIVALS (IRDGOT)	REAL		Degree of turbulence		*
C*	RIVALS (IRACNS)	REAL		Aircraft navigation system type	*
C*	RIVALS (IRTADR)	REAL		Aircraft data relay system type	*
C*	RIVALS (IRPCAT)	REAL		Temperature precision indicator	*
C*	RIVALS (IRMDVG)	REAL		Max. vertical gust speed in m/s	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Remove calls to ERRRPT 			*
C* J. Ator/NP12		12/96	Replace interface parm VGVL with MDEVG 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		05/98	Decode UNS phase of aircraft flight	*
C* D. Kidwell/NCEP       6/99   Changed turbulence code values          *
C* D. Kidwell/NCEP       7/99   Removed irptr from calling sequence,    *
C*				changed meters to feet in prologue      *
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C* J. Ator/NCEP		04/00	Decode day if it exists in time group	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	field*(MXLENF)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret  = 0 
	irptr = 1
C
C*	Get the phase of aircraft flight group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 3 )  THEN
C
C*	    Decode the phase of aircraft flight.
C
C*	    Phase of aircraft flight values are stored in the interface
C*	    format as code figures from WMO BUFR Table 0 08 004.
C
	    IF  ( field (1:3) .eq. 'UNS' )  THEN
		poaf = 2.0
	    ELSE IF  ( field (1:3) .eq. 'LVR' )  THEN
		poaf = 3.0
	    ELSE IF  ( field (1:3) .eq. 'LVW' )  THEN
		poaf = 4.0
	    ELSE IF  ( field (1:3) .eq. 'ASC' )  THEN
		poaf = 5.0
	    ELSE IF  ( field (1:3) .eq. 'DES' )  THEN
		poaf = 6.0
	    ELSE
		poaf = RMISSD
	    END IF
	    IF  ( .not. ( ERMISS ( poaf ) ) )  THEN
C
C*		Store the phase of flight indicator.
C
		rivals ( irpoaf ) = poaf
	    END IF
	END IF
C
C*	Get the aircraft flight identifier group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Store the aircraft flight identifier.
C
	IF  ( lenf .le. 8 )  THEN
	    lacid = lenf
	ELSE
	    lacid = 8
	END IF
	civals ( icacid ) = field ( 1 : lacid )
C
C*	Get the latitude group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the latitude data.
C
	    CALL AF_SLAT  ( field (1:4), field (5:5), ierslt )
	END IF
C
C*	Get the longitude group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 6 )  THEN
C
C*	    Decode and store the longitude data.
C
	    CALL AF_SLON  ( field (1:5), field (6:6), iersln )
	END IF
C
C*	Get the time group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 4 )  THEN
C
C*	    Decode and store the hour and minute.
C
	    CALL AF_HHMM  ( field (1:4), iertim )
	ELSE IF  ( lenf .eq. 6 )  THEN
C
C*	    Decode and store the day, hour, and minute.
C
	    CALL ST_INTG  ( field (1:2), idays, ier )
	    IF  ( ( ier .eq. 0 ) .and.
     +		  ( idays .ge. 1 ) .and. ( idays .le. 31 ) )  THEN
		rivals ( irdays ) = FLOAT ( idays )
	    END IF
C
	    CALL AF_HHMM  ( field (3:6), iertim )
	END IF
C
C*	Get the pressure altitude group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  (  ( lenf .eq. 4 ) .and.
     +		     ( ( field (1:1) .eq. 'F' ) .or.
     +		       ( field (1:1) .eq. 'A' ) )  )  THEN
C
C*	    Decode and store the pressure altitude data.
C
	    CALL AF_HHFM  ( field (2:4), psal, ierhfm )
	    IF  ( .not. ( ERMISS ( psal ) ) )  THEN
		IF  ( field (1:1) .eq. 'A' ) THEN
		    psal = psal * (-1.0)
		END IF
		rivals ( irpsal ) = psal
	    END IF
	END IF
C
C*	Get the temperature group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the temperature data.
C
	    CALL AF_TMPC  ( field (1:2), field (3:5), iertmp )
	END IF
C
C*	Is there a moisture group present?
C
	ipt1 = irptr
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
	IF  ( lenf .eq. 7 )  THEN
	    irptr = ipt1
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the dewpoint temperature data.
C
	    IF  ( ( field (1:2) .eq. 'MS' ) .or.
     +		  ( field (1:2) .eq. 'PS' ) )  THEN 
		CALL ST_INTG  ( field (3:5), idwpc, ier )
		IF  ( ier .eq. 0 )  THEN
		    dwpc = FLOAT ( idwpc ) / 10.0
		    IF  ( field (1:1) .eq. 'M' )  THEN
			dwpc = dwpc * (-1.0)
		    END IF
		    rivals ( irdwpc ) = dwpc
		END IF
	    END IF
	ELSE IF  ( lenf .eq. 3 )  THEN
C
C*	    Decode and store the relative humidity data.
C
	    CALL ST_INTG  ( field (1:3), irelh, ier )
	    IF  ( ( ier .eq. 0 ) .and.
     +		  ( irelh .ge. 0 ) .and. ( irelh .le. 100 ) )  THEN
		rivals ( irrelh ) = FLOAT ( irelh )
	    END IF
	END IF
C
C*	Get the wind group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 7 )  THEN
C
C*	    Decode and store the wind data.
C
	    CALL AF_WIND  ( field (1:3), field (5:7), ierwnd )
	END IF
C
C*	Get the turbulence group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( ( lenf .eq. 3 ) .and.
     +		    ( field (1:2) .eq. 'TB' ) )  THEN
C
C*	    Decode and store the turbulence data.
C
C*	    Degree of turbulence values are stored in the interface
C*	    format using the following code figures -
C*		NONE     = 0
C*		LIGHT    = 2
C*		MODERATE = 4
C*		SEVERE   = 6
C
	    CALL ST_INTG  ( field (3:3), idgot, ier )
	    IF  ( ier .eq. 0 )  THEN
		rivals ( irntrb ) = 1
		rivals ( irdgot (1) ) = FLOAT ( idgot * 2 )
	    END IF
	END IF
C
C*	Get the system information group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
	tadr = RMISSD
C
	IF  ( ( lenf .eq. 4 ) .and. ( field (1:1) .eq. 'S' ) )  THEN
C
C*	    Decode and store the indicator for aircraft
C*	    navigation system.
C
C*	    Aircraft navigation system type values are stored in the
C*	    interface format as code figures from WMO Code Table 3866.
C
	    CALL ST_INTG  ( field (2:2), iacns, ier )
	    IF  ( ier .eq. 0 )  THEN
		rivals ( iracns ) = FLOAT ( iacns )
	    END IF
C
C*	    Decode and store the indicator for type of aircraft
C*	    data relay system used.
C
C*	    Aircraft data relay system type values are stored in the
C*	    interface format as code figures from WMO Code Table 3867.
C
	    CALL ST_INTG  ( field (3:3), itadr, ier )
	    IF  ( ier .eq. 0 )  THEN
		tadr = FLOAT ( itadr )
		rivals ( irtadr ) = tadr
	    END IF
C
C*	    Decode and store the indicator for temperature precision.
C
C*	    Temperature precision indicators are stored in the interface
C*	    format as code figures from WMO Code Table 3868.
C
	    CALL ST_INTG  ( field (4:4), ipcat, ier )
	    IF  ( ier .eq. 0 )  THEN
		rivals ( irpcat ) = FLOAT ( ipcat )
	    END IF
	END IF
C
C*	If this report was not made by an ACARS system, then
C*	there is nothing more to decode.
C
	IF  ( ERMISS ( tadr ) )  THEN
	    RETURN
	ELSE IF  ( tadr .lt. 3 )  THEN
	    RETURN
	END IF
C
C*	Check that the next group is '333'.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .ne. 3 )  THEN
	    RETURN
	ELSE IF  ( field (1:3) .ne. '333' )  THEN
	    RETURN
	END IF
C
C*	Get the flight level group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( ( lenf .eq. 4 ) .and.
     +		    ( field (1:1) .eq. 'F' ) )  THEN
C
C*	    Decode the flight level data.
C
	    CALL AF_FLVL  ( field (2:4), ierflv )
	END IF
C
C*	Get the maximum derived equivalent vertical gust speed group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( ( lenf .eq. 5 ) .and.
     +		    ( field (1:2) .eq. 'VG' ) )  THEN
C
C*	    Decode and store the maximum derived equivalent vertical
C*	    gust speed data.
C
	    CALL ST_INTG  ( field (3:5), idevg, ier )
	    IF  ( ier .eq. 0 )  THEN
		rivals ( irmdvg ) = FLOAT ( idevg ) / 10.0
	    END IF
	END IF
C*
	RETURN
	END
