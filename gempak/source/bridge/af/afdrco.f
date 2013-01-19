	SUBROUTINE AF_DRCO  ( report, lenr, iret )
C************************************************************************
C* AF_DRCO								*
C*									*
C* This subroutine decodes a RECCO report.				*
C*									*
C* AF_DRCO  ( REPORT, LENR, IRET )					*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		RECCO report 			*
C*	LENR		INTEGER		Length of REPORT 		*
C*									*
C* Output parameters:							*
C*	CIVALS (ICRPID)	CHAR*		RECCO identifier		*
C*	RIVALS (IRDAYW)	REAL		Day of the week			*
C*	RIVALS (IRSLAT)	REAL		Latitude in degrees		*
C*	RIVALS (IRSLON)	REAL		Longitude in degrees		*
C*	RIVALS (IRNTRB)	REAL		Number of turbulence levels	*
C*	RIVALS (IRDGOT)	REAL		Degree of turbulence		*
C*	RIVALS (IRTPOT)	REAL		Type of turbulence		*
C*	RIVALS (IRPSAL)	REAL		Pressure altitude in feet  	*
C*	RIVALS (IRTMPC)	REAL		Temperature in Celsius		*
C*	RIVALS (IRDWPC)	REAL		Dewpoint temperature in Celsius	*
C*	RIVALS (IRNPWX)	REAL		Number of weather levels	*
C*	CIVALS (ICWCOD)	CHAR*		Weather string			*
C*	RIVALS (IRVSBY)	REAL		Visibility in miles		*
C*	RIVALS (IRSSTC)	REAL		Sea surface temp. in Celsius	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		11/96						*
C* J. Ator/NP12		01/97	Decode sea temperature, surface wind,	*
C*				icing, visibility, and cloud data 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		03/98	Decode RPID following 'RMK' indicator	*
C* J. Ator/NCEP		12/98	Initialize rpids, lrpids via DATA stmts,*
C*				don't decode RPID from WMO 2nd hdr line	*
C* D. Kidwell/NCEP	 6/99   Changed turbulence values, added type   *
C* D. Kidwell/NCEP	 7/99   Changed code table for idayw, removed   *
C*			        irptr from call, psal meters to feet    *
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C* J. Ator/NCEP		 4/03	Fix bug in decoding of temps < -50C	*
C* C. Caruso Magee/NCEP  3/04   Fix bug in decoding of dew points < -50C*
C*			        and fix bug in decoding of altitudes    *
C*			        at or above 10000 meters                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	PARAMETER	( NRPIDS = 3 )
C*
	CHARACTER	rpids ( NRPIDS )*5, crid*8, field*(MXLENF)
C*
	INTEGER		lrpids ( NRPIDS )
C*
	DATA		rpids
     +		    / '97779', '92229', '95559' /
	DATA		lrpids
     +		    / 5, 5, 5 /
C-----------------------------------------------------------------------
	iret  = 0
	irptr = 1
C
C*	Set a default value for the RECCO identifier.
C
	civals ( icrpid ) = 'DRP99A'
C
C*	Locate the '97779', '92229', or '95559' indicator which
C*	denotes the start of the report.
C
	CALL ST_NXTS  ( report, irptr, ( irptr + 40 ), rpids, lrpids,
     +			NRPIDS, ipt1, irpid, iernxt )
	IF  ( iernxt .ne. 0 )  THEN
	    RETURN
	END IF
	irptr = irptr + ipt1 + ( lrpids ( irpid ) ) - 1
C
C*	Get the time group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the time (i.e. hour/minute) data.
C
	    CALL AF_HHMM  ( field (1:4), iertim )
C
C*	    Decode the id indicator for later use.
C
	    CALL ST_INTG  ( field (5:5), iidind, ier )
	ELSE
	    iidind = IMISSD
	END IF
C
C*	Get the latitude group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the day of the week data.
C
C*	    Day of the week values are stored in the interface format
C*	    as code figures from WMO code table 4900.
C
	    CALL ST_INTG  ( field (1:1), idayw, ier )
	    IF  ( ier .eq. 0 )  THEN
		rivals ( irdayw ) = FLOAT ( idayw )
	    END IF
C
C*	    Decode the octant of the globe.
C
	    CALL ST_INTG  ( field (2:2), iqoct, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  ( ( iqoct .eq. 4 ) .or. ( iqoct .eq. 9 ) )  THEN
		    iqoct = IMISSD
		END IF
	    END IF
C
C*	    Decode and store the latitude data.
C
	    CALL ST_INTG  ( field (3:5), islat, ier )
	    IF  ( ier .eq. 0 )  THEN
		slat = FLOAT ( islat ) / 10.0
		IF  ( iqoct .ne. IMISSD )  THEN
		    IF  ( ( iqoct .ge. 5 ) .and. ( slat .gt. 0 ) )  THEN
			slat = slat * (-1)
		    END IF
		    rivals ( irslat ) = slat
		END IF
	    END IF
	ELSE
	    iqoct = IMISSD
	END IF
C
C*	Get the longitude group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the longitude data.
C
	    CALL ST_INTG  ( field (1:3), islon, ier )
	    IF  ( ier .eq. 0 )  THEN
		slon = FLOAT ( islon ) / 10.0
		IF  ( iqoct .ne. IMISSD )  THEN
		    IF  ( ( iqoct .eq. 1 ) .or. ( iqoct .eq. 2 ) .or.
     +			  ( iqoct .eq. 6 ) .or. ( iqoct .eq. 7 ) )  THEN
			IF  ( INT ( slon ) .le. 80 )  THEN
			  slon = slon + 100.0
			END IF
		    END IF
		    IF  ( ( iqoct .eq. 0 ) .or. ( iqoct .eq. 1 ) .or.
     +			  ( iqoct .eq. 5 ) .or. ( iqoct .eq. 6 ) )  THEN
			IF  ( slon .gt. 0 )  THEN
			  slon = slon * (-1)
			END IF
		    END IF
		    rivals ( irslon ) = slon
		END IF
	    END IF
C
C*	    Decode and store the turbulence data.
C
C*	    Degree of turbulence values are stored in the interface
C*	    format using the following code figures - 
C* 		NONE     = 0
C*		LIGHT    = 2
C*		MODERATE = 4
C*		SEVERE   = 6
C*	    Type of turbulence values are stored as
C*		In clear air = 1
C*		In cloud     = 4
C
	    CALL ST_INTG  ( field (4:4), ibind, ier )
	    IF  ( ier .eq. 0 )  THEN
		tpot = RMISSD
		IF  ( ibind .eq. 0 )  THEN
		    dgot = 0.0
		ELSE IF ( ibind .eq. 1 )  THEN
		    dgot = 2.0
		ELSE IF ( ( ibind .ge. 2 ) .and. ( ibind .le. 5 ) ) THEN
		    dgot = 4.0
		    IF ( ibind .le. 3 ) THEN
			tpot = 1.0
		      ELSE
			tpot = 4.0
		    END IF
		ELSE IF ( ( ibind .ge. 6 ) .and. ( ibind .le. 9 ) ) THEN
		    dgot = 6.0
		    IF ( ibind .le. 7 ) THEN
			tpot = 1.0
		      ELSE
			tpot = 4.0
		    END IF
		END IF
		
		rivals ( irntrb ) = 1
		rivals ( irdgot (1) ) = dgot
		rivals ( irtpot (1) ) = tpot
	    END IF
C
C*	    Decode the fc indicator for later use.
C
	    CALL ST_INTG  ( field (5:5), ifcind, ier )
	    IF  ( ( ifcind .ne. 0 ) .and. ( ifcind .ne. 8 ) .and.
     +		  ( ifcind .ne. 9 ) )  THEN
		ifcind = IMISSD
	    END IF 
	ELSE
	    ifcind = IMISSD
	END IF
C
C*	Get the pressure altitude group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the pressure altitude data.
C*          Note:  Units for ipsal are decametres, so first convert to
C*          meters, then reconvert to feet for storage into rivals array.
C
	    CALL ST_INTG  ( field (1:3), ipsal, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF ( ( iidind .eq. 1 ) .or. ( iidind .eq. 3 ) .or.
     +               ( iidind .eq. 5 ) .or. ( iidind .eq. 7 ) ) THEN
C
C*                 Altitude is at or above 10000 meters.
C
	           rivals ( irpsal ) = 10000. + 
     +                                 ( FLOAT ( ipsal ) * 10.0 )
                ELSE 
C
C*                 Altitude is less than 10000 meters.
C
	           rivals ( irpsal ) = FLOAT ( ipsal ) * 10.0
                END IF
		rivals ( irpsal ) = PR_HGMF ( rivals ( irpsal ) ) 
	    END IF
	END IF
C
C*	Get the wind group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the wind data.
C
	    CALL AF_WIND  ( field (1:2), field (3:5), ierwnd )
	END IF
C
C*	Get the temperature/dewpoint/weather group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the temperature data.
C
	    CALL ST_INTG  ( field (1:2), itmpc, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  ( itmpc .gt. 50 )  THEN
C
C*                  Temperature reported is negative but warmer than 
C*                  -50 deg C.
C
		    tmpc = FLOAT ( 50 - itmpc )
		ELSE
		    IF ( ( iidind .eq. 2 ) .or. ( iidind .eq. 3 ) .or.
     +			 ( iidind .eq. 6 ) .or. ( iidind .eq. 7 ) ) THEN
C
C*                      Temperature reported is negative but is equal to
C*                      or colder than -50 deg C (i.e. an offset from -50
C*                      deg C was reported, not the actual temperature).
C
			tmpc = FLOAT ( 50 + itmpc ) * (-1.0)
		    ELSE
C
C*                      Temperature reported was equal to or warmer than
C*                      0 deg C.
C
		        tmpc = FLOAT ( itmpc )
		    END IF
		END IF
		rivals ( irtmpc ) = tmpc
	    END IF
C
C*	    Decode and store the dewpoint data.
C
	    CALL ST_INTG  ( field (3:4), idwpc, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  ( idwpc .gt. 50 )  THEN
C
C*                  Dew point reported is negative but warmer than 
C*                  -50 deg C.
C
		    dwpc = FLOAT ( 50 - idwpc )
		ELSE
		    IF ( ( iidind .eq. 6 ) .or. ( iidind .eq. 7 ) ) THEN
C
C*                      Dew point reported is negative but is equal to or
C*                      colder than -50 deg C (i.e. an offset from -50
C*                      deg C was reported, not the actual dew point).
C
			dwpc = FLOAT ( 50 + idwpc ) * (-1.0)
		    ELSE
C
C*                      Dew point reported was equal to or warmer than
C*                      0 deg C.
C
		        dwpc = FLOAT ( idwpc )
                    END IF
		END IF
		rivals ( irdwpc ) = dwpc
	    END IF
C
C*	    Decode and store the present weather data.
C
	    CALL ST_INTG  ( field (5:5), iwxind, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  ( iwxind .eq. 4 )  THEN
		    civals ( icwcod (1) ) = 'FG'
		ELSE IF  ( iwxind .eq. 5 )  THEN
		    civals ( icwcod (1) ) = 'DZ'
		ELSE IF  ( iwxind .eq. 6 )  THEN
		    civals ( icwcod (1) ) = 'RA'
		ELSE IF  ( iwxind .eq. 7 )  THEN
		    civals ( icwcod (1) ) = 'SN'
		ELSE IF  ( iwxind .eq. 9 )  THEN
		    civals ( icwcod (1) ) = 'TS'
		END IF
		IF  ( civals ( icwcod (1) ) (1:1) .ne. ' ' )  THEN
		    rivals ( irnpwx ) = 1
		END IF
	    END IF
	END IF
C
C*	Get the /JHHH (i.e. pressure/height) group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( lenf .eq. 5 )  THEN
C
C*	    Decode and store the pressure/height data.
C
	    CALL AF_RPHT  ( field (1:5), ierpht )
	END IF
C
	IF  ( irptr .lt. lenr )  THEN
C
C*	    Look for an 'RMK' indicator in the remainder of the report.
C
	    irmk = INDEX ( report ( irptr : lenr ), 'RMK' )
	    IF  ( irmk .ne. 0 )  THEN
		istart = irptr + irmk + 2 
		IF  ( istart .lt. lenr )  THEN
C
C*		    Decode and store the RECCO identifier.
C
		    CALL DC_CRID  ( report ( istart : lenr ),
     +				    crid, lcrid, ierrid )
		    IF  ( ierrid .eq. 0 )  THEN
			civals ( icrpid ) = '        '
			civals ( icrpid ) = crid ( 1 : lcrid )
		    END IF
		END IF
	    END IF
	END IF
C
C*	If the report indicator was '92229', then there
C*	is not an additional section within this report.
C
	IF  ( irpid .eq. 2 )  THEN
	    RETURN
	END IF
C
C*	Decode and store any additional section data within this report.
C
C*	There is no standard format for the additional section
C*	(i.e. many types of data are possible, each of which can be
C*	reported or omitted in accordance with local requirements, and
C*	the types which are reported can appear in any order with
C*	respect to each other!); thus, to prevent mistakes, stop the
C*	decoding of additional section data altogether if a format
C*	error is detected during decoding of any one of the individual
C*	types.
C
	DO WHILE ( .true. )
	    ipt1 = irptr
	    CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	    IF  ( ( ier .ne. 0 ) .or. ( lenf .ne. 5 ) ) THEN
		RETURN
	    END IF
	    IF  ( field (1:1) .eq. '1' )  THEN
C
C*		Decode the cloud data.
C
		irptr = ipt1
		CALL AF_RCLD  ( report, lenr, irptr, iercld )
		IF  ( iercld .ne. 0 )  THEN
		    RETURN
		END IF
	    ELSE IF  ( field (1:1) .eq. '4' )  THEN
C
C*		Decode and store the surface wind data.
C
		CALL AF_RSWD  ( field (2:5), ierswd )
	    ELSE IF  ( field (1:1) .eq. '6' )  THEN
	    ELSE IF  ( field (1:1) .eq. '7' )  THEN
C
C*		Decode and store the icing data.
C
		irptr = ipt1
		CALL AF_RICG  ( report, lenr, irptr, iericg )
		IF  ( iericg .ne. 0 )  THEN
		    RETURN
		END IF
	    ELSE IF  ( field (1:1) .eq. '8' )  THEN
	    ELSE IF  ( field (1:1) .eq. '9' )  THEN
C
C*		Decode and store the flight visibility data.
C
		CALL ST_INTG  ( field (2:2), ivi, ier )
		IF  ( ( ivi .ge. 1 ) .and. ( ivi .le. 3 ) )  THEN
		    rivals ( irvsby ) = FLOAT ( ivi )
		END IF
C
C*		Decode and store the sea-surface temperature data.
C
		CALL ST_INTG  ( field (3:5), isstc, ier )
		IF  ( ier .eq. 0 )  THEN
		    rivals ( irsstc ) = FLOAT ( isstc ) / 10.0
		END IF
	    ELSE
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
