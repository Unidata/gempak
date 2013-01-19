	SUBROUTINE AF_AWPT  ( field, lenf, cicli, iret )
C************************************************************************
C* AF_AWPT								*
C*									*
C* This subroutine determines whether FIELD contains a waypoint		*
C* identifier and, if so, stores the associated latitude and longitude	*
C* from the AIREP waypoint table.  In the case of multiple table	*
C* entries for a single waypoint identifier, the subroutine will try to	*
C* pick an entry whose ICAO country code matches the country code from	*
C* the originator of the bulletin which contained this AIREP report.	* 
C*									*
C* AF_AWPT  ( FIELD, LENF, CICLI, IRET )				*
C*									*
C* Input parameters:							*
C*	FIELD		CHAR*		Field 				*
C*	LENF		INTEGER		Length of FIELD 		*
C*	CICLI		CHAR*		Bulletin originator 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRSLAT)	REAL		Latitude in degrees		*
C*	RIVALS (IRSLON)	REAL		Longitude in degrees		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = FIELD is not a waypoint ID*
C*					 +1 = Couldn't pick from among	*
C*					      multiple table entries	*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		12/97	AF_BSRC -> DC_BSRC			*
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	field, cicli
C-----------------------------------------------------------------------
	iret = 0
C
C*	Is the input field a waypoint identifier?
C
	CALL DC_BSRC  ( field (1:lenf), adwypt, nade, jj, iersrc )
	IF  ( jj .eq. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	The input field is a waypoint identifier.
C
	IF  ( adnswp (jj) .eq. 1 )  THEN
C
C*	    There is only one entry in the AIREP waypoint
C*	    table for this waypoint identifier.
C
	    iindwp = jj
	ELSE
C
C*	    There are multiple adjacent entries in the AIREP
C*	    waypoint table for this waypoint identifier, so
C*	    care must be taken to select the correct one.
C
C*	    Select the entry whose ICAO country code matches
C*	    the first two characters of the originator
C*	    for the AIREP bulletin containing this report.
C
	    iindwp = 0
	    iind = jj - adnswp (jj) + 1
C
	    DO WHILE  (  ( iindwp .eq. 0 ) .and.
     +			( iind .le. ( jj + adnswp (jj) - 1 ) )  )
		IF  ( ( iind .ge. 1 ) .and. ( iind .le. nade ) )  THEN
		    IF  ( ( field (1:lenf) .eq. adwypt ( iind ) ) .and.
     +			   ( cicli (1:2) .eq. adiccn ( iind ) )  )  THEN
			iindwp = iind
		    END IF
		END IF
		iind = iind + 1
	    END DO
C
	    IF  ( iindwp .eq. 0 )  THEN
		logmsg = 'AIREP waypoint '  //  field (1:lenf)  //
     +		    ' unknown for country code '  //  cicli (1:2)
		CALL DC_WLOG  ( 2, 'AF', 1, logmsg, ierwlg )
		iret = 1
		RETURN
	    END IF
	END IF
C
C*	Store the latitude and longitude values associated with this
C*	waypoint identifier.
C
	rivals ( irslat ) = adlat ( iindwp )
	rivals ( irslon ) = adlon ( iindwp )
C*
	RETURN
	END
