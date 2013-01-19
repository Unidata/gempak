	SUBROUTINE AF_PLOC  ( report, isloc, ieloc, iret )
C************************************************************************
C* AF_PLOC								*
C*									*
C* This subroutine decodes and stores the location data from within	*
C* a PIREP report.  The location data may be a set of one or two	*
C* "location points" (i.e. a "location point" is a navaid or a		*
C* navaid/bearing/distance), or it may be a latitude/longitude		*
C* combination such as is found in AIREP reports.			*
C*									*
C* AF_PLOC  ( REPORT, ISLOC, IELOC, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISLOC		INTEGER		Pointer to start of location	*
C*					data within REPORT 		*
C*	IELOC		INTEGER		Pointer to end of location	*
C*					data within REPORT 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRSLAT)	REAL		Latitude in degrees		*
C*	RIVALS (IRSLON)	REAL		Longitude in degrees		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Allow AIREP lat/long combination 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* A. Hardy/GSC         04/98   Changed IF condition for lat/lon return *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C-----------------------------------------------------------------------
	iret = 0
C
C*	Break up the input string into groups of "like-type" in order
C*	to facilitate decoding.
C
	CALL AF_BKGP  ( report ( isloc : ieloc ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*      Determine if the location data was reported as a latitude/
C*      longitude combination and, if so, decode and store the
C*      latitude and longitude.
C
	CALL AF_ALAT  ( 1, 5, islat, ielat, ieralt )
	CALL AF_ALON  ( 1, 5, islon, ielon, ieraln )
	IF  ( ( ielat .ne. IMISSD ) .and. ( ielon .ne. IMISSD ) )  THEN
	    RETURN
        END IF
C
C*	Decode the first location point.
C
	isl1 = 1
	CALL AF_PNBD  ( isl1, iel1, rlat1, rlon1, ierpn1 )
	IF  ( ierpn1 .ne. 0 )  THEN
	    RETURN
	END IF
C
	IF  ( iel1 .eq. nflds )  THEN
C
C*	    This is a point report, so the first location point
C*	    is equal to the aircraft location.
C
	    slat = rlat1
	    slon = rlon1
	ELSE IF  (  ( ( iel1 + 2 ) .le. nflds ) .and.
     +		   ( lensf ( iel1 + 1 ) .eq. 1 ) .and.
     +		   ( fields ( iel1 + 1 ) (1:1) .eq. '-' )  )  THEN
C
C*	    This is a route report.
C
C*	    Decode the second location point.
C
	    isl2 = iel1 + 2
	    CALL AF_PNBD  ( isl2, iel2, rlat2, rlon2, ierpn2 )
	    IF  ( ierpn2 .ne. 0 )  THEN
		RETURN
	    ELSE IF  ( iel2 .ne. nflds )  THEN
		logmsg = 'aircraft location '
     +			//  report ( isloc : ieloc )
		CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
		RETURN
	    END IF
C
C*	    Use the midpoint of the route as the aircraft location.
C
	    slat = ( rlat1 + rlat2 ) / 2.0
	    slon = ( rlon1 + rlon2 ) / 2.0
	ELSE
	    logmsg = 'aircraft location '
     +		    //  report ( isloc : ieloc )
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Store the aircraft location.
C
	rivals ( irslat ) = slat
	rivals ( irslon ) = slon
C*
	RETURN
	END
