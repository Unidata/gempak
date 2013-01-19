	SUBROUTINE AF_SLAT  ( clat, clatd, iret )
C************************************************************************
C* AF_SLAT								*
C*									*
C* This subroutine decodes and stores the latitude from AMDAR, AIREP,	*
C* and PIREP reports.							*
C*									*
C* AF_SLAT  ( CLAT, CLATD, IRET )					*
C*									*
C* Input parameters:							*
C*	CLAT		CHAR*		Encoded	latitude 		*
C*	CLATD		CHAR		Direction of latitude 	 	*
C*					  "N" = north, "S" = south	*
C*									*
C* Output parameters:							*
C*	RIVALS (IRSLAT)	REAL		Latitude in degrees		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = error during decoding	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Allow AIREP lat/long combination 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* A. Hardy/GSC         03/98   Changed latitude lengths                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	clat
	CHARACTER	clatd
C-----------------------------------------------------------------------
	iret = -1
C
	lclat = LEN ( clat )
	IF  ( ( lclat .eq. 3 ) .or. ( lclat .eq. 4 ) )  THEN
C
C*	    The latitude was reported in degrees and minutes.
C*	    Decode the minutes.
C
	    CALL ST_INTG  ( clat (lclat-1:lclat), iltmin, ier )
	    ldglat = lclat - 2
	    IF  (  ( ier .ne. 0 )  .or.
     +		 ( ( iltmin .lt. 0 ) .or. ( iltmin .gt. 59 ) )  )  THEN
		logmsg = 'latitude minutes '  //  clat (lclat-1:lclat)
		CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
		RETURN
	    END IF
	ELSE IF  ( ( lclat .le. 2 ) .and. 
     +		     ( bultyp .ne. AMDAR ) )  THEN
C
C*	    The latitude was reported in degrees.
C
	    iltmin = 0
	    ldglat = lclat
	ELSE
	    logmsg = 'latitude '  //  clat (1:lclat)
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Decode the degrees.
C
        CALL ST_INTG  ( clat (1:ldglat), iltdeg, ier )
	IF  (  ( ier .ne. 0 )  .or.
     +		( ( iltdeg .lt. 0 ) .or. ( iltdeg .gt. 90 ) )  )  THEN
	    logmsg = 'latitude degrees '  //  clat (1:ldglat)
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Compute the latitude.
C
	slat = FLOAT ( iltdeg ) + ( FLOAT ( iltmin ) / 60.0 )
	IF  ( clatd .eq. 'S' )  THEN
	    slat = slat * (-1)
	ELSE IF  ( clatd .ne. 'N' )  THEN
	    logmsg = 'latitude direction '  //  clatd
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Store the latitude.
C
	rivals ( irslat ) = slat
C
	iret = 0
C*
	RETURN
	END
