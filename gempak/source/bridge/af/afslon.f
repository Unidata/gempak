	SUBROUTINE AF_SLON  ( clon, clond, iret )
C************************************************************************
C* AF_SLON								*
C*									*
C* This subroutine decodes and stores the longitude from AMDAR, AIREP,	*
C* and PIREP reports.							*
C*									*
C* AF_SLON  ( CLON, CLOND, IRET )					*
C*									*
C* Input parameters:							*
C*	CLON		CHAR*		Encoded	longitude 		*
C*	CLOND		CHAR		Direction of longitude 	 	*
C*					  "W" = west, "E" = east	*
C*									*
C* Output parameters:							*
C*	RIVALS (IRSLON)	REAL		Longitude in degrees		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = error during decoding	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Allow AIREP lat/long combination 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* A. Hardy/GSC         03/98   Changed longitude lengths               *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	clon
	CHARACTER	clond
C-----------------------------------------------------------------------
	iret = -1
C
	lclon = LEN ( clon )
	IF ( ( lclon .eq. 4 ) .or. ( lclon .eq. 5 ) ) THEN
C
C*	    The longitude was reported in degrees and minutes.
C*	    Decode the minutes.
C
	    CALL ST_INTG  ( clon ( lclon-1:lclon ), ilnmin, ier )
	    ldglon = lclon - 2
	    IF  (  ( ier .ne. 0 )  .or. 
     +		 ( ( ilnmin .lt. 0 ) .or. ( ilnmin .gt. 59 ) )  )  THEN
		logmsg = 'longitude minutes '  //  clon ( lclon-1:lclon )
		CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
		RETURN
	    END IF
	ELSE IF  ( ( lclon .le. 3 ) .and. 
     +		     ( bultyp .ne. AMDAR ) )  THEN
C
C*	    The longitude was reported in degrees.
C
	    ilnmin = 0
	    ldglon = lclon 
	ELSE
	    logmsg = 'longitude '  //  clon (1:lclon)
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Decode the degrees.
C
        CALL ST_INTG  ( clon ( 1:ldglon ), ilndeg, ier )
	IF  (  ( ier .ne. 0 )  .or.
     +	    ( ( ilndeg .lt. 0 ) .or. ( ilndeg .gt. 180 ) )  )  THEN
	    logmsg = 'longitude degrees '  //  clon (1:ldglon)
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Compute the longitude.
C
	slon = FLOAT ( ilndeg ) + ( FLOAT ( ilnmin ) / 60.0 )
	IF  ( clond .eq. 'W' )  THEN
	    slon = slon * (-1)
	ELSE IF  ( clond .ne. 'E' )  THEN
	    logmsg = 'longitude direction '  //  clond
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Store the longitude.
C
	rivals ( irslon ) = slon
C
	iret = 0
C*
	RETURN
	END
