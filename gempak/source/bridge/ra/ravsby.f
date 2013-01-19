	SUBROUTINE RA_VSBY  ( ipcaft, iptbef, ipvaft, vsby, iret )
C************************************************************************
C* RA_VSBY								*
C*									*
C* This subroutine gets the visibility from an airways report.		*
C*									*
C* RA_VSBY  ( IPCAFT, IPTBEF, IPVAFT, VSBY, IRET )			*
C*									*
C* Input parameters:							*
C*	IPCAFT		INTEGER		Pointer to field after clouds	*
C*	IPTBEF		INTEGER		Pointer to field before temp	*
C*									*
C* Output parameters:							*
C*	IPVAFT		INTEGER		Pointer to field after vis	*
C*	VSBY		REAL		Visibility in miles		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* B. Doty/RDS		11/87						*
C* I. Graffman/RDS	 4/88						*
C* M. desJardins/GSFC	 9/89	GEMPAK 5				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
	LOGICAL		found
C-----------------------------------------------------------------------
	iret   = 0
	vsby   = RMISSD
C
C*	Skip to number which is the visibility.
C
	found  = .false.
	ipvaft = ipcaft
	DO WHILE  ( ( .not. found ) .and. ( ipvaft .le. iptbef ) )
	    IF  ( iftype ( ipvaft ) .eq. 2 )  THEN
		found = .true.
	      ELSE
		ipvaft = ipvaft + 1
	    END IF
	END DO
	IF  ( .not. found )  ipvaft = ipcaft
C
C*	If the pointer is at a number this is the visibility.
C
	IF  ( iftype (ipvaft) .eq. 2 )  THEN
	    ivis1  = ifintg (ipvaft )
	    ipvaft = ipvaft + 1
C
C*	    Return if there is no visibility information.
C
	  ELSE
	    RETURN
	END IF
C
C*	Check for a two part visibility.  If the next field is a
C*	number followed by a slash and another number, this is the
C*	fractional visibility.
C
	IF  ( ( ipvaft + 2 .le. iptbef ) .and. 
     +	      ( iftype ( ipvaft     ) .eq. 2 ) .and.
     +	      ( iftype ( ipvaft + 1 ) .eq. 3 ) .and.
     +	      ( iftype ( ipvaft + 2 ) .eq. 2 ) )  THEN
	    ivis2 = ifintg ( ipvaft )
	    ivis3 = ifintg ( ipvaft + 2 )
	    IF  ( ivis3 .ne. 0 )  THEN
		vsby = FLOAT (ivis1) + ( FLOAT (ivis2) / FLOAT (ivis3) )
	    END IF
	    ipvaft = ipvaft + 3
C
C*	    Check for a slash followed by a number.  This means that
C*	    ivis1 contains the whole visibility plus the numerator of
C*	    the fraction.
C
	  ELSE IF  ( ( ipvaft + 1 .le. iptbef ) .and.
     +		     ( iftype ( ipvaft ) .eq. 3 ) .and.
     +		     ( iftype ( ipvaft + 1 ) .eq. 2 ) )  THEN
	    ivis2 = MOD ( ivis1, 10 )
	    ivis1 = ivis1 / 10
	    ivis3 = ifintg ( ipvaft + 1 )
	    IF  ( ivis3 .ne. 0 )  THEN
		vsby = FLOAT (ivis1) + ( FLOAT (ivis2) / FLOAT (ivis3) )
	    END IF
	    ipvaft = ipvaft + 2
C
C*	    Check for decimal place followed by number.
C
	  ELSE IF  ( cfield ( ipvaft ) .eq. '.' )  THEN
	    ipvaft = ipvaft + 1
	    IF  ( ( iftype ( ipvaft ) .eq. 2 ) .and.
     +		  ( ipvaft .le. iptbef ) )  THEN
		CALL ST_LSTR  ( cfield ( ipvaft ), lenv, ier )
		llll = -lenv
		vis2 = FLOAT ( ifintg ( ipvaft ) ) * (10. ** llll)
		vsby = FLOAT ( ivis1 ) + vis2
		ipvaft = ipvaft + 1
	      ELSE
		vsby = FLOAT ( ivis1 )
	    END IF
C
C*	    If the next field is the weather or we are at the numeric
C*	    group, this is a whole value for visibility.
C
	  ELSE IF  ( ( iftype ( ipvaft ) .eq. 1 ) .or.
     +		     ( ipvaft .eq. iptbef + 1 ) )  THEN
	    vsby = FLOAT ( ivis1 )
	END IF
C
C*	Check for visibility with a +.  Eliminate these values since
C*	they mean "at least" rather than an actual distance.  Also
C*	eliminate V after visibility so it will not be included in
C*	weather codes.
C
	IF  ( cfield (ipvaft) (1:1) .eq. '+' )  THEN
	    vsby = RMISSD
	    cfield (ipvaft) = cfield (ipvaft) (2: )
	  ELSE IF  ( cfield (ipvaft) (1:2) .eq. '.+' )  THEN
	    vsby = RMISSD
	    cfield (ipvaft) = cfield (ipvaft) (3: )
	  ELSE IF  ( cfield (ipvaft) (1:1) .eq. 'V' )  THEN
	    cfield (ipvaft) = cfield (ipvaft) (2: )
	END IF
C*
	RETURN
	END
