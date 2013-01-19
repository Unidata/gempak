 	SUBROUTINE RA_WIND  ( irpntr, sknt, drct, gust, ipwbef,
     +			      ipwaft, iret )
C************************************************************************
C* RA_WIND								*
C*									*
C* This subroutine finds the wind information in an airways report.	*
C* The first numeric field of length 4 is assumed to be the wind	*
C* field.  Pointers to the first field before and after the wind	*
C* field are returned.							*
C*									*
C* RA_WIND  ( IRPNTR, SKNT, DRCT, GUST, IPWBEF, IPWAFT, IRET )		*
C*									*
C* Input parameters:							*
C*	IRPNTR		INTEGER		First field after header	*
C*									*
C* Output parameters:							*
C*	SKNT		REAL		Wind speed in knots		*
C*	DRCT		REAL		Wind direction in degrees	*
C*	GUST		REAL		Wind gust as reported		*
C*	IPWBEF		INTEGER		Field before wind		*
C*	IPWAFT		INTEGER		Field after wind		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = no wind group found    	*
C**									*
C* Log:									*
C* B. Doty/RDS		 9/87	GEMPAK 4				*
C* I. Graffman/RDS	 3/88						*
C* M. desJardins/GSFC	 9/89	GEMPAK5					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
	LOGICAL		good
C----------------------------------------------------------------------
	iret   = 0 
	ipwbef = 0
	ipwaft = 0 
	sknt   = RMISSD
	drct   = RMISSD
	gust   = RMISSD
C
C*	Find first four digit number after the pointer field.
C 
	good  = .false.
	iwind = irpntr
	DO WHILE  ( ( .not. good ) .and. ( iwind .le. nfield ) )
	    IF  ( ( iftype (iwind) .eq. 2 ) .and. 
     +		  ( ifsize (iwind) .eq. 4 ) )  THEN
		good = .true.
	      ELSE
		iwind = iwind + 1
	    END IF
	END DO
C
C*	If there are more than 4 slashes already, this is not the wind.
C
	IF  ( good )  THEN
	    islash = 0
	    DO  i = irpntr, iwind
		IF  ( iftype (i) .eq. 3 )  islash = islash + 1
	    END DO
	    IF  ( islash .gt. 4 )  good = .false.
	END IF
C
C*	If found, get the wind.
C
	IF  ( good )  THEN
	    ival   = ifintg (iwind)
	    idrctn = ival / 100
	    ispeed = MOD ( ival, 100 )
C
C*	    Check numbers for validity.
C
	     IF  ( idrctn .gt. 36 )  THEN 
		IF  (( idrctn .lt. 51 ) .or. ( idrctn .gt. 86 ))  THEN
		    good = .false.
		  ELSE
		    ispeed = ispeed + 100
		    idrctn = idrctn - 50 
		END IF
	      ELSE
		IF  ( idrctn .lt. 0 )  good = .false.
		IF  ( idrctn .eq. 36 )  idrctn = 0
	    END IF
	    IF  ((ispeed .eq. 0) .and. (idrctn .ne. 0))  good = .false.
	END IF
C
C*	Check for estimated winds and look for gusts.
C
	IF  ( good )  THEN
	    ipwbef = iwind - 1
C
C*	    Move pointer for special case of estimated winds.
C
	    IF  ( ( iftype (ipwbef) .eq. 1 ) .and. 
     +		  ( cfield (ipwbef) .eq. 'E' ) )  THEN
		ipwbef = ipwbef - 1
	    END IF
	    ipwaft = iwind + 1
	    sknt   = FLOAT (ispeed)
	    drct   = FLOAT (idrctn) * 10.
C
C*	    Gusts are reported as a G followed by a number.
C
	    IF  ( ipwaft .lt. nfield )  THEN
		IF  ( ( cfield (ipwaft) .eq. 'G' ) .or.
     +		      ( cfield (ipwaft) .eq. '+' ) )  THEN
		    ipwaft = ipwaft + 1
		    IF  ( iftype (ipwaft) .eq. 2 )  THEN
			gust   = ifintg (ipwaft)
			ipwaft = ipwaft + 1
		    END IF
		END IF
	    END IF
	END IF
C
C*	If wind was not found, check for pattern:  #/#//
C*	Assume that the two slashes indicate missing wind.
C
	IF  ( .not. good )  THEN
	    iwind = irpntr + 3
	    DO WHILE  ( ( .not. good ) .and. ( iwind .lt. nfield ) )
		IF  ( ( iftype (iwind)   .eq. 3 ) .and.
     +		      ( iftype (iwind+1) .eq. 3 ) .and.
     +		      ( iftype (iwind-1) .eq. 2 ) .and.
     +		      ( iftype (iwind-2) .eq. 3 ) .and.
     +		      ( iftype (iwind-3) .eq. 2 ) )  THEN
		    ipwbef = iwind
		    ipwaft = iwind + 1
		    good   = .true.
		  ELSE
		    iwind  = iwind + 1
		END IF
	    END DO
	END IF
C
C*	If not valid, set the return code.
C
	IF  ( .not. good )  iret = -3
C*
	RETURN
	END
