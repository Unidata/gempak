	SUBROUTINE RU_SHDR  ( report, lenr, irpnt, part, istnm, stid,
     +		 	      iday, ihour, wnknot, itopwn, xlat, xlon,
     +			      iret )
C************************************************************************
C* RU_SHDR								*
C*									*
C* This subroutine decodes the header from an upper-air TT, PP, UU, 	*
C* or XX report.  The fields after IRPNT are the part type, the 	*
C* station time and the station number.  TOPWND is the hundreds digit 	*
C* for TTAA data and the tens digit for TTCC data.  On return, IRPNT	*
C* points to the first field after the station number.			*
C*									*
C* RU_SHDR  ( REPORT, LENR, IRPNT, PART, ISTNM, STID, IDAY, IHOUR, 	*
C*            WNKNOT, ITOPWN, XLAT, XLON, IRET )			*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Station report			*
C*	LENR		INTEGER		Length of report		*
C*									*
C* Input and output parameters:						*
C*	IRPNT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	PART		CHAR*4		Part name			*
C*	ISTNM		INTEGER		Station number			*
C*	STID		CHAR*8		Station identifier		*
C*	IDAY		INTEGER		Observation day			*
C*	IHOUR		INTEGER		Observation hour		*
C*	WNKNOT		LOGICAL		Flag for speed in knots		*
C*	ITOPWN		INTEGER		Pressure for last wind report	*
C*	XLAT		REAL		Mobile platform latitude	*
C*	XLON		REAL		Mobile platform longitude	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = no report			*
C*					 -3 = invalid group		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 9/87						*
C* B. Doty/RDS		11/87	Added check for NIL=			*
C* D. Kidwell/NCEP	 3/01	Added dropsonde ('XX') processing       *
C* D. Kidwell/NCEP	 5/01	Added ship ('UU'), return stid		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, part, stid
	LOGICAL		wnknot, mobile, ship
C*
	CHARACTER	field*6, p1*2, p2*2, strg*12
C------------------------------------------------------------------------
	iret   = 0
	mobile = .false.
	ship   = .false.
	stid   = ' '
C
C*	Get the first field which identifies the part.
C
	part  = report ( irpnt: irpnt + 3 )
	irpnt = irpnt + 4
	p1    = part (1:2)
	p2    = part (3:4)
	IF  ( ( ( p1 .ne. 'TT' ) .and. ( p1 .ne. 'PP' ) )  .or.
     +	      ( ( p2 .ne. 'AA' ) .and. ( p2 .ne. 'BB' ) .and.
     +	        ( p2 .ne. 'CC' ) .and. ( p2 .ne. 'DD' ) ) )  THEN
	    IF ( ( p1 .eq. 'XX' ) .and. 
     +		 ( ( p2 .eq. 'AA' ) .or. ( p2 .eq. 'BB' ) ) ) THEN
		mobile = .true.
C
C*		For dropsonde decoding, treat 'XX' as 'TT'.
C
		part ( 1:2 ) = 'TT'
	      ELSE IF ( ( p1 .eq. 'UU' ) .and.
     +		 ( ( p2 .eq. 'AA' ) .or. ( p2 .eq. 'BB' ) .or.
     +		   ( p2 .eq. 'CC' ) .or. ( p2 .eq. 'DD' ) ) ) THEN
C
C*		Use MOBILE flag for mobile lat/lon decoding, and use
C*		SHIP flag for obtaining platform call sign.
		ship   = .true.
		mobile = .true.
C
C*              For ship decoding, treat 'UU' as 'TT'.
C
                part ( 1:2 ) = 'TT'
	      ELSE
	        iret = -3
	        RETURN
	    END IF
	  ELSE
	    mobile = .false.
	END IF
C
C*	If a ship sounding, call sign comes before date group.
C
	IF ( ship ) THEN
	    CALL RU_GFLD  ( report, lenr, irpnt, field, lenshp, iret )
	    IF ( lenshp .le. 8 ) THEN
	        stid = field ( 1:lenshp )
	      ELSE
	        iret = -3
	        RETURN
	    END IF
	END IF
C
C*	Get the second field, which contains the day, hour, wind type,
C*	and wind indicator.
C
	CALL RU_GFLD  ( report, lenr, irpnt, field, lenf, iret )
	IF  ( ( iret .ne. 0 ) .or. ( lenf .lt. 5 ) )  THEN
	    iret = -3
	    RETURN
	  ELSE
C
C*	    The first two characters are the day.  If the day is
C*	    greater than 50, the wind is in knots.
C
	    CALL ST_INTG  ( field (1:2), intg, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -3
		RETURN
	      ELSE
		iday = intg
		IF ( intg .gt. 50 ) THEN
		    iday = iday - 50
		    wnknot = .true.
		  ELSE
		    wnknot = .false.
		END IF
	    END IF
C
C*	    The second two characters are the hour.
C
	    CALL ST_INTG  ( field (3:4), intg, ier )
	    IF  ( ier .ne. 0 )  THEN
		iret = -3
		RETURN
	      ELSE
		ihour = intg
	    END IF
C
C*	    The last character is the wind indicator.
C
	    CALL ST_INTG  ( field (5:5), intg, ier )
	    IF  ( ier .eq. 0 )  THEN
		itopwn = intg
	      ELSE
		itopwn = IMISSD
	    END IF
	END IF
C
C*	The next field contains the station number, if not a 
C*	mobile platform (e.g., dropsonde or ship).
C
	IF ( .not. mobile ) THEN
	    CALL RU_GFLD  ( report, lenr, irpnt, field, lenf, ier )
	    IF  ( ( ier .ne. 0 ) .or. ( lenf .lt. 5 ) )  THEN
	        iret = -3
	        RETURN
	      ELSE
	        CALL ST_INTG  ( field (1:5), istnm, ier )
	        IF  ( ier .ne. 0 )  THEN
		    iret = -3
		    RETURN
	        END IF
	    END IF
	    xlat = RMISSD
	    xlon = RMISSD
	  ELSE
C
C*	    The report is mobile.  Get the location.
C
	    CALL RU_DROP  ( report, lenr, irpnt, xlat, xlon, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -3
		RETURN
	    END IF
C    
C*	    If this is a ship, use the call sign stid, and set station
C*	    number to missing.
C
	    IF ( ship ) THEN
C
C*	        Use IMISSD as station number for ships.
C
	        istnm = IMISSD
	        IF ( stid .eq. 'SHIP' ) THEN
C
C*	            For platforms called "SHIP", create a semi-unique ID
C*	            using lat and lon.
C
		    inum = ABS ( xlat * 10 ) * 10000 + ABS ( xlon * 10 )
		    CALL ST_INLN ( inum, strg, lenf, ier ) 
		    nn = 7 - lenf
		    DO ii = 1, nn
			strg = '0' // strg ( :lenf )
			lenf = lenf + 1
		    END DO
		    stid ( 2:8 ) = strg ( 1:7 )
		  ELSE
C
C*		    Append the observation hour to the ship call sign to
C*		    make a unique id.
C
		    CALL ST_INLN ( ihour, strg, lenf, ier )
		    IF ( lenf .eq. 1 ) strg ( 1:2 ) = '0' // strg ( :1 )
		    IF ( lenshp .gt. 6 ) lenshp = 6
		    stid = stid ( 1:lenshp ) // strg ( 1:2 )
	        END IF
	      ELSE
C    
C*	        Set the dropsonde id to the hour.  This will be used
C*	        later to generate an id from the section beginning 
C*		with 61616.
C    
	        istnm = ihour
	    END IF
	END IF
C
C*	Check for a NIL report.
C 
	IF  ( report (irpnt:irpnt+3) .eq. 'NIL=' )  iret = -2
C*
	RETURN
	END
