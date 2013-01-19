	SUBROUTINE MT_PKWD ( stpkwd, iret )
C************************************************************************
C* MT_PKWD							        *
C*								        *
C* This subroutine decodes peak wind direction, speed and time. 	*
C* The values are stored in common.					*
C*								        *
C* As of 2/97, many stations are not following the METAR specifications	*
C* for encoding the peak wind.  Logging level of decode errors was set	*
C* higher in calling program MT_RMKS for the time being.		*
C*								        *
C* MT_PKWD ( STPKWD, IRET )				       	        *
C*								        *
C* Input parameters:						        *
C*	STPKWD		CHAR*		Peak wind string		*
C*	IRHOUR		INTEGER		Hour of report                  *
C*								        *
C* Output parameters:						        *
C*      RIVALS(IRPWDR) 	REAL		Peak wind direction             *
C*	RIVALS(IRPWSP)	REAL		Peak wind speed (knots)         *
C*	RIVALS(IRPWHR)	REAL		Hour of peak wind               *
C*	RIVALS(IRPWMN)	REAL		Minutes of peak wind            *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C*					  33 = decode error 	        *
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	12/95	Original author		                *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP	 5/97	Removed ERMISS reference to integer arg *
C* D. Kidwell/NCEP	 6/97	ST_LSTR -> INDEX and ST_CRNM -> ST_INTG *
C* D. Kidwell/NCEP       4/98   New interface                           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	stpkwd
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	IF ( ERMISS ( rivals ( irpwdr ) ) .or. 
     +       ERMISS ( rivals ( irpwsp ) ) .or.
     +	     ERMISS ( rivals ( irpwhr ) ) .or. 
     +	     ERMISS ( rivals ( irpwmn ) ) ) THEN
C
C*	    Determine length of direction and speed subfield.
C
	    iloc = INDEX ( stpkwd, '/' )
	    IF ( ( iloc .eq. 6 ) .or. ( iloc .eq. 7 ) ) THEN
	        ldir = 3
	      ELSE IF ( iloc .eq. 5 ) THEN
C
C*	        Check for old format (2-digit wind direction).
C
	        ldir = 2
	      ELSE
	        iret = -2
	    END IF
C
C*	    Get peak wind direction and speed.
C
	    IF ( iret .eq. 0 ) THEN
	        CALL ST_INTG ( stpkwd ( 1:ldir ), ipkwdr, jret )
		IF ( ( jret .eq. 0 ) .and. ( ldir .eq. 2 ) )
     +		       ipkwdr = ipkwdr * 10
		rivals ( irpwdr ) = FLOAT ( ipkwdr )
	        CALL ST_INTG ( stpkwd ( ldir+1:iloc-1 ), ipkwsp, kret )
		rivals ( irpwsp ) = FLOAT ( ipkwsp )
	        iret = MIN ( jret, kret )
	    END IF
C
C*	    Get time of peak wind.
C
	    IF ( iret .eq. 0 ) THEN
	        lens = INDEX ( stpkwd ( iloc + 1: ), ' ' ) - 1
C
C*	        Look for extraneous '/' at end of field.
C
	        IF ( INDEX ( stpkwd ( iloc + 1: ) , '/' ) .ne. 0 )
     +               lens = lens - 1
C
		IF ( ( lens .eq. 2 ) .and. ( irhour .ne. IMISSD ) ) THEN
		    rivals ( irpwhr ) = FLOAT ( irhour )
		    kret = 0
		  ELSE IF ( lens .eq. 4 ) THEN
		    CALL ST_INTG (stpkwd (iloc+1:iloc+2), ipkwhr, kret )
		    rivals ( irpwhr ) = FLOAT ( ipkwhr )
		  ELSE    
		    kret = -2
		END IF
C
		IF ( kret .eq. 0 ) THEN
        	    CALL ST_INTG ( stpkwd ( iloc+lens-1:iloc+lens ),
     +			           ipkwmn, kret)
		    rivals ( irpwmn ) = FLOAT ( ipkwmn )
		END IF
	        iret = MIN ( iret, kret )
	    END IF
	END IF
	IF ( iret .ne. 0 ) iret = 33
C*
	RETURN
	END
