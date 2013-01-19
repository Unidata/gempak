	SUBROUTINE MT_SLP ( stslp, iret )
C************************************************************************
C* MT_SLP							        *
C*								        *
C* This subroutine decodes sea level pressure.  No hundreds digit is 	*
C* given, so altimeter setting is used to determine the proper hundreds *
C* digit.  The value is stored in common.				*
C*								        *
C* MT_SLP ( STSLP, IRET )				       	        *
C*								        *
C* Input parameters:						        *
C*	STSLP		CHAR*		Possible SLP string		*
C*								        *
C* Output parameters:						        *
C*      RIVALS(IRPMSL)    REAL		Sea level pressure (hPa)        *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C*					  27 = decode error 	        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	11/95	Original author		                *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* K. Tyle/GSC		 3/97	Add rules for assigning hundreds' digit	*
C* D. Kidwell/NCEP       6/97   Replaced ST_CRNM with ST_INTG           *
C* D. Kidwell/NCEP       4/98   New interface                           *
C* S. Jacobs/NCEP	 2/99	Rewrote algorithm to compute MSL press	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	stslp
C*
	LOGICAL		found
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Convert the coded value to an integer. If there is a problem
C*	return with a warning.
C
	CALL ST_INTG ( stslp ( 1:3 ), islprs, iret )
	IF ( iret .ne. 0 ) THEN
	    iret = 27
	    RETURN
	END IF
	slp = ( FLOAT ( islprs ) * .1 )
C
C*	Check for the altimeter setting in millibars or inches.
C*	If neither exists, then the mean sea level pressure can have
C*	the range of 980-1049 mb.
C*
C*	If the altimeter exists, get the value in millibars.
C
	IF  ( ERMISS ( rivals ( iraltm ) ) )  THEN
	    IF  ( ERMISS ( rivals ( iralti ) ) )  THEN
		IF  ( slp .lt. 50.0 )  THEN
		    rivals ( irpmsl ) = 1000.0 + slp
		  ELSE IF  ( slp .ge. 80.0 )  THEN
		    rivals ( irpmsl ) =  900.0 + slp
		END IF
		RETURN
	      ELSE
		altmtr = PR_ALTM ( rivals ( iralti ) )
	    END IF
	  ELSE
	    altmtr = rivals ( iraltm )
	END IF
C
C*	Check possible pressure values. Start with 1000+coded value.
C*	Loop until the mean sea level pressure and the altimeter differ
C*	by less than 20 mb or the msl pressure is out of range.
C
	pmsl  = 1000.0 + slp
	found = .false.
	DO WHILE  ( .not. found .and. ( pmsl .gt. 800.0 ) )
	    IF  ( ABS ( pmsl - altmtr ) .le. 20.0 )  THEN
		found = .true.
		rivals ( irpmsl ) = pmsl
	    END IF
	    pmsl = pmsl - 100.0
	END DO
C
C*	If a value for the msl pressure was not found, then the 
C*	coded value for the msl pressure and the coded value for 
C*	the altimeter are in question. Therefore, reset the
C*	altimeter values to missing.
C
	IF  ( .not. found )  THEN
	    rivals ( iraltm ) = RMISSD
	    rivals ( iralti ) = RMISSD
	END IF
C*
	RETURN
	END
