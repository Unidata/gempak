	SUBROUTINE RU_PWND  ( report, lenr, wnknot, above, ipoint, 
     +			      data,   nlev, iret )
C************************************************************************
C* RU_PWND								*
C*									*
C* This subroutine decodes significant wind data on pressure surfaces.	*
C* ABOVE indicates whether the data being decoded are above 100 mb	*
C* (used to decode pressure properly).  The output data are ordered	*
C* PRES DRCT SPED .							*
C*									*
C* RU_PWND  ( REPORT, LENR, WNKNOT, ABOVE, IPOINT, DATA, NLEV, IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Station message			*
C*	LENR		INTEGER		Length of message		*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	DATA (3,NLEV)	REAL		Significant wind data		*
C*	NLEV		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/87						*
C* M. desJardins/GSFC	 4/89	Check for missing spd or dir		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C*
	CHARACTER*(*)	report
	LOGICAL		above, wnknot
	REAL		data ( 3, * )
C*
	LOGICAL		skip, gettmp
	CHARACTER	field*10
	LOGICAL		ENDRPT
C*
	INCLUDE		'ERMISS.FNC'
C
C*	Function to check for end of message.
C
	ENDRPT ( field, lenf, ier ) =  
     +				      ( ( lenf .gt. MAXFLN )  .or.
     +					( ier  .ne. 0 )       .or.
     +					( field (1:5) .eq. '41414') .or.
     +					( field (1:5) .eq. '51515' ) )
C------------------------------------------------------------------------
	iret   = 0
	nlev   = 0
	ier    = 0
C
C*	Set the next level expected.
C
	IF  ( above )  THEN
	    levexp = 1
	  ELSE
	    levexp = 0
	    DO  i = 1, 3
		data ( i, 1 ) = RMISSD
	    END DO
	END IF
C
C*	Loop through the levels reporting.
C*	SKIP will be set when levels are received out of order.
C
	skip = .false.
	DO WHILE  ( ier .eq. 0 )
C
C*	    Read in the next field.
C
	    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
C
C*	    Check for end of report.
C
	    IF  ( ENDRPT ( field, lenf, ier ) )  THEN
		RETURN
	    END IF
C
C*	    Get pressure and level number.
C
	    IF  ( lenf .ge. 5 )  THEN
		CALL RU_PLVL  ( field, above, level, pres, jer )
C
C*		If level is the expected level, set flags to process
C*		level.
C
		IF  ( level .eq. levexp )  THEN
		    gettmp = .true.
		    skip   = .false.
C
C*		    If this is an invalid level, do not process next 
C*		    field.  If last level was out of order, eliminate 
C*		    last data.
C
		  ELSE IF  ( level .le. 0 )  THEN
		    gettmp = .false.
		    IF  ( skip )  nlev = nlev - 1
		    skip   = .false.
		    levexp = -2
C
C*		    For valid levels received out of order, process data
C*		    but set flag to indicate that some levels were
C*		    skipped.  Check that pressure is decreasing.
C
		  ELSE IF ((nlev .eq. 0) .or. (pres .lt. data (1,nlev)))
     +								THEN
		    gettmp = .true.
		    IF  ( skip )  nlev = nlev - 1
		    skip   = .true.
		    levexp = level
C
C*		    Otherwise, this level is invalid.
C
		  ELSE
		    gettmp = .false.
		    IF  ( skip )  nlev = nlev - 1
		    skip   = .false.
		    levexp = -2
		END IF
C
C*		If the level was decoded, get the direction/speed.
C
		IF  ( gettmp )  THEN
		    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, 
     +				    ier )
C
C*		    Check for end of report.
C
		    IF  ( ENDRPT ( field, lenf, ier ) )  THEN
			RETURN
C
C*			Check length and decode wind field.
C
		      ELSE IF  ( lenf .ge. 5 )  THEN
			CALL RU_WIND  ( field, wnknot, dir, spd, jer )
C
C*			If direction is missing, don't store the data.
C
			IF ( ERMISS ( dir ) .or. ERMISS ( spd ) )  THEN
			    skip = .false.
			  ELSE
			    nlev = nlev + 1
			    IF  ( (nlev .eq. 1) .and. (level .ne. 0) 
     +				  .and. ( .not. above ) )  nlev = 2
			    data ( 1, nlev ) = pres
			    data ( 2, nlev ) = dir
			    data ( 3, nlev ) = spd
			END IF
		    END IF
C
C*		    Increment expected level counter.
C
		    levexp = levexp + 1
		    IF  ( levexp .gt. 9 )  levexp = 1
		END IF
C
C*		Reset skip flag if there are no levels found.
C
		IF  ( nlev .eq. 0 )  skip = .false.
	    END IF
	END DO
C*
	RETURN
	END
