	SUBROUTINE RU_STMP  ( report, lenr, above, ipoint, data, nlev,
     +			      iret )
C************************************************************************
C* RU_STMP								*
C*									*
C* This subroutine decodes significant temperature level data above	*
C* the surface.  ABOVE indicates whether the data being decoded are	*
C* above 100 mb (used to decode pressure properly).  The output is	*
C* ordered PRES TEMP DWPT.						*
C*									*
C* RU_STMP  ( REPORT, LENR, ABOVE, IPOINT, DATA, NLEV, IRET )		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Station message			*
C*	LENR		INTEGER		Length of message		*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer to next field		*
C*									*
C* Output parameters:							*
C*	DATA (3,NLEV)	REAL		Significant data		*
C*	NLEV		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C*
	CHARACTER*(*)	report
	LOGICAL		above
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
     +					( ier  .ne. 0 )  .or.
     +					(field (1:5) .eq. '41414') .or.
     +					(field (1:5) .eq. '51515') )
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
C*	Skip will be set when levels are received out of order.
C
	skip = .false.
	DO WHILE  ( ier .eq. 0 )
C
C*	    Read in the next field.
C
	    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
C
C*	    Check for the end of the message.
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
C*		    field.  If last level was out of order, 
C*		    eliminate last data.
C
		  ELSE IF  (  level .le. 0 )  THEN
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
C*		If the level was decoded, get the temperature/dewpoint
C*		 field.
C
		IF  ( gettmp )  THEN
		    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, 
     +				    ier )
		    IF  ( lenf .ge. 5 )  THEN
			CALL RU_TEMP  ( field, t, td, jer )
C
C*			If temperature is missing, don't store the data.
C
			IF  ( ERMISS ( t ) )  THEN
			    skip = .false.
			  ELSE
			    nlev = nlev + 1
			    IF  ( (nlev .eq. 1) .and. (level .ne. 0) 
     +				  .and. ( .not. above ) )  nlev = 2
			    data ( 1, nlev ) = pres
			    data ( 2, nlev ) = t
			    data ( 3, nlev ) = td
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
