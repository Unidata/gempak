	SUBROUTINE RU_SIGW ( report, lenr, wnknot, above, ipoint, data, 
     +			     nlev,   iret )
C************************************************************************
C* RU_SIGW								*
C*									*
C* This subroutine decodes significant wind data on height surfaces.	*
C* The output is ordered HGHT DRCT SPED.				*
C*									*
C* RU_SIGW  ( REPORT, LENR, WNKNOT, ABOVE, IPOINT, DATA, NLEV, IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Sig wind report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	DATA (3,NLEV)	REAL		Sig wind data			*
C*	NLEV		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C*
	REAL		data (3,*)
	CHARACTER*(*)	report
	LOGICAL		wnknot, above
C*
	LOGICAL		done
	CHARACTER	field*8
	REAL		hght (3)
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
	iret = 0
	nlev = 0
	done = .false.
C
C*	Initialize first level to missing for surface data.
C
	DO  i = 1, 3
	    data  ( i, 1 ) = RMISSD
	END DO
C
C*	Loop through the report.
C
	DO WHILE  ( .not. done )
C
C*	    Get the next field.
C
	    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
C
C*	    Check for end of report.
C
	    IF  ( ENDRPT ( field, lenf, ier ) )  THEN
		done = .true.
C
C*		Check that field is at least 5 characters long.
C
	      ELSE IF  ( lenf .ge. 5 ) THEN
C
C*		Get values of heights reporting.
C
		CALL RU_WHGT ( field, nwnd, hght, jer )
C
C*		Get wind for reporting heights.
C
		DO  i = 1, nwnd
C
C*		    Get wind report and decode it.
C
		    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, 
     +				    ier )
C
C*		    Check for end of report.
C
		    IF  ( ENDRPT ( field, lenf, ier ) )  THEN
			RETURN
C
C*			Decode wind if length is at least five.
C
		      ELSE IF  ( lenf .ge. 5 )  THEN
			CALL RU_WIND ( field, wnknot, dir, spd, jer )
C
C*			Add to data array if no data is missing.
C
			IF  ( ( .not. ERMISS ( dir ) ) .and.
     +			      ( .not. ERMISS ( spd ) ) .and.
     +			      ( .not. ERMISS ( hght ( i ) ) ) )  THEN
			    nlev = nlev + 1
			    IF ( ( nlev .eq. 1 ) .and. 
     +				 ( hght (i) .ne. 0. ) .and. 
     +				 ( .not. above ) )  nlev = 2
			    data ( 1, nlev ) = hght ( i )
			    data ( 2, nlev ) = dir
			    data ( 3, nlev ) = spd
			END IF
		    END IF
		END DO
	    END IF
	END DO
C*
	RETURN
	END
