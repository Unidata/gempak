	SUBROUTINE RU_MMXW  ( report, lenr, wnknot, above, ipoint, data,
     +			      nlev, iret )
C************************************************************************
C* RU_MMXW								*
C*									*
C* This subroutine decodes data for the max wind level(s) from the TTAA *
C* reports.  The output data are ordered PRES DRCT SPED.		*
C*									*
C* RU_MMXW  ( REPORT, LENR, WNKNOT, ABOVE, IPOINT, DATA, NLEV, IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Station report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for speed in knots		*
C*	ABOVE		LOGICAL		Above 100 mb flag               *
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	DATA (3,NLEV)	REAL		Max wind station data		*
C*	NLEV		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/01						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C*
	CHARACTER*(*)	report
	LOGICAL		wnknot, above
	REAL		data (3,*)
C*
	CHARACTER	pp*2, field*10
	LOGICAL		done
C*
	LOGICAL		ENDRPT
C*
	INCLUDE		'ERMISS.FNC'
C
C*	Function to check for end of message.
C
	ENDRPT ( field, lenf, ier ) =  
     +				( ( lenf .gt. MAXFLN )  .or.
     +				  ( ier  .ne. 0 )  .or. 
     +				  ( field (1:5) .eq. '51515' ) )
C------------------------------------------------------------------------
	iret = 0
	nlev = 0 
	done = .false.
C
C*	Read the max wind data level by level.
C
	DO WHILE ( .not. done )
C
C*	    Get the first field which contains the pressure.
C
	    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
C
C*	    Check for the end of message.
C
	    IF  ( ENDRPT ( field, lenf, ier ) )  THEN
		ipoint = lenr + 1
		RETURN
	      ELSE
		pp = field ( 1:2 )
	    END IF
C
C*	    Check if this is a max wind level and for a pressure value.
C
	    IF ( pp .eq. '77' .or. pp .eq. '66' ) THEN
      	        IF ( field ( 3:lenf ) .ne. '999' ) THEN
C
C*		    This was the expected level.  Parse it.
C
		    CALL ST_CRNM ( field ( 3:lenf ), pres, iret )
     		    IF ( .not. ERMISS ( pres ) ) THEN
		        CALL RU_GFLD  ( report, lenr, ipoint, field,
     +				        lenf, ier )
		        CALL RU_WIND  ( field, wnknot, dir, spd, ier )
C
C*		        Check for missing speed and direction.
C
		        IF ( ERMISS ( dir ) .and. ERMISS ( spd ) ) THEN
C
C*			    Do nothing.
C
		          ELSE
		            nlev = nlev + 1
			    IF ( above ) pres = pres / 10.
			    data ( 1, nlev ) = pres
			    data ( 2, nlev ) = dir
			    data ( 3, nlev ) = spd
			    IF ( nlev .eq. LLMXLV ) done = .true.
		        END IF
		    END IF
		  ELSE IF ( pp .eq. '77' ) THEN
		    RETURN
		END IF
	    END IF
	END DO
C*
	RETURN
	END
