	SUBROUTINE RU_MTRP  ( report, lenr, wnknot, above, ipoint, data,
     +			      nlev, iret )
C************************************************************************
C* RU_MTRP								*
C*									*
C* This subroutine decodes data for the tropopause level(s) from the 	*
C* TTAA reports.  The output data are ordered PRES TEMP DWPT DRCT SPED.	*
C*									*
C* RU_MTRP  ( REPORT, LENR, WNKNOT, ABOVE, IPOINT, DATA, NLEV, IRET )	*
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
C*	DATA (5,NLEV)	REAL		Tropopause station data		*
C*	NLEV		INTEGER		Number of tropopauses		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/01			                        *
C* D. Kidwell/NCEP	 3/01	Treat spurious 0 wind values as missing *
C* D. Kidwell/NCEP	 2/05	Added drop (925mb) flag to RU_MAND call *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C*
	CHARACTER*(*)	report
	LOGICAL		wnknot, above
	REAL		data (5,*)
C*
	REAL		rdata (6)
	CHARACTER	pp*2, field*10
	LOGICAL		done
C*
	LOGICAL		ENDRPT, ENDMXW
C*
	INCLUDE		'ERMISS.FNC'
C
C*	Function to check for end of message.
C
	ENDRPT ( field, lenf, ier ) =  
     +				( ( lenf .gt. MAXFLN )  .or.
     +				  ( ier  .ne. 0 )  .or. 
     +				  ( field (1:2) .eq. '66' )   .or.
     +				  ( field (1:2) .eq. '77' )   .or.
     +				  ( field (1:5) .eq. '51515' ) )
	ENDMXW ( field, lenf, ier ) =  ( ( field (1:2) .eq. '77' ) .or.
     +					 ( field (1:2) .eq. '66' ) )
C------------------------------------------------------------------------
	iret = 0
	nlev = 0
	done = .false.
C
C*	Read tropopause data level by level.
C
	DO WHILE ( .not. done )
	    ipsave = ipoint
C
C*	    Get the first field which contains the pressure.
C
	    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
C
C*	    Check for the end of message.
C
	    IF  ( ENDRPT ( field, lenf, ier ) )  THEN
		IF ( .not. ENDMXW ( field, lenf, ier ) ) THEN
		    ipoint = lenr + 1
		  ELSE
		    ipoint = ipsave
		END IF
		RETURN
	      ELSE
	    	pp = field ( 1:2 )
	    END IF
C
C*	    Check if this is a tropopause level and for pressure value.
C
	    IF ( pp .eq. '88' ) THEN
      	        IF ( field ( 3:lenf ) .ne. '999' ) THEN
C
C*		    This was the expected level.  Parse it.
C
		    CALL RU_MAND  ( field, report, lenr, wnknot, 1, 
     +			            .false., .false., ipoint, rdata,
     +				    ier )
C
C*		    If there is data, add this level to data.
C
		    IF ( ( .not. ERMISS ( rdata ( 1 ) ) ) .and. 
     +		         ( .not. ERMISS ( rdata ( 6 ) ) ) ) THEN
		        nlev = nlev + 1
			IF ( above ) rdata ( 6 ) = rdata ( 6 ) / 10.
		        data ( 1, nlev ) = rdata ( 6 )
			IF ( ( rdata ( 4 ) .eq. 0. ) .and.
     +			     ( rdata ( 5 ) .eq. 0. ) ) THEN
			    rdata ( 4 ) = RMISSD
			    rdata ( 5 ) = RMISSD
			END IF
		        DO ii = 2, 5
			    data ( ii, nlev ) = rdata ( ii )
		        END DO
		        IF ( nlev .eq. LLMXLV ) done = .true.
		    END IF
	          ELSE
		    RETURN
		END IF
	    END IF
	END DO
C*
	RETURN
	END
