	SUBROUTINE RU_MSFC  ( report, lenr, wnknot, ipoint, data, iret )
C************************************************************************
C* RU_MSFC								*
C*									*
C* This subroutine decodes the surface data from a TTAA report.		*
C* The output data are ordered  PRES  TEMP  DWPT  DRCT  SPED .		*
C*									*
C* RU_MSFC  ( REPORT, LENR, WNKNOT, IPOINT, DATA, IRET )		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*lenr	Station report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for speed in knots		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	DATA (5)	REAL		Mandatory surface data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = no data			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 8/87	Rewritten for GEMPAK 4			*
C* D. Kidwell/NCEP	 2/01	Initialized iret                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C*
	CHARACTER*(*)	report
	REAL		data ( * )
	LOGICAL		wnknot
C*
	CHARACTER	field*10
	LOGICAL		ENDRPT
C
C*	Function to check for end of message.
C
	ENDRPT ( field, lenf, ier ) =  
     +				( ( lenf .gt. MAXFLN )  .or.
     +				  ( ier  .ne. 0 )  .or.
     +				  ( field (1:2) .eq. '77' )   .or.
     +				  ( field (1:2) .eq. '88' )   .or.
     +				  ( field (1:5) .eq. '51515' ) )
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize the data.  Note that data (6) will store height.
C
	DO  i = 1, 6
	    data ( i ) = RMISSD
	END DO
C
C*	Save pointer for missing surface data.
C
	ipsave = ipoint
C
C*	Get the next field which is 99PPP.
C
	CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
C
C*	Check for the end of the report.
C
	IF  (  ENDRPT  ( field, lenf, ier ) )  THEN
	    ipoint = lenr + 1
	    iret   = -2
	    RETURN
	END IF
C
C*	Decode the surface pressure.
C
	IF  ( ( field (1:2) .eq. '99' ) .and. ( lenf .ge. 5 ) )  THEN
	    CALL ST_INTG  ( field (3:5), intg, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  ( intg .lt. 100 )  intg = intg + 1000
		data ( 1 ) = FLOAT ( intg )
	      ELSE
		ipoint = ipsave
		iret   = -2
		RETURN
	    END IF
	  ELSE
	    ipoint = ipsave
	    iret   = -2
	    RETURN
	END IF
C
C*	Get the next field which is a temperature group.
C
	CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
	IF  ( ENDRPT  ( field, lenf, ier ) )  THEN
	    ipoint = lenr + 1
	    RETURN
	  ELSE IF  ( lenf .ge. 5 )  THEN
	    CALL RU_TEMP  ( field, data (2), data (3), ier )
	END IF
C
C*	Get the next field which is a wind group.
C
	CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
	IF  ( ENDRPT  ( field, lenf, ier ) )  THEN
	    ipoint = lenr + 1
	    RETURN
	  ELSE IF  ( lenf .ge. 5 )  THEN
	    CALL RU_WIND  ( field, wnknot, data (4), data (5), ier )
	END IF
C*
	RETURN
	END
