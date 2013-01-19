	SUBROUTINE RU_PPAA ( report, lenr, wnknot, ipoint, data, nlev,
     +			     iret )
C************************************************************************
C* RU_PPAA								*
C*									*
C* This subroutine decodes PPAA reports.  These reports contain		*
C* wind data at mandatory levels below 100 mb.  The output data		*
C* are ordered  PRES DRCT SPED .					*
C*									*
C* RU_PPAA  ( REPORT, LENR, WNKNOT, IPOINT, DATA, NLEV, IRET )		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PPAA report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	DATA (3,NLEV)	REAL		Mandatory wind data		*
C*	NLEV		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/87	Written for GEMPAK4			*
C* M. desJardins/GSFC	 4/89	Add check for missing spd or dir	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C
	REAL		data ( 3, * )
	CHARACTER*(*)	report
	LOGICAL		wnknot
C*
	LOGICAL		above, done
	REAL		pres ( 3 )
	CHARACTER	field*10
	LOGICAL		ENDRPT
C*
	INCLUDE		'ERMISS.FNC'
C
C*	Function to check for end of message.
C
	ENDRPT ( field, lenf, ier ) =  ( ( lenf .gt. MAXFLN )  .or.
     +					 ( ier  .ne. 0 )       .or.
     +					 ( field (1:5) .eq. '51515' ) )
C------------------------------------------------------------------------
	iret  = 0
	above = .false.
	nlev  = 0
	pold  = 2000.
C
C*	Loop through report looking for data.
C
	done = .false.
	DO WHILE  ( .not. done )
C
C*	    Get next field which contains number of reports & pressures.
C
	    CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
	    IF  ( ENDRPT (field, lenf, ier) .or. ( lenf .ne. 5 ) )  THEN
		RETURN
	    END IF
C
C*	    Decode pressure field.
C
	    CALL RU_PPMW  ( field, above, pres, numb, ier )
	    IF  ( ier .ne. 0 )  THEN
		RETURN
	    END IF
C
C*	    Get wind reports.
C
	    DO  i = 1, numb
C
C*		Get and decode wind field.
C
		CALL RU_GFLD  ( report, lenr, ipoint, field, lenf, ier )
		IF  ( ENDRPT ( field, lenf, ier ) )  THEN
		    RETURN
		  ELSE IF  ( lenf .ge. 5 )  THEN
		    CALL RU_WIND ( field, wnknot, dir, spd, ier )
		  ELSE
		    ier = -1
		END IF
C
C*		Add this level to array.
C
		ppp = pres ( i )
		IF ( ( ier .eq. 0 ) .and. ( .not. ERMISS ( dir ) ) .and.
     +		     ( .not. ERMISS ( spd ) ) .and. 
     +		     ( ppp .lt. pold ) )  THEN
		    nlev = nlev + 1
		    data ( 1, nlev ) = ppp
		    data ( 2, nlev ) = dir
		    data ( 3, nlev ) = spd
		    pold = ppp
		END IF
	    END DO
	END DO
C*
	RETURN
	END
