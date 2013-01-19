	SUBROUTINE RU_MAND  ( field, report, lenr,  wnknot, topwnd, 
     +			      above, drop, ipoint, rdata, iret )
C************************************************************************
C* RU_MAND								*
C*									*
C* This subroutine decodes data from a single mandatory level.		*
C*									*
C* RU_MAND  ( FIELD, REPORT, LENR, WNKNOT, TOPWND, ABOVE, DROP, IPOINT,	*
C*            RDATA, IRET )			 			*
C*									*
C* Input parameters:							*
C*	FIELD		CHAR*		Field containing pressure	*
C*	REPORT		CHAR*		Station report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for speed in knots		*
C*	TOPWND		REAL		Highest level reporting winds	*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*	DROP		LOGICAL		Dropsonde flag                  *
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	RDATA (6)    	REAL		Mandatory station data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid group encountered	*
C*					 -5 = end of report reached	*
C**									*
C* Log:									*
C* B. Doty/RDS		10/87						*
C* M. desJardins/GSFC	12/87						*
C* D. Kidwell/NCEP	 3/99	Check for zero height and all missing   *
C* D. Kidwell/NCEP	 2/05	CSC for drop, added to RU_HGHT call     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'rucmn.cmn'
C*
	CHARACTER*(*)	report, field
	LOGICAL		wnknot, above, drop
C*
	REAL		rdata ( 6 )
	CHARACTER	fld*10
	LOGICAL		allmsg
C*
	LOGICAL		ENDRPT
C*
	INCLUDE		'ERMISS.FNC'
C
C*	Function to check for end of message.
C
	ENDRPT ( fld, lenf, ier ) =  ( ( lenf .gt. MAXFLN )  .or.
     +					 ( ier  .ne. 0 )  .or.
     +					 ( fld (1:5) .eq. '51515' ) )
C------------------------------------------------------------------------
	iret = 0 
C
C*	Initialize all fields to missing.
C
	DO i = 1, 6
	    rdata (i) = RMISSD
	ENDDO
C
C*	Parse the pressure/height group.
C
	CALL RU_HGHT  ( field, above, drop, rdata (1), rdata (6), ier )
	p = rdata (1)
C
C*	Get the temperature/dewpoint group.
C
	CALL RU_GFLD  ( report, lenr, ipoint, fld, lenf, ier )
	IF  ( ENDRPT ( fld, lenf, ier ) )  THEN
	    iret   = -5
	    RETURN
	END IF
	IF  ( ( lenf .lt. 5 ) .or. ( lenf .gt. 7 ) )  THEN 
	    iret = -3
	    RETURN
	END IF
C
C*	Parse the temperature/dewpoint group.
C
	CALL RU_TEMP  ( fld, rdata (2), rdata (3), ier )
C
C*	Handle the wind group if it is present as determined by topwnd.
C
	IF  ( p .ge. topwnd )  THEN
C
C*	    Get the wind group.
C
	    CALL RU_GFLD  ( report, lenr, ipoint, fld, lenf, ier )
	    IF  ( ENDRPT ( fld, lenf, ier ) )  THEN
		iret = -5
		RETURN
	    END IF
	    IF  ( ( lenf .lt. 5 ) .or. ( lenf .gt. 7 ) )  THEN 
		iret = -3
		RETURN
	    ENDIF
C
C*	    Parse the wind group.
C
	    CALL RU_WIND  ( fld, wnknot, rdata (4), rdata (5), ier )
	END IF
C
C*	Check for zero height and remaining fields missing.  If found,
C*	set height missing also.
C
	allmsg = .true.
	DO i = 2, 5
	    IF ( .not. ERMISS ( rdata ( i ) ) ) allmsg = .false.
	END DO
	IF ( allmsg .and. ( MOD ( NINT ( rdata (6) ), 1000 ) .eq. 0 ) )
     +	     rdata ( 6 ) = RMISSD
C*
	RETURN
	END
