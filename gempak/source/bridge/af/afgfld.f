	SUBROUTINE AF_GFLD  ( report, lenr, irptr, field, lenf, iret )
C************************************************************************
C* AF_GFLD								*
C*									*
C* This subroutine returns the next field from REPORT that begins at	*
C* or after pointer IRPTR.  The fields in REPORT must be separated by	*
C* blanks.  Any blanks between IRPTR and the start of the next field	*
C* will not be returned.  On output, the pointer IRPTR will point to	*
C* the first non-blank character in REPORT after the returned field.	*
C*									*
C* AF_GFLD  ( REPORT, LENR, IRPTR, FIELD, LENF, IRET )			*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Report 				*
C*	LENR		INTEGER		Length of REPORT		*
C*									*
C* Input and output parameters:						*
C*	IRPTR		INTEGER		Pointer within REPORT 		*
C*									*
C* Output parameters:							*
C*	FIELD		CHAR*		Next field 			*
C*	LENF		INTEGER		Length of FIELD 		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = no more fields in	REPORT	*
C*					 -2 = LENF for next field is	*
C*					      larger than MXLENF	*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report, field
C*
	LOGICAL		start
C------------------------------------------------------------------------
	iret  = 0
C
C*	Find the start of the next field.
C
	start = .false.
	DO WHILE  ( .not. start )
	    IF  ( irptr .gt. lenr )  THEN
		iret = -1
		RETURN
	    ELSE IF  ( report ( irptr : irptr ) .ne. ' ' )  THEN
		start = .true.
	    ELSE
		irptr = irptr + 1
	    END IF
	END DO
C
C*	Search for a blank to signal the end of this field.  If no blank
C*	is found, then the end of the field is at the end of the report.
C
	iend = INDEX ( report ( irptr : lenr ),  ' ' )
	IF  ( iend .eq. 0 ) THEN
	    iend = lenr
	ELSE
	    iend = iend + irptr - 2
	END IF
C
C*	Check that the length of the field is less than or equal to
C*	MXLENF characters.
C
	lnxfld = iend - irptr + 1
	IF  ( lnxfld .gt. MXLENF )  THEN
	    logmsg = 'Group ' //  report ( irptr : iend )  //
     +		' is too big'
	    CALL DC_WLOG  ( 2, 'AF', 1, logmsg, ierwlg )
	    iret = -2
	    RETURN
	END IF
C
C*	Set output values.
C
	field = report ( irptr : iend )
	lenf = iend - irptr + 1
	irptr = iend + 1
C
C*	Update the pointer to point to the first non-blank character
C*	after the returned field.
C
	DO WHILE  ( ( report (irptr:irptr) .eq. ' ' ) .and.
     +			( irptr .le. lenr )  ) 
		irptr = irptr + 1
	END DO
C*
	RETURN
	END
