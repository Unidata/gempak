	SUBROUTINE TI_TZDF ( itarr, tzon1, tzon2, jtarr, hours, iret )
C************************************************************************
C* TI_TZDF								*
C*									*
C* This subroutine converts the time in one time zone to the time in	*
C* a second time zone. The number of hours difference is also returned.	*
C*									*
C* TI_TZDF ( ITARR, TZON1, TZON2, JTARR, HOURS, IRET )			*
C*									*
C* Input parameters:							*
C*	ITARR (5)	INTEGER		Time array in first time zone	*
C*	TZON1		CHAR*		First time zone			*
C*	TZON2		CHAR*		Second time zone		*
C*									*
C* Output parameters:							*
C*	JTARR (5)	INTEGER		Time array in second time zone	*
C*	HOURS		REAL		Hours difference between zones	*
C*	IRET		INTEGER		Return code			*
C*					 -15 = invalid time zones	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	12/97						*
C* D. Kidwell/NCEP	 1/05						*
C* m.gamazaychikov/SAIC	06/06	Added HST				*
C************************************************************************
	CHARACTER*(*)	tzon1, tzon2
	INTEGER		itarr (*), jtarr (*)
C*
	PARAMETER	( NZONES = 13 )
	CHARACTER	tzones (NZONES)*3, ctz1*3, ctz2*3
	INTEGER		ihrdif (NZONES)
C
C*	The list of time zones should be ordered such that the later
C*	items are earlier than GMT.
C
	DATA		tzones / 'GMT', 'UTC', 'Z  ', 'AST',
     +				 'EDT', 'EST', 'CDT', 'CST',
     +				 'MDT', 'MST', 'PDT', 'PST',
     +				 'HST' /
	DATA		ihrdif /     0,     0,     0,     4,
     +				     4,     5,     5,     6,
     +				     6,     7,     7,     8 ,
     +				    10 /
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize the output array.
C
	DO  i = 1, 5
	    jtarr (i) = 0
	END DO
C
C*	Convert the input time zones to uppercase.
C
	CALL ST_LCUC ( tzon1, ctz1, ier )
	CALL ST_LCUC ( tzon2, ctz2, ier )
C
C*	Find the time zones in the list.
C
	CALL ST_FIND ( ctz1, tzones, NZONES, ipos1, ier1 )
	CALL ST_FIND ( ctz2, tzones, NZONES, ipos2, ier2 )
C
C*	Check for errors or invalid input time zones.
C
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +	      ( ipos1 .eq. 0 ) .or. ( ipos2 .eq. 0 ) )  THEN
	    iret = -15
	    RETURN
	END IF
C
C*	Compute the difference in hours.
C
	hours  = ihrdif(ipos1) - ihrdif(ipos2)
C
C*	If the input time zones are the same, set the output array.
C
	IF  ( ipos1 .eq. ipos2 )  THEN
	    DO  i = 1, 5
		jtarr (i) = itarr (i)
	    END DO
C
C*	    If the first time zone appears in the list before the second,
C*	    add the number of hours to the input time array.
C
	  ELSE IF  ( ipos1 .gt. ipos2 )  THEN
	    minute = ABS ( hours ) * 60
	    CALL TI_ADDM ( itarr, minute, jtarr, ier )
C
C*	    If the first time zone appears in the list after the second,
C*	    subtract the number of hours from the input time array.
C
	  ELSE
	    minute = ABS ( hours ) * 60
	    CALL TI_SUBM ( itarr, minute, jtarr, ier )
	END IF
C*
	RETURN
	END
