	SUBROUTINE UT_85DY  ( r85day, irptdt, iret )
C************************************************************************
C* UT_85DY								*
C*									*
C* This routine computes and returns the report date-time corresponding	*
C* to the "1985 Day" in the original report.  The "1985 Day" is a count	*
C* of the number of days elapsed since 1/1/1985, 0000Z; for example:	*
C*	at 1/1/1985, 0000Z, the "1985 Day" is 0.00,			*
C*	at 1/2/1985, 0000Z, the "1985 Day" is 1.00,			*
C*	at 1/3/1985, 0000Z, the "1985 Day" is 2.00,			*
C*	at 1/3/1985, 1200Z, the "1985 Day" is 2.50, etc.		*
C*									*
C* UT_85DY ( R85DAY, IRPTDT, IRET )					*
C*									*
C* Input parameters:							*
C*	R85DAY		REAL*8		# of days since 1/1/1985, 0000Z	*
C*									*
C* Output parameters:							*
C*	IRPTDT (*)	INTEGER		Report date-time		*
C*					(YYYY, MM, DD, HH, MM ) 	*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C*					 -1 = could not compute IRPTDT	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		01/01						*
C* J. Ator/NCEP		06/01	Fixed error in docblock			*
C* R. Hollern/NCEP	07/02	Adapted from NA_85DY			*
C* R. Hollern/NCEP	04/03	Added logic to handle the case when the *
C*				computed report time hour is 24		*
C************************************************************************
	INTEGER		irptdt (*),  jrptdt (5)
	INTEGER		iwkdt (3)
C*
	real*8		r85day
C*
	LOGICAL		gotyr
C*-----------------------------------------------------------------------
	iret = -1
C
C*	Beginning with 1985, determine how many days are in each
C*	successive year, and continue doing so until the cumulative sum
C*	is greater than or equal to the whole (i.e. integer) portion of
C*	the "1985 Day".  The last year for which a count was determined
C*	is then equal to the year of the report.
C	
	iwk85d = INT ( r85day )
C
	iwkdt (1) = 1985
	iwkdt (2) = 12
	iwkdt (3) = 31
C
	gotyr = .false.
	DO WHILE  ( .not. gotyr )
	    CALL TI_ITOJ  ( iwkdt, iwkyr, ijday, iertoj )
C
C*	    Note that 1/1/1985 represents day 0 (not day 1), so we need
C*	    to decrement by 1 our count of the number of days for that
C*	    particular year.
C
	    IF  ( iwkdt (1) .eq. 1985 )  ijday = ijday - 1 
C
	    IF  ( ijday .lt. iwk85d )  THEN
		iwk85d = iwk85d - ijday
		iwkdt (1) = iwkdt (1) + 1
	    ELSE
		gotyr = .true.
		irptdt (1) = iwkdt (1)
	    END IF
	END DO
C
C*	Now, determine the month and day of the report by using the
C*	remaining number of days that was leftover from the previous
C*	determination of the year of the report.
C
	CALL TI_JTOI  ( irptdt (1), iwk85d, iwkdt, iertoi )
	IF  ( iertoi .ne. 0 )  THEN
	    RETURN
	END IF
	irptdt (2) = iwkdt (2)
	irptdt (3) = iwkdt (3)
C
C*	Now, use the fractional portion of the "1985 Day" to determine
C*	the hour and minute of the report.
C
	nmins = NINT ( ( r85day - FLOAT ( INT ( r85day ) ) ) * 1440. )
	irptdt (4) = nmins / 60
	irptdt (5) = MOD ( nmins, 60 )
C
C*	Check if hour is 24.  If it is, set hour to 00 and add a day
C*	to the date.
C
	IF ( irptdt (4) .eq. 24 ) THEN
C
	   irptdt (4) = 0
C
	   CALL TI_ADDD ( irptdt, jrptdt, iret ) 
C
C*	   IF ( iret .ne. 0 ) RETURN
C
	   irptdt (1) = jrptdt(1)
	   irptdt (2) = jrptdt(2)
	   irptdt (3) = jrptdt(3)
	END IF
C
	iret = 0
C*
	RETURN
	END
