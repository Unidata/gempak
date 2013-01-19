	SUBROUTINE RU_RTIM  ( isdtar, irday, irhour, iodtar, iret )
C************************************************************************
C* RU_RTIM								*
C*									*
C* This subroutine combines an integer system time and the report	*
C* day and hour into an observation time.  It is assumed that the	*
C* system time accurately reflects the time at which the report was	*
C* received, and is later than the actual report time (i.e., the time	*
C* the observation was made).						*
C*									*
C* RU_RTIM  ( ISDTAR, IRDAY, IRHOUR, IODTAR, IRET )			*
C*									*
C* Input parameters:							*
C*	ISDTAR (5)	INTEGER		System time			*
C*	IRDAY		INTEGER		Report day			*
C*	IRHOUR		INTEGER		Report hour			*
C*									*
C* Output parameters:							*
C*	IODTAR (5)	INTEGER		Observation date/time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time              *
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	12/87	Changed TI subroutines			*
C* D. Kidwell/NCEP	10/98	Error return if report hour gt 24       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		isdtar (*), iodtar (*)
C*
	LOGICAL		addday
C------------------------------------------------------------------------
	iret = 0
C
C*	Use the system time for the observation time for the year and 
C*	month.
C
	ioyear = isdtar (1)
	iomnth = isdtar (2)
C
C*	Use the report time for the day and hour.
C
	ioday  = irday
	iohour = irhour
C
C*	If the report hour was 24, set addday flag and reset time.
C*	If the report hour was greater than 24 or less than zero, 
C*      error return.
C
	IF  ( iohour .eq. 24 )  THEN
	    iohour = 0
	    addday = .true.
	  ELSE IF ( iohour .lt. 24 .and. iohour .ge. 0 ) THEN
	    addday = .false.
	  ELSE
	    iret = -1
	    RETURN
	END IF
C
C*	Change the month and year if the report day is after the system
C*	day assuming that the report day was in the previous month.
C
	isday = isdtar ( 3 )
	IF  ( ( ioday .gt. 27 ) .and. ( isday .eq. 1 ) )  THEN
	    iomnth = iomnth - 1
	    IF  ( iomnth .eq. 0 )  THEN
		iomnth = 12
		ioyear = ioyear - 1
	    END IF
	END IF
C
C*	Fill output observation time array.
C
	iodtar (1) = ioyear
	iodtar (2) = iomnth
	iodtar (3) = ioday
	iodtar (4) = iohour
	iodtar (5) = 0
C
C*	Add a day if necessary.
C
	IF  ( addday )  THEN
	    CALL TI_ADDD ( iodtar, iodtar, ier )
	END IF
C
	RETURN
	END
