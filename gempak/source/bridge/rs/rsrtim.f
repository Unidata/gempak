	SUBROUTINE RS_RTIM  ( isdtar, day, hour, data, defalt, obfreq, 
     +			      irdtar, rtime, ihhmm, iret )
C************************************************************************
C* RS_RTIM								*
C*									*
C* This subroutine combines an integer system time, the bulletin	*
C* time containing the day and hour and the report day, hour, and	*
C* minute into an observation time.  It is assumed that the time in	*
C* isdtar accurately reflects the year and month of the observation	*
C* and is later than that time.						*
C*									*
C* RS_RTIM  ( ISDTAR, DAY, HOUR, DATA, DEFALT, OBFREQ, IRDTAR, RTIME,	*
C*	      IHHMM, IRET )						*
C*									*
C* Input parameters:							*
C*	ISDTAR (5)	INTEGER		System time			*
C*	DAY		INTEGER		Nominal day of report		*
C*	HOUR		INTEGER		Nominal hour of report		*
C*	DATA (*)	INTEGER		Decoded data from RS_DECO	*
C*	DEFALT		INTEGER		Missing data flag		*
C*	OBFREQ		INTEGER		Interval (min) between ob times	*
C*									*
C* Output parameters:							*
C*	IRDTAR (5)	INTEGER		Integer report nominal time	*
C*	RTIME		CHAR*		Report nominal date/time	*
C*	IHHMM		INTEGER		Report actual time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C*					 -2 = time conflicts with obfreq*
C**									*
C* Log:									*
C* J. Nielsen		 2/92	Loosely dapted from RA_RTIM		*
C* D. Kidwell/NCEP	 9/98	Fixed bugs in month, year changeovers   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		isdtar (*), day, hour, data (*), obfreq, defalt
	INTEGER		irdtar (*), ihhmm
	CHARACTER*(*)	rtime
C*
	LOGICAL		subday, addday
C*
	INTEGER		iodtar (5)
	EQUIVALENCE	( iodtar (1), ioyear ), ( iodtar (2), iomnth ),
     +			( iodtar (3), ioday  ), ( iodtar (4), iohour ),
     +			( iodtar (5), iomin  )
C------------------------------------------------------------------------
	iret   = 0
	addday = .false.
	subday = .false.
C
C*	Use the system time for the observation time for the year and 
C*	month.
C
	ioyear = isdtar (1)
	iomnth = isdtar (2)
C
C*	Get times from report, from header, or from system time, in 
C*	order of preference.  Fix date if we have an ob time of 00Z
C*	and a nominal time of prior to 00Z.  Use minutes from report
C*	if given, and assume zero otherwise.
C
	IF  (( data (91) .ne. defalt ) .and. 
     +	     ( data (91) .le. 2359 ))  THEN
C
C*	    Hour and minute of ob reported
C
	    iomin = MOD ( data(91), 100 )
	    iohour = ( data(91) - iomin) / 100
	    IF  ( iomin .ge. 60 )  iomin = 0
	    IF  ( iomin .lt. 0 )  iomin = 0
	    IF  (( data (3) .ne. defalt ) .and. 
     +		 ( data (3) .le. 31 ))  THEN
	        ioday = data (3)
		IF  ( iohour - data (4) .eq. 23 )  subday = .true. 
	    ELSE IF  ( day .le. 31 )  THEN
	        ioday = day
		IF  ( iohour - hour .eq. 23 )  subday = .true.
	    ELSE 
	        ioday = isdtar (3)
	    ENDIF
	ELSE IF  (( data(4) .ne. defalt ) .and. 
     +		  ( data(4) .le. 23 ))  THEN
C
C*	    Day and hour of ob reported
C
	    iohour = data(4)
	    iomin = 0
	    IF  (( data (3) .ne. defalt ) .and. 
     +		 ( data (3) .le. 31 ))  THEN
	        ioday = data (3)
	    ELSE IF  ( day .le. 31 )  THEN
	        ioday = day
		IF  ( iohour - hour .eq. 23 )  subday = .true.
	    ELSE 
	        ioday = isdtar (3)
	    ENDIF
	ELSE IF  ( hour .le. 23 ) THEN
C
C*	    Get day and hour from header info
C
	    iohour = hour
	    iomin = 0
	    IF  ( day .le. 31 )  THEN
	        ioday = day
	    ELSE 
	        ioday = isdtar (3)
	    ENDIF
	ELSE
C
C*	    Use system day and hour
C
	    iohour = isdtar (4)
	    iomin = 0
	    ioday = isdtar (3)
	ENDIF   
C
C*	Make sure the report and bulletin time are within one hour.
C
	ihdiff = ABS  ( hour - iohour )
	IF  ( ( ihdiff .gt. 1 ) .and. ( ihdiff .ne. 23 ) ) THEN
C
C*	    Bulletin and report time do not match.
C
	    iret  = -1
	    rtime = ' '
	    RETURN
	END IF
C
C*	Before adjusting time to correspond to obfreq, save ihhmm
C
	ihhmm = iohour * 100 + iomin
C
C*	Establish the proper minutes using obfreq
C
	itmin = iohour * 60 + iomin
	IF  ( obfreq .gt. 60 )  THEN
	    ifreq = 60
	ELSE
	    ifreq = obfreq
	END IF
	IF  ( obfreq .gt. 0 )  THEN
	    ileft = MOD ( itmin, obfreq )
	    IF  ( ileft .ge. ifreq/2 )  THEN
		IF  ( ( obfreq - ileft ) .gt. ifreq/2 )  THEN
		    iret = -2
		    RETURN
		END IF
		itmin = itmin - ileft + obfreq
	    ELSE
		itmin = itmin - ileft
	    ENDIF
	ENDIF
	IF  ( itmin .ge. 24*60 )  THEN 
	    addday = .true.
	    itmin = itmin - 24*60
	ENDIF
	iomin = MOD ( itmin, 60 )
	iohour = ( itmin - iomin ) / 60
	
C
C*	Check that the month has not changed.
C
	IF  ( ( isdtar (3) .eq. 1 ) .and. ( iodtar (3) .gt. 27 ) )
     +								THEN
	    iomnth = iomnth - 1
	    IF  ( iomnth .eq. 0 )  THEN
		iomnth = 12
		ioyear = ioyear - 1
	    END IF
	ELSE IF  ( ( isdtar (3) .gt. 27 ) .and. 
     +		   ( iodtar (3) .eq. 1 ) )  THEN
	    iomnth = iomnth + 1
	    IF  ( iomnth .eq. 13 )  THEN
		iomnth = 1
		ioyear = ioyear + 1
	    END IF
	END IF
C
C*	Add or subtract days if necessary.
C
	IF  ( subday )  CALL TI_SUBD  ( iodtar, iodtar, ier )
	IF  ( addday )  CALL TI_ADDD  ( iodtar, iodtar, ier )
C
C*	Convert to a GEMPAK time.
C
	CALL TI_ITOC  ( iodtar, rtime, iret )
	DO  i = 1, 5
	    irdtar (i) = iodtar (i)
	END DO
C*
	RETURN
	END
