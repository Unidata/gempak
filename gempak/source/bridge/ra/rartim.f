	SUBROUTINE RA_RTIM  ( isdtar, btime, irhour, irmin, irdtar,
     +			      rtime, iret )
C************************************************************************
C* RA_RTIM								*
C*									*
C* This subroutine combines an integer system time, the bulletin	*
C* time containing the day, month and hour and the report day and	*
C* hour into an observation time.  It is assumed that the system	*
C* time accurately reflects the year and month of the observation	*
C* and is later than that time.						*
C*									*
C* RA_RTIM  ( ISDTAR, BTIME, IRHOUR, IRMIN, IRDTAR, RTIME, IRET )	*
C*									*
C* Input parameters:							*
C*	ISDTAR (5)	INTEGER		System time			*
C*	BTIME		CHAR*		Bulletin day, hour, minute	*
C*	IRHOUR		INTEGER		Report hour			*
C*	IRMIN		INTEGER		Report minute			*
C*									*
C* Output parameters:							*
C*	IRDTAR (5)	INTEGER		Integer report time		*
C*	RTIME		CHAR*		Report date/time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/90						*
C* J. Whistler/SSAI	 4/91	Subtract 24 from ibhour when > 24	*
C* J. Whistler/SSAI	 7/91	Changed iyear to ioyear were mistyped	*
C* S. Jacobs/NCEP	 1/96	Changed minute check from >45 to >=45	*
C* S. Jacobs/NCEP	 7/96	Added check for return from TI_ITOC	*
C* K. Tyle/GSC		 1/97	Added check for return from TI_ITOC (2)	*
C* D. Kidwell/NCEP	 4/98	Added store to irdtar when btime is ' ' *
C* D. Kidwell/NCEP	 9/98	Fixed bugs in month, year changeovers   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		isdtar (*), irdtar (*)
	CHARACTER*(*)	btime, rtime
C*
	LOGICAL		subday, addday
C*
	INTEGER		iodtar (5)
	EQUIVALENCE	( iodtar (1), ioyear ), ( iodtar (2), iomnth ),
     +			( iodtar (3), ioday  ), ( iodtar (4), iohour ),
     +			( iodtar (5), iomin  )
C*
	DATA		iomin / 0 /
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
C*	If no bulletin time was sent, use day from system time and 
C*	hour from report time.
C
	IF  ( btime .eq. ' ' )  THEN
	    ioday  = isdtar (3)
C
C*	    Get correct hour depending on the minutes on the report.
C
	    IF  ( irmin .ge. 45 )  THEN
		iohour = irhour + 1
	      ELSE
		iohour = irhour
	    END IF
	    IF  ( iohour .ge. 24 )  iohour = iohour - 24
	    IF  ( iohour .gt. isdtar (4) )  THEN
		CALL TI_SUBD  ( iodtar, iodtar, ier )
	    END IF
C
C*	    Get full time.
C
	    CALL TI_ITOC  ( iodtar, rtime, ierr )
	    IF  ( ierr .ne. 0 )  iret = -1
	    DO  i = 1, 5
		irdtar (i) = iodtar (i)
	    END DO
	  ELSE
C
C*	    Get the day, hour and minute from the bulletin.
C
	    CALL ST_INTG  ( btime (1:2), ioday,  ier )
	    CALL ST_INTG  ( btime (3:4), ibhour, ier )
	    CALL ST_INTG  ( btime (5:6), ibmin,  ier )
	    IF  ( ibmin .ge. 45 )  THEN
		ibhour = ibhour + 1
		IF  ( ibhour .ge. 24 )  THEN
		    ibhour = ibhour - 24
		    addday = .true.
		END IF
	    END IF
C
C*	    Get the hour from the report time.
C
	    iohour = irhour
	    IF  ( irmin .ge. 45 )  THEN
		iohour = iohour + 1
		IF  ( iohour .ge. 24 )  iohour = iohour - 24
	    END IF
C
C*	    Make sure the report and bulletin time are within one hour.
C
	    ihdiff = ABS  ( ibhour - iohour )
	    IF  ( ihdiff .le. 1 )  THEN
C
C*		This report is ok.
C
	      ELSE IF  ( ( iohour .eq. 0 ) .and. ( ibhour .eq. 23 ) )
     +								THEN
		addday = .true.
	      ELSE IF  ( ( iohour .eq. 23 ) .and. ( ibhour .eq. 0 ) )
     +								THEN
		subday = .true.
	      ELSE
C
C*		Bulletin and report time do not match.
C
		iret  = -1
		rtime = ' '
		RETURN
	    END IF
C
C*	    Check that the month has not changed.
C
	    IF  ( ( isdtar (3) .eq. 1 ) .and. ( iodtar (3) .gt. 27 ) )
     +								THEN
		iomnth = iomnth - 1
		IF  ( iomnth .eq. 0 )  THEN
		    iomnth = 12
		    ioyear = ioyear - 1
		END IF
	      ELSE IF  ( ( isdtar (3) .gt. 27 ) .and. 
     +			 ( iodtar (3) .eq. 1 ) )  THEN
		iomnth = iomnth + 1
		IF  ( iomnth .eq. 13 )  THEN
		    iomnth = 1
		    ioyear = ioyear + 1
		END IF
	    END IF
C
C*	    Add or subtract days if necessary.
C
	    IF  ( subday )  CALL TI_SUBD  ( iodtar, iodtar, ier )
	    IF  ( addday )  CALL TI_ADDD  ( iodtar, iodtar, ier )
C
C*	    Convert to a GEMPAK time.
C
	    CALL TI_ITOC  ( iodtar, rtime, ierr )
	    IF ( ierr .ne. 0 ) iret = -1
	    DO  i = 1, 5
		irdtar (i) = iodtar (i)
	    END DO
	END IF
C*
	RETURN
	END
