	SUBROUTINE FF_RTIM  ( isdtar, btime, irdtar, rtime, iret)
C************************************************************************
C* FF_RTIM								*
C*									*
C* This subroutine combines an integer system time, and the bulletin	*
C* time containing the day, month and hour into an observation time.	*
C* It is assumed that the system time accurately reflects the year 	*
C* and month of the observation	and is later than that time.		*
C*									*
C* FF_RTIM  ( ISDTAR, BTIME, IRDTAR, RTIME, IRET )			*
C*									*
C* Input parameters:							*
C*	ISDTAR (5)	INTEGER		System time			*
C*	BTIME		CHAR*		Bulletin day, hour, minute	*
C*									*
C* Output parameters:							*
C*	IRDTAR (5)	INTEGER		Integer report time		*
C*	RTIME		CHAR*		Report date/time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					  4 = invalid time		*
C**									*
C* Log:									*
C* L. Sager/NCEP 	 5/96	Copied from RA_RTIM                   	*
C* L. Sager/NCEP 	 6/96	Corrected hour                        	*
C* K. Tyle/GSC		 3/97	Allow multiple times per day		*
C* D. Kidwell/NCEP 	 9/98	Fixed several bugs                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		isdtar (*), irdtar (*)
	CHARACTER*(*)	btime, rtime
C*
	LOGICAL		addday
	INTEGER		iodtar (5)
C*
	EQUIVALENCE	( iodtar (1), ioyear ), ( iodtar (2), iomnth ),
     +			( iodtar (3), ioday  ), ( iodtar (4), iohour ),
     +			( iodtar (5), iomin  )
	DATA		iomin / 0 /
C------------------------------------------------------------------------
	iret   = 0
	addday = .false.
C
C*	Use the system time for the observation time for the year and 
C*	month.
C
	ioyear = isdtar (1)
	iomnth = isdtar (2)
C
C*	If no bulletin time was sent, use day and hour from 
C*	system time.
C
	IF  ( btime .eq. ' ' )  THEN
	    ioday  = isdtar (3)
	    iohour = isdtar (4) 
C
C*	    Get full time.
C
	    CALL TI_ITOC  ( iodtar, rtime, ier )
	    DO  i = 1, 5
		irdtar (i) = iodtar (i)
	    END DO
	  ELSE
C
C*	    Get the day, hour and minute from the bulletin.
C
	    CALL ST_INTG  ( btime (1:2), ioday,  ier )
	    CALL ST_INTG  ( btime (3:4), iohour, ier )
	    CALL ST_INTG  ( btime (5:6), ibmin,  ier )
	    IF  ( ibmin .ge. 45 )  THEN
		iohour = iohour + 1
		IF  ( iohour .ge. 24 )  THEN
		    iohour = iohour - 24
		    addday = .true.
		END IF
	    END IF
C
C*          Check that the month has not changed.
C
            IF  ( ( isdtar (3) .eq. 1 ) .and. ( iodtar (3) .gt. 27 ) )
     +                                                          THEN
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
C*          Add day if necessary.
C
            IF  ( addday )  CALL TI_ADDD  ( iodtar, iodtar, ier )
C
C*	    Convert to a GEMPAK time.
C
	    CALL TI_ITOC  ( iodtar, rtime, iret )
	    rtime = rtime (1:9) // '00'
	    DO  i = 1, 5
		irdtar (i) = iodtar (i)
	    END DO
	    IF ( iret .ne. 0 ) iret = 4
	END IF
C*
	RETURN
	END
