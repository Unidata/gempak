	SUBROUTINE WW_RTIM  ( isdtar, irday, irhour, irmin, irdtar,
     +                        iret )
C************************************************************************
C* WW_RTIM								*
C*									*
C* This subroutine combines an integer system time and a watch issue    *
C* day, hour and minute into an integer watch issue time.  It assumes   *
C* that the system time accurately reflects the year and month of the   *
C* watch issue time.	                        			*
C*									*
C* WW_RTIM  ( ISDTAR, IRDAY, IRHOUR, IRMIN, IRDTAR, IRET )              *
C*									*
C* Input parameters:							*
C*	ISDTAR (5)	INTEGER		System time			*
C*	IRDAY		INTEGER		Watch issue day                 *
C*	IRHOUR		INTEGER		Watch issue hour        	*
C*	IRMIN		INTEGER		Watch issue minute		*
C*									*
C* Output parameters:							*
C*	IRDTAR (5)	INTEGER		Integer watch issue time	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 8/98	Copied from RA_RTIM			*
C* D. Kidwell/NCEP	 9/98	Fixed bugs in month, year changeovers   *
C* D. Kidwell/NCEP	 4/99	Rewrote                                 *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		isdtar (*), irdtar (*)
C*
	INTEGER		iodtar (5)
	EQUIVALENCE	( iodtar (1), ioyear ), ( iodtar (2), iomnth ),
     +			( iodtar (3), ioday  ), ( iodtar (4), iohour ),
     +			( iodtar (5), iomin  )
C------------------------------------------------------------------------
	iret   = 0
C
C*	Use the system time for the watch time year and month.
C
	ioyear = isdtar (1)
	iomnth = isdtar (2)
	ioday  = irday
	iohour = irhour
	iomin  = irmin
C
C*	Check that the month has not changed.
C
	IF  ( ( isdtar (3) .lt. 5 ) .and. ( iodtar (3) .gt. 27 ) ) THEN
	    iomnth = iomnth - 1
	    IF  ( iomnth .eq. 0 )  THEN
	        iomnth = 12
	        ioyear = ioyear - 1
	    END IF
	  ELSE IF  ( ( isdtar (3) .gt. 27 ) .and. 
     +		     ( iodtar (3) .lt. 5  ) )  THEN
	    iomnth = iomnth + 1
	    IF  ( iomnth .eq. 13 )  THEN
	        iomnth = 1
	        ioyear = ioyear + 1
	    END IF
	END IF
C
	DO  i = 1, 5
	    irdtar (i) = iodtar (i)
	END DO
C*
	RETURN
	END
