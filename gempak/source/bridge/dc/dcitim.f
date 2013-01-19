	SUBROUTINE DC_ITIM  ( isdtar, irday, irhour, irmin, irdtar,
     +                        iret )
C************************************************************************
C* DC_ITIM								*
C*									*
C* This subroutine combines an integer system time and an issue day,    *
C* hour and minute into an integer issue time.  It assumes that the     *
C* system time accurately reflects the year and month of the bulletin   *
C* issue time.	                        				*
C*									*
C* DC_ITIM  ( ISDTAR, IRDAY, IRHOUR, IRMIN, IRDTAR, IRET )              *
C*									*
C* Input parameters:							*
C*	ISDTAR (5)	INTEGER		System time			*
C*	IRDAY		INTEGER		Issue day     			*
C*	IRHOUR		INTEGER		Issue hour     			*
C*	IRMIN		INTEGER		Issue minute			*
C*									*
C* Output parameters:							*
C*	IRDTAR (5)	INTEGER		Integer issue time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* A. Hardy/GSC		 3/00		Replaces hc_rtim, ww_rtim       *
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
C*	Use the system time for the year and month.
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
