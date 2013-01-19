	SUBROUTINE MA_CGDT ( iret )
C************************************************************************
C* MA_CGDT                                                              *
C*                                                                      *
C* This subroutine sets the report date/time using system and bulletin  *
C* header as input.  Parameters not in the calling sequence are found	*
C* in common.								*
C*                                                                      *
C* MA_CGDT ( IRET )	       	                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*	RCTIM(*)	REAL		System year, month, and day	*
C*                                                                      *
C* Output parameters:                                                   *
C*	IRPTDT(*)	INTEGER		Report date and time		*
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* C. Caruso Magee/NCEP  4/01   Modifying for Coast Guard data.         *
C* F. J. Yen/NCEP	 4/01	Reformatted and renamed from CG_DATM.  	*
C*				Removed include 'GEMPRM.PRM' statement.	*
C*				Added parameters in common to prologue.*
C************************************************************************
        INCLUDE 	'macmn.cmn'
C-----------------------------------------------------------------------
	iret   = 0
C
C*	Use system year/month and bulletin day/hour/minute.
C
	irptdt(1) = nint ( rctim(2) )
	irptdt(2) = nint ( rctim(3) )
C
C*	Store bulletin day as report day.
C
	CALL ST_INTG ( btime(1:2), ist1, ier )
	irptdt(3) = ist1 
	IF ( irptdt(3) .gt. 31 ) THEN
	    WRITE ( UNIT = logmsg, FMT = '( I4 )' ) irptdt(3)
	    CALL DC_WLOG ( 2, 'MA', 7, logmsg, ierwlg )
	    iret   = -1
	    RETURN
	END IF
C
C*	Check to see if system day is first day of month.  If so,
C*	then if bulletin day is from previous month (i.e. bulletin
C*	day is greater than 1) then set month back to previous month.
C
	IF ( rctim(4) .eq. 1. .and. irptdt(3) .gt. 1 ) THEN
	    IF ( irptdt(2) .gt. 1) THEN
		irptdt(2) = irptdt(2) - 1 
	      ELSE
		irptdt(1) = irptdt(1) - 1
		irptdt(2) = 12
	    END IF
	END IF
C
C*	Store bulletin hour as report hour.
C
	CALL ST_INTG ( btime(3:4), ist2, ier )
	irptdt(4) = ist2
	IF ( irptdt(4) .lt. 0 .or. irptdt(4) .gt. 23 ) THEN
	    WRITE ( UNIT = logmsg, FMT = '( I4 )' ) irptdt(4)
	    CALL DC_WLOG ( 2, 'MA', 8, logmsg, ierwlg )
	    iret   = -1
	    RETURN
	END IF
C
C*	Store bulletin minute as report minute.
C
	CALL ST_INTG ( btime(5:6), ist3, ier )
	irptdt(5) = ist3 
	IF ( irptdt(5) .lt. 0 .or. irptdt(4) .gt. 59 ) THEN
	    WRITE ( UNIT = logmsg, FMT = '( I4 )' ) irptdt(5)
	    CALL DC_WLOG ( 2, 'MA', 9, logmsg, ierwlg )
	    iret   = -1
	RETURN
	END IF
C*
	RETURN
	END
