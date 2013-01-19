	SUBROUTINE TG_FTOI  ( iftime, intdtf, iret )
C************************************************************************
C* TG_FTOI								*
C*									*
C* This subroutine converts the two integers stored in a grid file	*
C* into three integers containing the date, time and forecast time.	*
C*									*
C* TG_FTOI  ( IFTIME, INTDTF, IRET )					*
C*									*
C* Input parameters:							*
C*	IFTIME (2)	INTEGER		Grid time stored in file	*
C*									*
C* Output parameters:							*
C*	INTDTF (3)	INTEGER		Date, time, forecast time	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C************************************************************************
	INTEGER		iftime (*), intdtf (*)
C------------------------------------------------------------------------
	iret   = 0
C
C*	If there is no forecast information, the string is stored as
C*	date and time.
C
	IF  ( iftime (1) .lt. 100000000 )  THEN
	    intdtf (1) = iftime (1)
	    intdtf (2) = iftime (2)
	    intdtf (3) = 0
C
C*	    Otherwise, decode date/time and forecast info from the
C*	    two integers.
C
	  ELSE
C
C*	    The first word contains MMDDYYHHMM.  This must be turned
C*	    into YYMMDD and HHMM.
C
	    intdtf (1) = iftime (1) / 10000
	    intdtf (2) = iftime (1) - intdtf (1) * 10000
	    mmdd = intdtf (1) / 100
	    iyyy = intdtf (1) - mmdd * 100
	    intdtf (1) = iyyy * 10000 + mmdd
C
C*	    The forecast time remains the same.
C
	    intdtf (3) = iftime (2)
	END IF
C*
	RETURN
	END
