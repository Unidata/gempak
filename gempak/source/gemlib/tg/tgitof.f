	SUBROUTINE TG_ITOF  ( intdtf, iftime, iret )
C************************************************************************
C* TG_ITOF								*
C*									*
C* This subroutine converts three integers containing the date, time	*
C* and forecast field into the two integers stored in a grid file.	*
C*									*
C* TG_ITOF  ( INTDTF, IFTIME, IRET )					*
C*									*
C* Input parameters:							*
C*	INTDTF (3)	INTEGER		Date, time, forecast time	*
C*									*
C* Output parameters:							*
C*	IFTIME (2)	INTEGER		Grid time stored in file	*
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
	IF  ( intdtf (3) .eq. 0 )  THEN
	    iftime (1) = intdtf (1) 
	    iftime (2) = intdtf (2)
C
C*	    Otherwise, turn the date and time into a single integer of
C*	    the form MMDDYYHHMM.
C
	  ELSE
	    iyyy = intdtf (1) / 10000
	    mmdd = intdtf (1) - 10000 * iyyy
	    iftime (1) = mmdd * 1000000 + iyyy * 10000 + intdtf (2)
C
C*	    The forecast time remains the same.
C
	    iftime (2) = intdtf (3)
	END IF
C*
	RETURN
	END
