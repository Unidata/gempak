	SUBROUTINE TG_ITOC  ( intdtf, gdattm, iret )
C************************************************************************
C* TG_ITOC								*
C*									*
C* This subroutine converts an integer time array containing the date,	*
C* time and forecast time into a GEMPAK grid time.			*
C*									*
C* TG_ITOC  ( INTDTF, GDATTM, IRET )					*
C*									*
C* Input parameters:							*
C*	INTDTF (3)	INTEGER		Date, time, forecast time	*
C*									*
C* Output parameters:							*
C*	GDATTM		CHAR*		GEMPAK grid time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid date or time	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C************************************************************************
	CHARACTER*(*)	gdattm
	INTEGER		intdtf (*)
C*
	CHARACTER	ftype*1, ftime*8, ttt*20
C------------------------------------------------------------------------
	iret   = 0
	gdattm = ' '
C
C*	Check for the blank time which may be found.
C
	IF  ( ( intdtf (1) .eq. 0 ) .and. ( intdtf (2) .eq. 0 ) .and.
     +	      ( intdtf (3) .eq. 0 ) )  RETURN
C
C*	Put the date and time into the character time.
C
	CALL TI_CDTM  ( intdtf (1), intdtf (2), gdattm, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Decode the forecast information if there is any.
C
	IF  ( intdtf (3) .ne. 0 )  THEN
	    CALL TG_CFTM  ( intdtf (3), ftype, ftime, iret )
	    IF  ( iret .ne. 0 )  RETURN
C
C*	    Combine two parts into string.
C
	    ttt    = gdattm
	    gdattm = ttt ( 1:11 ) // ftype // ftime
	END IF
C*
	RETURN
	END
