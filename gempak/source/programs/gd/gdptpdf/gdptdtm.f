	SUBROUTINE GDPTDTM  ( gdatim, gvcord, gfunc, firstm, lastim, 
     +			     time, iret )
C************************************************************************
C* GDPTDTM								*
C*									*
C* This subroutine gets the date/time requested by the user.		*
C*									*
C* GDPDTV  ( GDATIM, GVCORD, GFUNC, FIRSTM, LASTIM, TIME, IRET )	*
C*									*
C* Input parameters:							*
C*	GDATIM		CHAR*		User input for date/time	*
C*	GVCORD		CHAR*		User input for vert coord	*
C*	GFUNC		CHAR*		User input for grid function	*
C*	FIRSTM		CHAR*		First time in file		*
C*	LASTIM		CHAR*		Last time in file		*
C*									*
C* Output parameters:							*
C*	TIME  (2)	CHAR*		Date/time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = invalid time		*
C**									*
C* Log:									*
C* M. Li/SAIC		08/07	Modified from GDPDTV			*
C************************************************************************
	CHARACTER*(*)	gdatim, gvcord, gfunc, firstm, lastim, time (2)
C*
	CHARACTER	ctime*48
C------------------------------------------------------------------------
	iret  = 0
C
C*	Read input grid function to see if it contains date/time.
C
	CALL ST_LSTR  ( gfunc, iend, ier )
	idt = INDEX ( gfunc, '^' )
	IF  ( idt .eq. 0 )  THEN
	    ctime = gdatim
	  ELSE
	    idt   = idt + 1
	    ict   = 0
	    ctime = ' '
	    DO WHILE  ( ( idt .le. iend ) .and. 
     +			( gfunc (idt:idt) .ne. ')' ) .and.
     +			( gfunc (idt:idt) .ne. ']' ) .and.
     +			( gfunc (idt:idt) .ne. ',' ) .and.
     +			( gfunc (idt:idt) .ne. ';' ) .and.
     +			( gfunc (idt:idt) .ne. '^' ) .and.
     +			( gfunc (idt:idt) .ne. '%' ) .and.
     +			( gfunc (idt:idt) .ne. '+' ) .and.
     +			( gfunc (idt:idt) .ne. '@' ) )
		ict = ict + 1
		ctime (ict:ict) = gfunc (idt:idt)
		idt = idt + 1
	    END DO
	END IF
C
C*	Change time into two parts.
C
	CALL GR_GTIM  ( ctime, firstm, lastim, time (1), time (2), ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -5
	    RETURN
	END IF
C*
	RETURN
	END
