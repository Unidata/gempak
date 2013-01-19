	SUBROUTINE GDPDTV  ( gdatim, gvcord, gfunc, firstm, lastim, 
     +			     time, ivcord, iret )
C************************************************************************
C* GDPDTV								*
C*									*
C* This subroutine gets the time and vertical level requested by the	*
C* user.								*
C*									*
C* GDPDTV  ( GDATIM, GVCORD, GFUNC, FIRSTM, LASTIM, TIME, IVCORD,	*
C*           IRET )							*
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
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = invalid time		*
C*					 -6 = invalid vert coord	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 7/89	Recoded					*
C* K. Brill/GSC         11/89   Added search for + in GFUNC		*
C* K. Brill/NMC		05/93	CALL ST_LCUC before LV_CORD		*
C************************************************************************
	CHARACTER*(*)	gdatim, gvcord, gfunc, firstm, lastim, time (2)
C*
	CHARACTER	ctime*48, ccord*20
C------------------------------------------------------------------------
	iret  = 0
C
C*	Read input grid function to see if it contains date/time or 
C*	vertical coordinate information.
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
C*
	idt = INDEX ( gfunc, '%' )
	IF  ( idt .eq. 0 )  THEN
	    ccord = gvcord
	  ELSE
	    idt   = idt + 1
	    ict   = 0
	    ccord = ' '
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
		ccord (ict:ict) = gfunc (idt:idt)
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
C
C*	Check that vertical coordinate is valid.
C
	CALL ST_LCUC  ( ccord, ccord, ier )
	CALL LV_CORD  ( ccord, ccord, ivcord, ier )
	IF  ( ( ier .ne. 0 ) .or. ( ivcord .eq. 0 ) )  THEN
	    iret = -6
	    RETURN
	END IF
C*
	RETURN
	END
