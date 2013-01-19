	SUBROUTINE TG_QRNG  ( gdatim, rngtyp, gtype, iret )
C************************************************************************
C* TG_QRNG								*
C*									*
C* This subroutine determines whether a GDATIM is a singular time,	*
C* multiple times based on forecast hour, or multiple times based on	*
C* cycles.								*
C*									*
C* TG_QRNG  ( GDATIM, RNGTYP, GTYPE, IRET )				*
C*									*
C* Input parameters:							*
C*	GDATIM		CHAR*		Input time			*
C*									*
C* Output parameters:							*
C*	RNGTYP		INTEGER		Type of time indicator		*
C*					= 0 - not a range		*
C*					= 1 - range as forecast hours	*
C*					= 2 - range as cycle hours	*
C*	GTYPE		LOGICAL		Grid time indicator		*
C*					.TRUE.  - start with a letter	*
C*					.FALSE. - start with a numeric	*
C*	IRET		INTEGER		Return code			*
C*					  As TG_RANG			*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 7/98	From TG_RANG				*
C* T. Lee/GSC		 7/99	Checked grid time type			*
C************************************************************************
	CHARACTER*(*)	gdatim
	INTEGER		rngtyp
C*
	CHARACTER	tstart*20, tstop*20, tinc*20, ctype*1, ginput*48
C
	CHARACTER	xxxx*1
	LOGICAL		qtype, gtype
        QTYPE (xxxx) = ( ( xxxx (1:1) .eq. 'F' ) .or.
     +                   ( xxxx (1:1) .eq. 'A' ) .or.
     +                   ( xxxx (1:1) .eq. 'G' ) .or.
     +                   ( xxxx (1:1) .eq. 'V' ) .or.
     +                   ( xxxx (1:1) .eq. 'I' ) )
C------------------------------------------------------------------------
	iret   = 0
	rngtyp = 0
	gtype = .false.
C
	CALL ST_LCUC  ( gdatim, ginput, ier )
	gtype = QTYPE ( ginput (1:1) ) .or. ( ginput (1:1) .eq. 'L' )
C
C*	Break range into parts.
C
	CALL ST_RANG  ( ginput, tstart, tstop, tinc, itype, ier )
C
C*	The returned itype from ST_RANG should not be confused with 
C*	rngtyp... itype indicates whether ginput is a range ( =0, 
C*	not a range), a range w/o and increment (=1), or a range w/ 
C*	increment (=2).
C
C*	If this is not a range, check for "ALL".
C
	IF  ( itype .eq. 0 )  THEN
	    indall = INDEX ( ginput, 'ALL' )
C
C*	    If ALL is not included somewhere (ALL or FALL or GALL, etc,), 
C*	    this is not a time range, return w/ rngtyp = 0.
C
	    IF  ( indall .eq. 0 )  THEN
		RETURN
C
C*		If the entire string is "ALL", assume this is a forecast
C*		hour range, return w/ rngtyp = 1.
C
	      ELSE IF  ( ginput .eq. 'ALL' )  THEN
		rngtyp = 1
		RETURN
C
C*		If ALL is first, but not entire string, assume this is a
C*		cycle range, return w/ rngtyp = 2.
C
	      ELSE IF  ( indall .eq. 1 )  THEN
                rngtyp = 2
                RETURN
C
C*		Check for FALL, AALL, GALL, IALL.
C
	      ELSE
		ctype = ginput ( indall-1 : indall-1 )
		IF  ( ( ctype .eq. 'F' ) .or. ( ctype .eq. 'A' ) .or.
     +		      ( ctype .eq. 'G' ) .or. ( ctype .eq. 'I' ) )  THEN
		    rngtyp = 1
                    RETURN
		  ELSE
		    iret = -7
		    RETURN
		END IF
	    END IF
	END IF
C
	IF ( tstart .eq. 'FIRST' .or. tstop .eq. 'LAST' )  THEN
	    rngtyp = 1
	    RETURN
	END IF
C
C*      Check for FIRST or LAST followed by forecast type.
C*      Assume this format indicates a cycle range.
C
        IF  ( ( tstart ( : 5 ) .eq. 'FIRST' ) .and.
     +        ( QTYPE ( tstart (6:6) ) ) )  THEN
            rngtyp = 2
            RETURN
          ELSE IF  ( ( tstop ( : 4 ) .eq. 'LAST' ) .and.
     +        ( QTYPE ( tstop (5:5) ) ) )  THEN
            rngtyp = 2
            RETURN
        END IF
C
C*      Check for first character being valid.
C*      Assume this format indicates a forecast range.
C
        IF  ( QTYPE ( tstart (1:1) ) .or. 
     +		     QTYPE ( tstop  (1:1) ) )  THEN
            rngtyp = 1
            RETURN
        END IF
C*
	RETURN
	END
