	SUBROUTINE TG_FULL  ( gdattm, firstm, lasttm, fulltm, iret )
C************************************************************************
C* TG_FULL								*
C*									*
C* This subroutine converts the user input for a single grid time	*
C* into a full grid time string.					*
C*									*
C* TG_FULL  ( GDATTM, FIRSTM, LASTTM, FULLTM, IRET )			*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		Input grid time			*
C*	FIRSTM		CHAR*		First time in grid file		*
C*	LASTTM		CHAR*		Last time in grid file		*
C*									*
C* Output parameters:							*
C*	FULLTM		CHAR*		Full GEMPAK grid time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid date or time	*
C*					 -2 = invalid forecast type	*
C*					 -3 = invalid forecast time	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89	Changed grid time routines		*
C* M. desJardins/GSFC	 5/89	Add last time if only Fxx is entered	*
C*				Add I time				*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* J. Whistler/SSAI	 4/91	Don't assume same grid type as lasttm	*
C*				if not specified in time, assume 'A'	*
C************************************************************************
	CHARACTER*(*)	gdattm, firstm, lasttm, fulltm
C*
	CHARACTER	time*20, ftype*1, ftime*8, ttt*20
	INTEGER		intdtf (3)
C------------------------------------------------------------------------
	iret   = 0
	fulltm = ' '
	CALL ST_LCUC  ( gdattm, time, ier )
C
C*	Check that input string is not blank.
C
	IF  ( time .eq. ' ' )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Replace FIRST and LAST with correct times and return.
C
	IF  ( time .eq. 'FIRST' )  THEN
	    fulltm = firstm 
	    RETURN
	  ELSE IF  ( time .eq. 'LAST' )  THEN
	    fulltm = lasttm
	    RETURN
	END IF
C
C*	Check for FIRST or LAST followed by forecast type.
C
	IF  ( ( time ( : 5 ) .eq. 'FIRST' ) .and.
     +	      ( ( time (6:6) .eq. 'A' ) .or. ( time (6:6) .eq. 'G' )
     +		.or. ( time (6:6) .eq. 'F' ) .or.
     +		( time (6:6) .eq. 'V' ) .or. ( time (6:6) .eq. 'I' )))
     +							THEN
	    ttt  = time
	    time = firstm ( 1:11 ) // ttt ( 6: )
	  ELSE IF  ( ( time ( : 4 ) .eq. 'LAST' ) .and.
     +	      ( ( time (5:5) .eq. 'A' ) .or. ( time (5:5) .eq. 'G' )
     +		.or. ( time (5:5) .eq. 'F' ) .or.
     +		( time (5:5) .eq. 'V' ) .or. ( time (5:5) .eq. 'I' )))
     +							THEN
	    ttt  = time
	    time = lasttm ( 1:11 ) // ttt ( 5: )
	  ELSE IF  ( ( time (1:1) .eq. 'F' ) .or. 
     +		     ( time (1:1) .eq. 'A' ) .or.
     +		     ( time (1:1) .eq. 'G' ) .or.
     +		     ( time (1:1) .eq. 'V' ) .or.
     +		     ( time (1:1) .eq. 'I' ) )  THEN
	    ttt  = time
	    time = lasttm ( 1:11 ) // ttt ( 1: )
	END IF
C
C*	Search for type within string.
C
	ia = INDEX ( time, 'A' )
	if = INDEX ( time, 'F' )
	ig = INDEX ( time, 'G' )
	iv = INDEX ( time, 'V' )
	ii = INDEX ( time, 'I' )
C
C*	Break the string into three parts containing time, type, 
C*	and forecast time.
C
	IF  ( ia .ne. 0 )  THEN
	    ftype = 'A'
	    ftime = time ( ia+1 : )
	    time  = time ( 1 : ia-1 )
	  ELSE IF  ( if .ne. 0 )  THEN
	    ftype = 'F'
	    ftime = time ( if+1 : )
	    time  = time ( 1 : if-1 )
	  ELSE IF  ( ig .ne. 0 )  THEN
	    ftype = 'G'
	    ftime = time ( ig+1 : )
	    time  = time ( 1 : ig-1 )
	  ELSE IF  ( iv .ne. 0 )  THEN
	    ftype = 'V'
	    ftime = time ( iv+1 : )
	    time  = time ( 1 : iv-1 )
	  ELSE IF  ( ii .ne. 0 )  THEN
	    ftype = 'I'
	    ftime = time ( ii+1 : )
	    time  = time ( 1 : ii-1 )
	  ELSE
	    ftype = 'A'
	    ftime = '00'
	END IF
C
C*	Get the full GEMPAK time.  TIME was reset to the date/time part.
C
	CALL TI_STAN  ( time, lasttm, time, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Build the full grid time.
C
	IF ( ftime .eq. ' ' ) ftime = lasttm (13: )
	fulltm = time ( 1:11 ) // ftype // ftime ( 1:5 )
C
C*	Convert the full string to integers and back to characters.
C*	This will check the forecast time and translate V times to F
C*	times.
C
	CALL TG_CTOI  ( fulltm, intdtf, iret )
	IF  ( iret .eq. 0 )  THEN
	    CALL TG_ITOC  ( intdtf, fulltm, iret )
	  ELSE
	    fulltm = ' '
	END IF
C*
	RETURN
	END
