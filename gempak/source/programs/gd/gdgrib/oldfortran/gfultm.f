	SUBROUTINE GFULTM  ( gdattm, lasttm, fulltm, iret )
C************************************************************************
C* GFULTM								*
C*									*
C* This subroutine converts the user input for a single grid time	*
C* into a full grid time string.  The string must start with a 4-digit	*
C* year.  Other fields may be abbreviated in accord with GEMPAK time	*
C* conventions.								*
C*									*
C* GFULTM  ( GDATTM, LASTTM, FULLTM, IRET )				*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		Input grid time			*
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
C* K. Brill/HPC		 9/99	Adpated from TG_FULL			*
C* K. Brill/HPC		 3/00	Avoid character assignment to itself	*
C************************************************************************
	CHARACTER*(*)	gdattm, lasttm, fulltm
C*
	CHARACTER	dtime*20, time*20, ftype*1, ftime*8, ttt*20
	INTEGER		intdtf (3)
C------------------------------------------------------------------------
	iret   = 0
	fulltm = ' '
	CALL ST_LCUC  ( gdattm, time, ier )
	ttt = time (3:)
	time = ttt
C
C*	Check that input string is not blank.
C
	IF  ( time .eq. ' ' )  THEN
	    iret = -1
	    RETURN
	END IF
	IF  ( ( time (1:1) .eq. 'F' ) .or. 
     +	      ( time (1:1) .eq. 'A' ) .or.
     +	      ( time (1:1) .eq. 'G' ) .or.
     +	      ( time (1:1) .eq. 'V' ) .or.
     +	      ( time (1:1) .eq. 'I' ) )  THEN
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
	dtime = time
	IF  ( ia .ne. 0 )  THEN
	    ftype = 'A'
	    ftime = time ( ia+1 : )
	    time  = dtime ( 1 : ia-1 )
	  ELSE IF  ( if .ne. 0 )  THEN
	    ftype = 'F'
	    ftime = time ( if+1 : )
	    time  = dtime ( 1 : if-1 )
	  ELSE IF  ( ig .ne. 0 )  THEN
	    ftype = 'G'
	    ftime = time ( ig+1 : )
	    time  = dtime ( 1 : ig-1 )
	  ELSE IF  ( iv .ne. 0 )  THEN
	    ftype = 'V'
	    ftime = time ( iv+1 : )
	    time  = dtime ( 1 : iv-1 )
	  ELSE IF  ( ii .ne. 0 )  THEN
	    ftype = 'I'
	    ftime = time ( ii+1 : )
	    time  = dtime ( 1 : ii-1 )
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
