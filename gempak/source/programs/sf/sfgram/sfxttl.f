	SUBROUTINE SFXTTL  ( title, stns, mtrace, ctime, icttl,
     +			     linttl, ttlstr, shrttl, iret )
C************************************************************************
C* SFXTTL								*
C*									*
C* This subroutine adds a title to the meteorogram.			*
C*									*
C* SFXTTL  ( TITLE, STNS, MTRACE, CTIME, ICTTL, LINTTL, TTLSTR, SHRTTL, *
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	TITLE		CHAR*		User input for title		*
C*	STNS (MTRACE)	CHAR*		Stations for traces		*
C*	MTRACE		INTEGER		Number of stations		*
C*	CTIME		CHAR*		Time range			*
C*									*
C* Output parameters:							*
C*	ICTTL		INTEGER		Title color			*
C*	LINTTL		INTEGER		Title line			*
C*	TTLSTR		CHAR*		Title string			*
C*	SHRTTL		CHAR*		Short title			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/90						*
C* S. Jacobs/EAI	11/92	Added construction of short title	*
C*				    and return title info to main prog	*
C* S. Jacobs/NCEP	 1/05	Changed meteogram to meteorogram	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	title, stns (*), ctime, shrttl, ttlstr
C*
	LOGICAL		onestn, short
	CHARACTER	oldstn*8, ttlinp*72
C------------------------------------------------------------------------
	iret = 0
	ipbar = INDEX ( title, '|' )
	IF  ( ipbar .ne. 0 )  THEN
	    short  = .true.
	    shrttl = title(ipbar+1:)
	    IF  ( ipbar .eq. 1 )  THEN
		ttlinp = ' '
	    ELSE
		ttlinp = title(:ipbar-1)
	    END IF
	ELSE
	    short  = .false.
	    ttlinp = title
	END IF
C
C*	Get the user input for title.
C
	CALL IN_TITL  ( ttlinp, 0, icttl, linttl, ttlstr, ier )
C
C*	Get a title if the title string is blank.
C
	IF  ( ttlstr .eq. ' ' )  THEN
C
C*	    Check for more than one station.
C
	    onestn = .true.
	    oldstn = ' '
	    DO  i = 1, mtrace
		IF  ( ( oldstn .ne. ' ' ) .and. 
     +		      ( stns (i) .ne. ' ' ) .and.
     +		      ( oldstn .ne. stns (i) ) )  THEN
		    onestn = .false.
		END IF
		IF  ( stns (i) .ne. ' ' )  oldstn = stns (i)
	    END DO
C
C*	    Put the stations in the string.
C
	    lent = 0
	    IF  ( onestn )  THEN
		CALL ST_LCUC  ( oldstn, oldstn, ier )
		CALL ST_LSTR  ( oldstn, lens, ier )
		ttlstr = oldstn
		lent   = lens + 2
	      ELSE
		DO  i = 1, mtrace
		    CALL ST_LCUC  ( stns (i), oldstn, ier )
		    CALL ST_LSTR  ( oldstn, lens, ier )
		    IF  ( lens .gt. 0 )  THEN
			ttlstr ( lent+1: ) = oldstn
			lent = lent + lens + 2
		    END IF
		END DO
	    END IF
C
C*	    Add the time string.
C
	    ttlstr ( lent+1: ) = ctime
	END IF
C
C*	Construct short title.
C
	IF  ( .not. short )  THEN
	    CALL ST_LSTR ( ctime, lenc, ier )
	    shrttl = 'METEOROGRAM ' // ctime(:lenc)
	    CALL ST_LSTR ( shrttl, lensh, ier )
	    CALL ST_LSTR ( stns(1), lens, ier )
	    shrttl(lensh+2:) = stns(1)(:lens)
	    CALL ST_LSTR ( shrttl, lensh, ier )
	    oldstn = stns(1)
	    DO  j = 2, mtrace
		IF  ( ( oldstn .ne. ' ' ) .and. 
     +		      ( stns (j) .ne. ' ' ) .and.
     +		      ( oldstn .ne. stns (j) ) )  THEN
		    CALL ST_LSTR ( stns(j), lens, ier )
		    shrttl(lensh+2:) = stns(j)(:lens)
		    CALL ST_LSTR ( shrttl, lensh, ier )
		    oldstn = stns(j)
		END IF
	    END DO
	END IF
C*
	RETURN
	END
