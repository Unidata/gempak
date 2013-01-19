	SUBROUTINE SNSTTL  ( title, timflg, stn, time, ntime,
     +			     icttl, linttl, ttlstr, shrttl, iret )
C************************************************************************
C* SNSTTL								*
C*									*
C* This subroutine writes a title for the cross-section program.	*
C* If this is a cross section, the time is written; if it is a time	*
C* section, the time is written.					*
C*									*
C* SNSTTL  ( TITLE, TIMFLG, STN, TIME, NTIME, ICTTL, LINTTL, TTLSTR,	*
C*		SHRTTL, IRET )						*
C*									*
C* Input parameters:							*
C*	TITLE		CHAR*		Title color/line/string		*
C*	TIMFLG		LOGICAL		Time section flag		*
C*	STN		CHAR*		Station				*
C*	TIME(*)		CHAR*		GEMPAK date/time		*
C*	NTIME		INTEGER		Number of times			*
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
C* M. desJardins/GSFC	12/85						*
C* G. Huffman/GSC	11/88	Documentation, revised TITLE global	*
C* M. desJardins/GSFC	 4/91	Add time to time section		*
C* S. Jacobs/EAI	11/92	Added construction of short title
C************************************************************************
	CHARACTER*(*)	title, time (*), stn, ttlstr, shrttl
	LOGICAL		timflg
C*
	CHARACTER	ttlinp*72
	LOGICAL		short
C------------------------------------------------------------------------
	iret = 0
C
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
C*	Break TITLE into color, line, and text string.
C
	CALL IN_TITL  ( ttlinp, 0, icttl, linttl, ttlstr, ier )
C
C*	Proceed if color is non-zero.
C
	IF  ( icttl .ne. 0 )  THEN
C
C*	    If no string was entered, use the default.
C
	    IF  ( ttlstr .eq. ' ' )  THEN
		IF  ( timflg )  THEN
		    CALL ST_LCUC  ( stn, ttlstr, ier )
		    CALL ST_LSTR  ( ttlstr, lent, ier )
		    lent = lent + 2
		    ttlstr ( lent: ) = time (1) (1:11) 
		    CALL ST_LSTR  ( ttlstr, lent, ier )
		    lent = lent + 1
		    ttlstr ( lent: ) =  '-' // time (ntime)
		  ELSE
		    ttlstr = time (1)
		END IF
	    END IF
	END IF
C
C*	Construct short title.
C
	IF  ( .not. short )  THEN
	    CALL ST_LSTR ( stn, len1, ier )
	    IF  ( timflg )  THEN
		shrttl = 'TIME-SECTION ' // stn(:len1) // ' ' //
     +			 time(1)(5:9) // '-' // time(ntime)(5:9)
	    ELSE
		shrttl = 'CROSS-SECTION ' // stn(:len1) // ' ' //
     +			 time(1)(5:9)
	    END IF
	END IF
C*
	RETURN
	END
