	SUBROUTINE ST_ABBR  ( string, stabbr, abbr, iret )
C************************************************************************
C* ST_ABBR								*
C*									*
C* This subroutine determines whether the string in STABBR is an	*
C* abbreviation (beginning substring) of STRING.  This comparison	*
C* is case sensitive.							*
C*									*
C* ST_ABBR  ( STRING, STABBR, ABBR, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Full string			*
C*	STABBR		CHAR*		Abbreviation			*
C*									*
C* Output parameters:							*
C*	ABBR		LOGICAL 	Abbreviation flag		*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/84						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 5/93	Assume strings are already right case	*
C*				Use LEN instead of LSTR for STRING	*
C************************************************************************
	CHARACTER*(*)	string, stabbr
	LOGICAL		abbr
C-----------------------------------------------------------------------
	abbr = .false.
	iret = 0
C
C*	Get lengths.
C
	len1 = LEN ( string )
	CALL ST_LSTR  ( stabbr, len2, ier )
C
C*	Check strings if abbreviation is shorter than string.
C
	IF  ( ( len2 .gt. 0 ) .and. ( len1 .ge. len2 ) )  THEN
	    IF  ( string (1:len2) .eq. stabbr (1:len2) )  abbr = .true.
	END IF
C*
	RETURN
	END
