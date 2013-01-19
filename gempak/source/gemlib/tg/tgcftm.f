	SUBROUTINE TG_CFTM  ( ifcast, ftype, ftime, iret )	
C************************************************************************
C* TG_CFTM								*
C*									*
C* This subroutine converts an integer grid forecast time into		*
C* the character forecast type and time.  The forecast type 		*
C* is  A (analysis), F (forecast), G (guess) or I (initialize).		*
C* If the forecast time is less than 100 and the minutes are 00,	*
C* only hh is returned.							*
C*									*
C* TG_CFTM  ( IFCAST, FTYPE, FTIME, IRET )				*
C*									*
C* Input parameters:							*
C*	IFCAST		INTEGER		GEMPAK grid time		*
C*									*
C* Output parameters:							*
C*	FTYPE		CHAR*1		Forecast type ( A,F,G,I )	*
C*	FTIME		CHAR*		Forecast time ( hhhmm )		*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = invalid forecast type	*
C*					 -3 = invalid forecast time	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 1/88	GEMPAK 4				*
C* M. desJardins/GSFC	 5/89	Add I type				*
C* S. Jacobs/EAI	 9/93	Changed to always output 5 digits	*
C* S. Jacobs/EAI	 9/93	Output 5 digits only if minutes present	*
C************************************************************************
	CHARACTER*(*)	ftype, ftime
C*
	CHARACTER	fff*8
C------------------------------------------------------------------------
	iret  = 0
	ftype = ' '
	ftime = ' '
C
C*	Check for negative times.
C
	IF  ( ifcast .lt. 0 )  THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Get the number representing forecast type and convert to a 
C*	character.
C
	iftype = ifcast / 100000
	IF  ( iftype .eq. 0 )  THEN
	    ftype = 'A'
	  ELSE IF  ( iftype .eq. 1 )  THEN
	    ftype = 'F'
	  ELSE IF  ( iftype .eq. 2 )  THEN
	    ftype = 'G'
	  ELSE IF  ( iftype .eq. 3 )  THEN
	    ftype = 'I'
	  ELSE
	    iret = -2
	    RETURN
	END IF
C
C*	Convert the time to a character.  Add 100000 so that leading
C*	zeros will be encoded.
C
	iftime = ifcast - iftype * 100000
	ietime = iftime + 100000
	CALL ST_INCH  ( ietime, fff, ier )
C
C*	If the forecast time has minutes, set the character output
C*	to all five digits. Otherwise, use only the first three digits,
C*	which represent hours.
C
	IF  ( MOD (ietime, 100) .eq. 0 )  THEN
	    ftime = fff ( 2 : 4 )
	ELSE
	    ftime = fff ( 2 : )
	END IF
C*
	RETURN
	END
