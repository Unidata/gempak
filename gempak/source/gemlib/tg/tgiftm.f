	SUBROUTINE TG_IFTM  ( ftype, ftime, ifcast, iret )	
C************************************************************************
C* TG_IFTM								*
C*									*
C* This subroutine converts the grid forecast type and time into	*
C* an integer forecast time.						*
C*									*
C* TG_IFTM  ( FTYPE, FTIME, IFCAST, IRET )				*
C*									*
C* Input parameters:							*
C*	FTYPE		CHAR*1		Forecast type ( A,F,G )		*
C*	FTIME		CHAR*		Forecast time ( hhhmm )		*
C*									*
C* Output parameters:							*
C*	IFCAST		INTEGER		GEMPAK forecast time		*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = invalid forecast type	*
C*					 -3 = invalid forecast time	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 1/88	GEMPAK 4				*
C* M. desJardins/GSFC	 5/89	Added I forecast type			*
C* S. Jacobs/EAI	 9/93	Changed to allow 3 digits for hour	*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	CHARACTER*(*)	ftype, ftime
C*
	CHARACTER	f1*1
C------------------------------------------------------------------------
	ifcast = 0
	iret   = 0
C
C*	Get length of forecast time.
C
	CALL ST_LSTR  ( ftime, lenf, ier )
C
C*	If there is no information, return 0.
C
	IF  ( lenf .eq. 0 )  THEN
	    RETURN
	  ELSE IF  ( lenf .gt. 5 )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Convert forecast time to integer.
C
	CALL ST_INTG  ( ftime ( : lenf ), ihhhmm, ier )
C
C*	Check for error in converting string.
C
	IF  ( ier .ne. 0 )  THEN
	    iret = -3
	    RETURN
	END IF
C
C*	If there were one, two or three characters, assume integer
C*	was hour value.
C
	IF  ( lenf .le. 3 )  ihhhmm = ihhhmm * 100
C
C*	Get value for forecast type.
C
	f1 = ftype
	IF  ( f1 .eq. 'F' )  THEN
	    ifval = 1
	  ELSE IF  ( f1 .eq. 'G' )  THEN
	    ifval = 2
	  ELSE IF  ( f1 .eq. 'A' )  THEN
	    ifval = 0
	  ELSE IF  ( f1 .eq. 'I' )  THEN
	    ifval = 3
	  ELSE
	    iret  = -2
	    RETURN
	END IF
C
C*	Combine two values into grid time.
C
	ifcast = ifval * 100000 + ihhhmm
C*
	RETURN
	END
