	SUBROUTINE ST_INTG  ( string, intg, iret )
C************************************************************************
C* ST_INTG								*
C*									*
C* This subroutine decodes a character string into an integer.  If 	*
C* the string cannot be decoded, INTG is set to IMISSD.  Note that	*
C* only the substring containing the digits to be decoded should be	*
C* sent to this subroutine, rather than a string with trailing blanks.	*
C*									*
C* ST_INTG  ( STRING, INTG, IRET )					*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Input string			*
C*									*
C* Output parameters:							*
C*	INTG		INTEGER		Decoded integer			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = conversion error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/86						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Whistler/SSAI	 7/91	Modified internal read			*
C* K. Brill/NMC		 9/91	Added call to ST_CKNI			*
C* M. desJardins/NMC	 3/92	Recode to eliminate special cases	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C*
	CHARACTER	sss*12, c*1
	LOGICAL		good, plus
C------------------------------------------------------------------------
	iret = -2
	intg = IMISSD
C
C*	Assume that string does not have embedded or trailing blanks.
C
	lens = LEN ( string )
	IF  ( lens .gt. 12 ) lens = 12
	sss  = string
C
C*	Check for empty string.
C
	IF  ( lens .eq. 0 )  RETURN
C
C*	Check for + or - in first character.
C
	IF  ( sss (1:1) .eq. '+' )  THEN
	    ibeg = 2
	    plus = .true.
	  ELSE IF  ( sss (1:1) .eq. '-' )  THEN
	    ibeg = 2
	    plus = .false.
	  ELSE
	    ibeg = 1
	    plus = .true.
	END IF
	IF  ( ibeg .gt. lens )  RETURN
C
C*	Now loop through all characters and turn into integer.
C
	ival0 = ICHAR ( '0' )
	intg  = 0
	good  = .true.
	i     = ibeg
	DO  WHILE  ( good .and. ( i .le. lens ) )
	    c     = sss (i:i)
	    ivalc = ICHAR ( c ) - ival0
	    IF  ( ( ivalc .ge. 0 ) .and. ( ivalc .le. 9 ) )  THEN
		intg  = intg * 10 + ivalc
		i     = i + 1
	      ELSE
		good  = .false.
	    END IF
	END DO
C
C*	Check for good value and add sign.
C
	IF  ( .not. good )  THEN
	    intg = IMISSD
	  ELSE IF  ( plus )  THEN
	    iret = 0
	  ELSE
	    iret = 0
	    intg = -intg
	END IF
C*
	RETURN
	END
