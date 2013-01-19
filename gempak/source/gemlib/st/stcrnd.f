	SUBROUTINE  ST_CRND  ( string, value, ndec, iret )
C************************************************************************
C* ST_CRND								*
C*									*
C* This subroutine converts a character string to a real number.  If	*
C* the conversion fails, RMISSD is returned.				*
C*									*
C* ST_CRND  ( STRING, VALUE, NDEC, IRET ) 				*
C*									*
C* Input parameters: 							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	VALUE		REAL		Real number			*
C*	NDEC		INTEGER		Number of decimal places	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = conversion error 		*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 4/97 	Copied from ST_CRNM			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C*
	CHARACTER	sss*24, ttt*4, c*1
	LOGICAL		good, plus, before
C------------------------------------------------------------------------
	iret = -2
	value = RMISSD
C
C*	Remove blanks from string.
C
	CALL ST_RMBL  ( string, sss, lens, ier )
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
C*	Now loop through all characters and turn into integers corresponding
C*	to values before and after decimal point and for exponent.
C
	ival0  = ICHAR ( '0' )
	ibefor = 0
	iafter = 0
	nafter = 0
	iexp   = 0
	good   = .true.
	i      = ibeg
	before = .true.
	DO  WHILE  ( good .and. ( i .le. lens ) )
	    c     = sss (i:i)
	    IF  ( ( c .ge. '0' ) .and. ( c .le. '9' ) )  THEN
		ivalc = ICHAR (c) - ival0
		IF  ( before )  THEN
		    ibefor = ibefor * 10 + ivalc
		  ELSE
		    iafter = iafter * 10 + ivalc
		    nafter = nafter + 1
		END IF
	      ELSE IF  ( c .eq. '.' )  THEN
		IF  ( before )  THEN
		    before = .false.
		  ELSE
		    good   = .false.
		END IF
	      ELSE IF  ( ( ( c .eq. 'E' ) .or. ( c .eq. 'e' ) )
     +                 .and. lens .ne. 1 )  THEN
		IF  ( i .lt. lens )  THEN
		    ttt = sss ( i+1 : )
		    CALL ST_NUMB  ( ttt, iexp, ier )
		    IF  ( ier .ne. 0 )  good = .false.
		END IF
		i = lens
	      ELSE
		good = .false.
	    END IF
	    i = i + 1
	END DO
C
C*	Compute the value using the three parts.
C
	IF  ( good )  THEN
	    iret = 0
	    value = FLOAT ( ibefor )
	    IF  ( nafter .gt. 0 )  THEN
		value = value + ( FLOAT ( iafter ) / 10. ** nafter )
	    END IF
	    IF  ( iexp .ne. 0 )  THEN
C
C*		If the exponent is greater than 20, set the value
C*		to missing.
C
		IF  ( ABS ( iexp ) .gt. 20 )  THEN
		    value = RMISSD
		    RETURN
		  ELSE
		    value = value * 10. ** iexp
		END IF
	    END IF
	    IF  ( .not. plus )  value = -value
	END IF
C
	ndec = nafter
C*
	RETURN
	END
