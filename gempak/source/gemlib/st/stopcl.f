	SUBROUTINE ST_OPCL ( instr, locopn, outstr, loccls, iret )
C************************************************************************
C* ST_OPCL								*
C*									*
C* This subroutine looks at the character in position locopn of the	*
C* input character string.  It then looks ahead in the string to find	*
C* the occurrence of the closing character matching the opening		*
C* character.  If necessary, extra closing characters are added to	*
C* the end of the string until the matching one is obtained.  When 	*
C* this occurs, OUTSTR is different from INSTR; otherwise, INSTR and	*
C* OUTSTR are identical.						*  
C*									*
C* ST_OPCL ( INSTR, LOCOPN, OUTSTR, LOCCLS, IRET )			*
C*									*
C* Input parameters:							*
C*	INSTR		CHAR*		Input character string		*
C*	LOCOPN		INTEGER		Location of open character	*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		Output character string		*
C*	LOCCLS		INTEGER		Location of closing character	*
C*	IRET		INTEGER		Return code			*
C*				 	 0 = normal return 		*
C**									*
C* Log:									*
C* T. Lee/SAIC		01/05						*
C************************************************************************
	CHARACTER*(*)	instr, outstr
C*
	CHARACTER*1	c, rc
C*------------------------------------------------------------------------
	iret = 0
C
	outstr = instr
	c = instr ( locopn:locopn )
	IF  ( c .eq. '(' )  THEN
	    rc = ')'
	  ELSE IF ( c .eq. '[' )  THEN
	    rc = ']'
	  ELSE IF ( c .eq. '{' )  THEN
	    rc = '}'
	  ELSE
	    rc = c
	END IF
C
	CALL ST_LSTR ( instr, lens, ier )
C	
C*	Look for LOCCLS.
C
	num = 1
	DO i = ( locopn + 1 ),  lens
	    IF  ( outstr ( i:i ) .eq. c )  THEN
		num = num + 1
	      ELSE IF ( outstr ( i:i ) .eq. rc )  THEN
		num = num - 1
	    END IF 
C
	    IF  ( num .eq. 0 )  THEN
		loccls = i
		RETURN 
	    END IF
	END DO
C
C*	Append the closing character if num is not 0.
C
	IF  (  num .ne. 0 )  THEN
	    iret = + 7
	    CALL ER_WMSG ( 'ST', iret, ' ', ier )
	END IF
C
	DO  i = num, 1, -1
	    outstr = outstr ( : lens ) // rc
	    CALL ST_LSTR ( outstr, lens, ier )
	END DO
C
	CALL ST_LSTR ( outstr, loccls, ier )
C*
	RETURN
	END
