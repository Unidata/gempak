	FUNCTION PT_WNUM  ( wthr )
C************************************************************************
C* PT_WNUM								*
C*									*
C* This function converts any character weather code into a GEMPAK	*
C* weather number, WNUM:						*
C*									*
C*                   WNUM = PT_WNUM ( WTHR )				*
C*									*
C* WNUM can be converted to a character weather code, WCOD, using	*
C* the function PT_WCOD.  The range of numbers which might result       *
C* is -3 to 512000.                     				*
C*									*
C* REAL PT_WNUM  ( WTHR )						*
C*									*
C* Input parameters:							*
C*	WTHR		CHAR*		Character weather string	*
C*									*
C* Output parameters:							*
C*	PT_WNUM		REAL		Weather number			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/87						*
C* S. Schotz/GSC	10/89	Documentation				*
C* M. desJardins/GSFC	11/89	Added SPW				*
C* S. Jacobs/NCEP	11/96	Added Q, V, PO, UP, BD+, BN+, BS+	*
C************************************************************************
	CHARACTER*(*)	wthr
C*
	CHARACTER	input*80, w1*1, w2*2, w3*3, w4*4
	REAL		fnum ( 3 )
C
C*	The following table of parts is used to assign number to the
C*	weather parts.  If new names are added to the table, the
C*	longer codes must follow shorter ones.  Also, the parameters
C*	which locate parts of various lengths must be updated.  The
C*	list must also be changed in PT_WCOD.
C
	CHARACTER	wpart (79)*4
	DATA		wpart / 'R', 'L', 'S', 'A', 'T', 'H', 'K', 
     +				'D', 'F', 'Q', 'V', ' ',
     +			        'R-', 'R+', 'ZR', 'RW', 'L-', 'L+',
     +				'ZL', 'S-', 'S+', 'SW', 'IP', 'SG', 
     +				'SP', 'A-', 'A+', 'T-', 'T+', 'IF', 
     +				'GF', 'BS', 'BD', 'BY', 'BN', 'IC',
     +				'IN', 'AP', 'KH', 'PO', 'UP', 7 * ' ',
     +				'ZR-', 'ZR+', 'RW-', 'RW+', 'ZL-',
     +				'ZL+', 'SW-', 'SW+', 'IP-', 'IP+',
     +				'SG-', 'SG+', 'SP-', 'SP+', 'IPW',
     +				'IC-', 'IC+', 'TRW', 'SPW', 'BD+',
     +				'BN+', 'BS+', 4 * ' ',
     +				'IPW-', 'IPW+', 'TRW-', 'TRW+', ' ' /
	DATA 		i1strt, i1stop, i2strt, i2stop, i3strt, i3stop,
     +			i4strt, i4stop / 1, 11, 13, 41, 49, 67, 75, 78 /
C------------------------------------------------------------------------
C*	Move input code to new variable and get length.
C
	input = wthr
	CALL ST_LSTR ( input, length, ier )
C
C*	Check for blank string and for special codes.
C
	IF  ( length .eq. 0 )  THEN
	    PT_WNUM = 0.
	    RETURN
	  ELSE IF  ( input ( 1 : 5 ) .eq. 'TORNA' )  THEN
	    PT_WNUM = -1.
	    RETURN
	  ELSE IF  ( input ( 1 : 5 ) .eq. 'FUNNE' )  THEN
	    PT_WNUM = -2.
	    RETURN
	  ELSE IF  ( input ( 1 : 5 ) .eq. 'WATER' )  THEN
	    PT_WNUM = -3.
	    RETURN
	END IF
C
C*	Loop until string is all used, until three parts are found,
C*	or until an unknown part is encountered.
C
	fnum (1) = 0
	fnum (2) = 0
	fnum (3) = 0
	iprt = 1
	DO WHILE  ( length .gt. 0 )
C
C*	    First check four character strings.
C
	    num = 0
	    w4  = input ( 1 : 4 )
	    DO  i = i4strt, i4stop
		IF  ( w4 .eq. wpart ( i ) ) num = i
	    END DO
C
C*	    If found, eliminate from string and fix length.
C
	    IF  ( num .ne. 0 )  THEN
		input  = input ( 5 : )
		length = length - 4
C
C*		Otherwise, check three character symbols.
C
	      ELSE 
		w3 = input ( 1 : 3 )
		DO  i = i3strt, i3stop
		    IF  ( w3 .eq. wpart ( i ) ) num = i
		END DO
C
C*		If found, eliminate from string and fix length.
C
		IF  ( num .ne. 0 )  THEN
		    input  = input ( 4 : )
		    length = length - 3
C
C*		    Otherwise, check two character symbols.
C
		  ELSE
		    w2 = input ( 1 : 2 )
		    DO  i = i2strt, i2stop
			IF  ( w2 .eq. wpart ( i ) )  num = i
		    END DO
C
C*		    If found, eliminate from string and fix length.
C
		    IF  ( num .ne. 0 )  THEN
			input  = input ( 3 : )
			length = length - 2
C
C*			Otherwise, check one character symbols.
C
		      ELSE
			w1 = input ( 1 : 1 )
			DO  i = i1strt, i1stop
			    IF  ( w1 .eq. wpart ( i ) )  num = i
			END DO
C
C*			If found, eliminate from string and fix length.
C
			IF  ( num .ne. 0 )  THEN
			    input  = input ( 2 : )
			    length = length - 1
C
C*			    Otherwise, an error was encountered.
C
			  ELSE
			    length = 0
			END IF
		    END IF
		END IF
	    END IF
C
C*	    Save this number.
C
	    fnum ( iprt ) = num
	    iprt = iprt + 1
	    IF  ( iprt .gt. 3 ) length = 0
C
C*	    Check that first character is not a + or -.
C
	    IF  (( input (1:1) .eq. '+' ) .or. ( input (1:1) .eq. '-' ))
     +							THEN
		input  = input ( 2 : )
		length = length - 1
	    END IF
	END DO
C
C*	Compute number.
C
	PT_WNUM = fnum (3) * 80 * 80 + fnum (2) * 80 + fnum (1)
C*
	RETURN
	END
