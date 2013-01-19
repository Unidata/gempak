	FUNCTION PT_WNMT  ( wcmt )
C************************************************************************
C* PT_WNMT								*
C*									*
C* This function converts any METAR character weather code into a       *
C* GEMPAK weather number, WNUM:						*
C*									*
C*                   WNUM = PT_WNMT ( WCMT )				*
C*									*
C* The range of numbers which might result is -2 to 512000.  WNUM can   *
C* be converted to a character weather code, WCOD, using the function	*
C* PT_WCOD.  (The character weather code which results will be the      *
C* GEMPAK weather code, not the METAR weather code.  For example, if    *
C* the original METAR code was '+RA', the code returned by PT_WCOD would*
C* be 'R+'.)                                                            *
C*									*
C* REAL PT_WNMT  ( WCMT )						*
C*									*
C* Input parameters:							*
C*	WCMT		CHAR*		Character weather string	*
C*									*
C* Output parameters:							*
C*	PT_WNMT		REAL		Weather number			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	11/96	Copied from PT_WNUM			*
C* D. Kidwell/NCEP	 5/98	If FC or +FC found, ignore other weather*
C* D. Kidwell/NCEP	 7/98	Changed all PE references to PL; fixed  *
C*                              prologue                                *
C* D. Kidwell/NCEP	12/01	Changed BR frm 31 to 9, MIFG frm 9 to 31*
C* S. Chiswell/Unidata	 5/07	Modified to keep '+' and '-' in compound*
C*				weather groups				*
C************************************************************************
	CHARACTER*(*)	wcmt
C*
	CHARACTER	input*80, tstr*80, w1*2, w2*3, w3*4, w4*5
	REAL		fnum ( 3 )
	LOGICAL		again
C
C*	The following table of parts is used to assign a number to each
C*	weather part.  If new names are added to the table, the
C*	longer codes must follow shorter ones.  Also, the parameters
C*	which locate parts of various lengths must be updated.  The
C*	method of computing the final weather number requires that no
C*      more than 79 weather parts be specified.
C
	CHARACTER	wpart (74)*5
	INTEGER		jpnum (74)
	DATA		wpart / 'BR', 'DS', 'DU', 'DZ', 'FC', 'FG',
     +				'FU', 'GR', 'GS', 'HZ', 'IC', 'PL',
     +				'PO', 'RA', 'SA', 'SG', 'SN', 'SQ',
     +				'SS', 'TS', 'UP', 'VA',
     +				'+DS', '-DZ', '+DZ', '+FC', '-GS',
     +				'+GS', '-PL', '+PL', '-RA', '+RA',
     +				'-SG', '+SG', '-SN', '+SN', '+SS',
     +				'BCFG', 'BLDU', 'BLPY', 'BLSA', 'BLSN',
     +				'DRDU', 'DRSA', 'DRSN', 'FZDZ', 'FZFG',
     +				'FZRA', 'MIFG', 'PRFG', 'SHGR', 'SHGS',
     +				'SHPL', 'SHRA', 'SHSN', 'TSRA',
     +				'+BLDU', '+BLSA', '+BLSN', '-FZDZ',
     +				'+FZDZ', '+FZFG', '-FZRA', '+FZRA',
     +				'-SHGS', '+SHGS', '-SHPL', '+SHPL',
     +				'-SHRA', '+SHRA', '-SHSN', '+SHSN',
     +				'-TSRA', '+TSRA' /
	DATA		jpnum /  9, 33,  8,  2, -2,  9, 
     +				 7,  4, 25,  6, 36, 23, 
     +				40,  1, 35, 24,  3, 10, 
     +				35,  5, 41, 11, 
     +				68, 17, 18, -1, 61, 
     +				62, 57, 58, 13, 14, 
     +				59, 60, 20, 21, 69, 
     +				 9, 33, 34, 35, 32, 
     +				33, 35, 32, 19, 30, 
     +				15, 31,  9, 27, 67, 
     +				63, 16, 22, 66, 
     +				68, 69, 70, 53, 
     +				54, 30, 49, 50, 
     +				67, 67, 75, 76, 
     +				51, 52, 55, 56, 
     +				77, 78 /
	DATA 		i1strt, i1stop, i2strt, i2stop, i3strt, i3stop,
     +			i4strt, i4stop / 1, 22, 23, 37, 38, 56, 57, 74 /
C------------------------------------------------------------------------
C*	Move input code to new variable and get length.
C
	input = wcmt
	CALL ST_LSTR ( input, length, ier )
C
C*	Check for blank string.
C
	IF  ( length .eq. 0 )  THEN
	    PT_WNMT = 0.
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
	    again = .false.
	    num = 0
	    w4  = input ( 1 : 5 )
	    DO  i = i4strt, i4stop
		IF  ( w4 .eq. wpart ( i ) ) num = jpnum ( i )
	    END DO
C
C*	    If found, eliminate from string and fix length.
C
	    IF  ( num .ne. 0 )  THEN
		input  = input ( 6 : )
		length = length - 5
C
C*		Otherwise, check three character symbols.
C
	      ELSE 
		w3 = input ( 1 : 4 )
		DO  i = i3strt, i3stop
		    IF  ( w3 .eq. wpart ( i ) ) num = jpnum ( i )
		END DO
C
C*		If found, eliminate from string and fix length.
C
		IF  ( num .ne. 0 )  THEN
		    IF  ( ( input (1:1) .eq. '+' ) .or.
     +                  ( input (1:1) .eq. '-' ) )  THEN
			tstr  = input(1:1) // input(5:)
			input = tstr
			length = length - 3
		    ELSE
		        input  = input ( 5 : )
		        length = length - 4
		    END IF
C
C*		    Otherwise, check two character symbols.
C
		  ELSE
		    w2 = input ( 1 : 3 )
		    DO  i = i2strt, i2stop
			IF  ( w2 .eq. wpart ( i ) )  num = jpnum ( i )
		    END DO
C
C*		    If found, eliminate from string and fix length.
C
		    IF  ( num .ne. 0 )  THEN
			IF  ( ( input (1:1) .eq. '+' ) .or.
     +                            ( input (1:1) .eq. '-' ) )  THEN
			    tstr  = input(1:1) // input(4:)
			    input = tstr
			    length = length - 2
			ELSE
			    input  = input ( 4 : )
			    length = length - 3
			END IF
C
C*			Otherwise, check one character symbols.
C
		      ELSE
			w1 = input ( 1 : 2 )
			DO  i = i1strt, i1stop
			    IF  ( w1 .eq. wpart ( i ) )  num = jpnum(i)
			END DO
C
C*			If found, eliminate from string and fix length.
C
			IF  ( num .ne. 0 )  THEN
			    IF  ( ( input (1:1) .eq. '+' ) .or.
     +                            ( input (1:1) .eq. '-' ) )  THEN
			        tstr  = input(1:1) // input(3:)
			        input = tstr
			        length = length - 1
			    ELSE
			        input  = input ( 3 : )
			        length = length - 2
			    END IF
C
C*			    Otherwise, the code was not found.
C
			  ELSE
C
C*			    Check for unknown ID.
C
			    IF  ( ( input (1:1) .eq. '+' ) .or.
     +				  ( input (1:1) .eq. '-' ) )  THEN
				tstr  = input(2:3) // input(1:1) //
     +					input(4:)
				input = tstr
				again = .true.
			      ELSE
				length = 0
			    END IF
			END IF
		    END IF
		END IF
	    END IF
C
C*	    Save this number.
C
	    IF  ( .not. again )  THEN
		fnum ( iprt ) = num
		iprt = iprt + 1
		IF  ( iprt .gt. 3 ) length = 0
	    END IF
	END DO
C
C*	Compute number.  Check for FC (funnel cloud) or +FC (tornado
C*	or waterspout).  If found, ignore all other weather.
C
	IF ( ( fnum (1) .ge. 0. ) .and. ( fnum (2) .ge. 0. ) .and.
     +	     ( fnum (3) .ge. 0. ) ) THEN  
	    PT_WNMT = fnum (3) * 80 * 80 + fnum (2) * 80 + fnum (1)
	  ELSE
      	    PT_WNMT = AMIN1 ( fnum (1), fnum (2), fnum (3) )
 	END IF
C*
	RETURN
	END
