	SUBROUTINE IN_HILO ( hilo, icolr, csymbl, isymbl, isymtp,
     +			     valflg, iprecn, range, krad, knt,
     +			     intflg, iret )
C************************************************************************
C* IN_HILO								*
C*									*
C* This subroutine scans the user input for HILO.  This consists of	*
C* six input strings separated by slashes.  When a string can be two	*
C* substrings separated by a semicolon, the first substring applies	*
C* to highs, the second to lows.  If no substring is given a default	*
C* is provided.  The table below summarizes the input format and	*
C* gives the defaults.							*
C*									*
C*  STRING	Description        Format      Example     Default	*
C*    1       Color number          i;i           1           0		*
C*    2       Symbol#precision    c#i;c#i      H#2;L#1       H;L	*
C*    3       Range               i-i;i-i       30-40      0-0;0-0	*
C*    4       Search radius	     i            4           3		*
C*    5       Count                 i;i		10;5	    20;20	*
C*    6       Interpolation flag     l            T           F		*
C*									*
C* If the first string is a single number, that number is the color	*
C* number for both highs and lows.  In the second string, the c in the	*
C* table stands for either a character string, or a marker or special 	*
C* symbol number. Special symbols are specified by preceeding the 	*
C* number with an 'S'. The # sign is a flag to plot the values beneath 	*
C* the marker.  The number following # is the number of decimal places	*
C* to display in the value.  If the number is absent, integer values are*
C* displayed.								*
C*									*
C* The defaults above apply if no string is entered.  If one of the	*
C* entries of a string is entered, it is assumed to apply to highs	*
C* unless it is preceded by a semicolon.  The omitted entry defaults	*
C* according to the table below:					*
C*									*
C*    STRING          DEFAULT FOR OMITTED PART				*
C*       1		   Same value					*
C*	 2		   No symbol					*
C*	 3		   Missing values				*
C*	 5		   Same value					*
C*									*
C* All of the output parameters except KRAD and INTFLG involve outputs	*
C* for both highs and lows.  The first output is always for highs.	*
C* For example, ISYMBL (1) is for highs; ISYMBL (2) is for lows.	*
C*									*
C*									*
C* IN_HILO ( HILO, ICOLR, CSYMBL, ISYMBL, ISYMTP, VALFLG, IPRECN,	*
C*	     RANGE, KRAD, KNT, INTFLG, IRET )				*
C*									*
C* Input parameters:							*
C*	HILO		CHAR*		User input			*
C*									*
C* Output parameters:							*
C*	ICOLR  (2)	INTEGER		Color number			*
C*	CSYMBL (2)	CHAR*		Symbol string			*
C*	ISYMBL (2)	INTEGER		Marker number			*
C*	ISYMTP (2)	INTEGER		Symbol type			*
C*					  0 = NONE			*
C*					  1 = CHARACTERS		*
C*					  2 = Marker number		*
C*					  3 = Special symbol number	*
C*	VALFLG (2)	LOGICAL		Flag to plot value		*
C*	IPRECN (2)	INTEGER		Number of decimal places	*
C*	RANGE  (4)	REAL		Range for maxima and minima	*
C*					  RANGE (1) = min for max	*
C*					  RANGE (2) = max for max	*
C*					  RANGE (3) = min for min	*
C*					  RANGE (4) = max for min	*
C*	KRAD		INTEGER		Search radius			*
C*	KNT    (2)	INTEGER		Maximum number to find		*
C*	INTFLG		LOGICAL		Flag to interpolation		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		01/93						*
C* S. Jacobs/NMC	10/94	Parse the range with ST_RANG		*
C* D. Keiser/GSC	11/95	Incr. dimension of variable part	*
C* S. Jacobs/NCEP	 4/96	Parse colors using IN_COLR		*
C* G. Krueger/EAI	 8/96	Fix no semicolon cases			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER* (*)	hilo, csymbl (2)
	INTEGER		icolr (2), isymbl (2), isymtp (2), knt (2),
     +			iprecn (2)
	REAL		range (4)
	LOGICAL		valflg (2), intflg
C*
	CHARACTER	strngs(6)*32, part(2)*50, subprt(2)*8,
     +			first*8, last*8, inc*8
C------------------------------------------------------------------------
	iret  = 0
C
C*	Split the input into six substrings.
C
	CALL ST_CLST ( hilo, '/', ' ', 6, strngs, num, ier )
C
C*	Get the color numbers.
C
	isemi = INDEX ( strngs (1), ';' )
	IF  ( isemi .eq. 0 )  THEN
	    IF  ( strngs (1) .eq. ' ' )  THEN
		icolr (1) = 0
		icolr (2) = 0
	      ELSE
		CALL IN_COLR ( strngs (1), 1, icolr, ier )
		icolr (2) = icolr (1)
	    END IF
	  ELSE
	    CALL IN_COLR ( strngs (1), 2, icolr, ier )
	END IF
C
	IF  ( ( icolr (1) .eq. 0 ) .and. ( icolr (2) .eq. 0 ) ) RETURN
C
C*	Get the symbol type and set flag for plotting the value.
C
	isemi = INDEX ( strngs (2), ';' )
	IF ( isemi .eq. 0 ) THEN
	    IF ( strngs (2) .eq. ' ' ) THEN
		strngs (2) = 'H;L'
	    ELSE
		CALL ST_LSTR ( strngs (2), lenstr, ier )
		strngs (2) = strngs (2) (1:lenstr) // ';' // strngs (2)
	    END IF
	END IF
C
	CALL ST_CLST ( strngs (2), ';', ' ', 2, part, num, ier )
	DO i = 1, 2
	    CALL ST_CLST ( part (i), '#', ' ', 2, subprt, npp, ier )
	    CALL ST_NUMB ( subprt (1), ival, ier )
	    CALL ST_NUMB ( subprt (1)(2:), ivalm, ierm )
	    IF ( ier .eq. 0 ) THEN
		csymbl (i) = ' '
		isymbl (i) = ival
		isymtp (i) = 2
	    ELSE IF ( ierm .eq. 0 ) THEN
		csymbl (i) = ' '
		isymbl (i) = ivalm
		CALL ST_LCUC ( subprt (1)(1:1), first, ier )
		IF ( first .eq. 'S' )  THEN
		    isymtp (i) = 3
		ELSE
		    isymtp (i) = 2
		END IF
	    ELSE IF ( subprt (1) .ne. ' ' ) THEN
	        csymbl (i) = subprt (1)
		isymbl (i) = IMISSD
	        isymtp (i) = 1
	    ELSE
		csymbl (i) = ' '
		isymbl (i) = IMISSD
		isymtp (i) = 0
	    END IF
	    IF ( INDEX ( part (i), '#' ) .eq. 0 ) THEN
		valflg (i) = .false.
		iprecn (i) = 0
	    ELSE
		valflg (i) = .true.
		CALL ST_NUMB ( subprt (2), ival, ier )
		IF ( ier .eq. 0 ) THEN
		    iprecn (i) = ival
		ELSE
		    iprecn (i) = 0
		END IF
	    END IF
	    IF  ( iprecn(i) .lt. 0 )  iprecn(i) = 0
	    IF  ( iprecn(i) .gt. 9 )  iprecn(i) = 9
	END DO
C
C*	Get the range.
C
	IF ( ( strngs (3) .eq. 'all' .or. strngs (3) .eq. 'ALL' )
     +	    .or. strngs (3) .eq. ' ' ) THEN
	    DO i = 1, 4
		range (i) = 0.0
	    END DO
	ELSE
C
	    isemi = INDEX ( strngs (3), ';' )
	    IF ( isemi .eq. 0 ) THEN
		CALL ST_LSTR ( strngs (3), lenstr, ier )
		strngs (3) = strngs (3) (1:lenstr) // ';' // strngs (3)
	    END IF
	    CALL ST_CLST ( strngs (3), ';', ' ', 2, part, num, ier )
	    DO i = 1, num
		indx = (i - 1) * 2 + 1
		CALL ST_RANG ( part (i), first, last, inc, itype, ier )
		CALL ST_CRNM ( first, val, ier )
		IF ( ier .ne. 0 ) val = 0
		range (indx) = val
		indx = indx + 1
		CALL ST_CRNM ( last, val, ier )
		IF ( ier .ne. 0 ) val = 0
		range (indx) = val
		indx = indx + 1
	    END DO
	    save = range (1)
	    range (1) = MIN ( range (1), range (2) )
	    range (2) = MAX ( save, range (2) )
	    save = range (3)
	    range (3) = MIN ( range (3), range (4) )
	    range (4) = MAX ( save, range (4) )
	END IF
C
C*	Get the search radius.
C
	CALL ST_CRNM ( strngs (4), val, ier )
	IF ( val .le. 0 .or. ier .ne. 0 ) THEN
	    krad = 3
	ELSE
	    krad = val
	END IF
C
C*	Get the maximum count for extrema.
C
	CALL ST_ILST ( strngs (5), ';', 20, 2, knt, nn, ier )
	IF ( ier .ne. 0 ) THEN
	    knt (1) = 20
	    knt (2) = 20
	END IF
	IF ( nn .eq. 1 ) knt (2) = knt (1)
C
C*	Get the interpolation flag.
C
	IF ( strngs (6) (1:1) .eq. 'y' .or. strngs (6) (1:1)
     +	     .eq. 'Y' ) THEN
	    intflg = .true.
	ELSE
	    intflg = .false.
	END IF
C*
	RETURN
	END
