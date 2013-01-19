	SUBROUTINE BG_RDPK ( luntbl, parms, iscl, iref, ibit, plist,
     +			     np, iret )
C************************************************************************
C* BG_RDPK								*
C*									*
C* This subroutine reads a Jack Woollen BUFR encoding table file to	*
C* get packing information for a given string of nmemonics.  PARMS	*
C* contains a string of nmemonics separated by blanks.  PARMS is	*
C* is converted to upper case.						*
C*									*
C* BG_RDPK ( LUNTBL, PARMS, ISCL, IREF, IBIT, PLIST, NP, IRET )		*
C*									*
C* Input parameters:							*
C*	LUNTBL		INTEGER		Unit number of BUFR Table file	*
C*									*
C* Input and output parameter:						*
C*	PARMS		CHAR*		List of parameter names		*
C*									*
C* Output parameters:							*
C*	ISCL    (NP)	INTEGER		Power of 10 scaling		*
C*	IREF 	(NP)	INTEGER		Scaled reference value		*
C*	IBIT 	(NP)	INTEGER		Number of bits			*
C*	PLIST	(NP)    CHAR*(*)	Parameter array			*
C*	NP		INTEGER		Number of parm names in string  *
C*	IRET		INTEGER		Return code			*
C*					 +1 = not all parms were found	*
C*					  0 = normal return		*
C*					 -1 = Improper table file	*
C*					 -2 = No parameters		*
C**									*
C* Log:									*
C* K. Brill/EMC		11/96						*
C* K. Brill/EMC		 3/97	Fixes for embedded parm names		*
C************************************************************************
C*
	CHARACTER*(*)	parms, plist(*)
	INTEGER		iscl (*), iref (*), ibit (*)
C*
	CHARACTER	sbuf*80, ctst*1
	LOGICAL		found, count
C-----------------------------------------------------------------------
	iret = 0
C
C*	Count the number of parameters in the input string.
C
	np = 0
	count = .true.
	istop = LEN ( parms )
	DO i = istop, 1, -1
	    ctst = parms ( i:i )
	    IF ( ctst .ne. ' ' .and. count ) THEN
		np = np + 1
		count = .false.
	    ELSE IF ( ctst .eq. ' ' .and. .not. count ) THEN
		count = .true.
	    END IF
	END DO
	IF ( np .le. 0 ) then
	    iret = -2
	    RETURN
	END IF
C
C*	Convert to upper case and form parameter array.
C
	CALL ST_LCUC ( parms, parms, ier )
	CALL ST_CLST ( parms, ' ', ' ', np, plist, num, ier )
	IF ( ier .ne. 0 .or. num .ne. np ) THEN
	    iret = -2
	    RETURN
	END IF
	DO i = 1, np
	    iscl (i) = -9999
	    iref (i) = -9999
	    ibit (i) = -9999
	END DO
C
C*	Search first for REFERENCE label.
C
	REWIND luntbl
	found = .false.
	DO WHILE ( .not. found )
	    READ (luntbl, 1000, IOSTAT=ios ) sbuf
1000	    FORMAT (A)
	    IF ( ios .ne. 0 ) THEN
		iret = -1
		RETURN
	    END IF
	    iq = INDEX ( sbuf, '| REFERENCE' )
	    IF ( iq .ne. 0 ) found = .true.
	END DO
C
C*	Search for requested parameters.
C
	iostat = 0
	icnt = 0
	DO WHILE ( iostat .eq. 0 .and. icnt .lt. np )
	    READ (luntbl, 1000, IOSTAT=iostat ) sbuf
	    IF ( iostat .eq. 0 ) THEN
		found = .false.
		ip = 0
		DO WHILE ( .not. found .and. ip .lt. np )
		    ip = ip + 1
		    CALL ST_LSTR ( plist (ip), lnth, ier )
		    iq = INDEX ( sbuf, plist (ip)(1:lnth) )
		    iql = iq + lnth
		    iqm1 = iq - 1
		    ctst = sbuf ( iql:iql )
		    IF ( iq. ne. 0 .and. iq .lt. 12 .and.
     +		       ( ctst .eq. ' ' .or. ctst .eq. '|' ) .and.
     +		       ( sbuf (iqm1:iqm1) .eq. ' ' .or.
     +		         sbuf (iqm1:iqm1) .eq. '|' ) ) THEN
			icnt = icnt + 1
			CALL ST_NUMB ( sbuf(13:18), iscl (ip), ier )
			CALL ST_NUMB ( sbuf(20:32), iref (ip), ier )
			CALL ST_NUMB ( sbuf(34:38), ibit (ip), ier )
			found = .true.
		    END IF
		END DO
	    END IF
	END DO
	IF ( icnt .ne. np ) THEN
	    iret = 1
	END IF
C*
	RETURN
	END
