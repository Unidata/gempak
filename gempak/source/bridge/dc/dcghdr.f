	SUBROUTINE DC_GHDR  ( bultin, lenbul, seqnum, wmohdr, oristn,
     +			      time, bbb, nchar, iret )
C************************************************************************
C* DC_GHDR								*
C*									*
C* This subroutine gets the header information from a WMO bulletin.	*
C*									*
C* The bulletin is assumed to have three lines that compose the header	*
C* information. The parsing is done based on the existence of these	*
C* three lines of data. The bulletin should still have all control	*
C* characters present for this routine to function properly.		*
C*									*
C* DC_GHDR  ( BULTIN, LENBUL, SEQNUM, WMOHDR, ORISTN, TIME, BBB,	*
C*	      NCHAR, IRET )						*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		WMO bulletin w/ control chars	*
C*	LENBUL		INTEGER		Length of the WMO bulletin 	*
C*									*
C* Output parameters:							*
C*	SEQNUM		CHAR*		Bulletin sequence number	*
C*	WMOHDR		CHAR*		WMO header			*
C*	ORISTN		CHAR*		Originating station		*
C*	TIME		CHAR*		Time				*
C*	BBB		CHAR*		Indicator for COR, AMD, etc.	*
C*	NCHAR		INTEGER		Number of chars in the header	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-12 = cannot get header info	*
C*					-16 = invalid time              *
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/89	GEMPAK 5				*
C* S. Jacobs/NMC	 6/95	Renamed from RG_GHDR			*
C* S. Jacobs/NCEP	10/95	Added sequence number			*
C* S. Jacobs/NCEP	12/95	Recoded and added BBB and NCHAR		*
C* S. Jacobs/NCEP	 1/96	Added check for MAXCHR in header line	*
C* S. Jacobs/NCEP	 9/96	Added check to skip lines w/ only CHLF	*
C* K. Tyle/GSC		 1/97	Check for 'Z' at end of DDHHMM		*
C* F. J. Yen/NCEP	10/01	Added check for 'UTC' in header line	*
C* D. Kidwell/NCEP	10/05	Added validity checks for TIME          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MAXCHR = 48 )
C*
	CHARACTER*(*)	bultin, seqnum, wmohdr, oristn, time, bbb
C*
	CHARACTER	tarr (3)*48, c*1, carr (5)*8, tmpstr*8
C------------------------------------------------------------------------
	iret  = 0
C
C*	If the first character is not a Control-A, there is no way to
C*	count through the header and separate the elements.
C
	IF  ( bultin (1:1) .ne. CHCTLA )  THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Blank out the array strings.
C
	DO  ii = 1, 3
	    tarr (ii) = ' '
	END DO
C
C*	Set the counters and loop over all the characters looking for 
C*	the third line feed.
C
	nchar = 0
	jcnt  = 0
	knt   = 0
	DO  WHILE ( ( nchar .lt. lenbul ) .and. ( knt .lt. 3 ) )
	    nchar = nchar + 1
	    c = bultin (nchar:nchar)
C
C*	    If a line feed is found, increment the counter.
C
	    IF  ( c .eq. CHLF )  THEN
		IF  ( jcnt .gt. 0 )  THEN
		    knt  = knt + 1
		    jcnt = 0
		END IF
	      ELSE
C
C*		Add valid characters to the current array string.
C
C*		Keep a count of all characters on the line, so that
C*		lines with only a line feed are ignored.
C
		IF  ( ( c .eq. CHCTLA ) .or. ( c .eq. ' ' ) .or.
     +		      ( ( c .ge. '0' ) .and. ( c .le. '9' ) ) .or.
     +		      ( ( c .ge. 'A' ) .and. ( c .le. 'Z' ) ) )  THEN
		    jcnt = jcnt + 1
C
C*		    If the character count is greater than the maximum,
C*		    return with an error.
C
		    IF  ( jcnt .gt. MAXCHR )  THEN
			iret = -12
			RETURN
		    END IF
C
C*		    Otherwise, add the character to the string.
C
		    tarr (knt+1)(jcnt:jcnt) = c
		END IF
	    END IF
	END DO
C
C*	The second string is the sequence number.
C
	seqnum = tarr (2)
C
C*	Break the third string into parts.
C
	CALL ST_CLST  ( tarr(3), ' ', ' ', 5, carr, n, ier )
	wmohdr = carr (1)
	oristn = carr (2)
	time   = carr (3)
	IF ( carr (4) .eq. 'UTC' .and. n .eq. 5 ) THEN
	    bbb = carr (5)
	  ELSE
	    bbb = carr (4)
	END IF
C
C*	Check for a time that is too long. If it is more than six
C*	characters, it is probably of the form MMDDHHMM instead of
C*	DDHHMM, or of the form DDHHMMZ.
C
	CALL ST_LSTR ( time, lent, ier )
	IF  ( lent .eq. 8 )  THEN
	    tmpstr = time (3:8)
	    time   = tmpstr
	  ELSE IF ( lent .eq. 7 .and. time (7:7) .eq. 'Z' ) THEN
	    tmpstr = time (1:6)
	    time   = tmpstr
	END IF
C
C*	Check for a valid bulletin time.
C
	CALL ST_LSTR ( time, lent, ier )
	IF ( lent .ge. 6 ) THEN
	    CALL ST_INTG ( time ( 1:2 ), iday, ier )
	    IF ( ( iday .le. 0 ) .or. ( iday .gt. 31 ) ) iret = -16
	    CALL ST_INTG ( time ( 3:6 ), ihrm, ier )
	    IF ( ( ihrm .lt. 0 ) .or. ( ihrm .gt. 2359 ) ) iret = -16
	  ELSE
	    iret = -16
	END IF
C*
	RETURN
	END
