	SUBROUTINE DC_GPIL  ( bultin, lenbul, pilhdr, wmohdr, oristn,
     +			      time, nchar, iret )
C************************************************************************
C* DC_GPIL								*
C*									*
C* This subroutine gets the header information from an AFOS bulletin.	*
C*									*
C* The bulletin is assumed to have two lines that compose the header	*
C* information. The parsing is done based on the existence of these	*
C* two lines of data. The bulletin should still have all control	*
C* characters present for this routine to function properly.		*
C*									*
C* DC_GPIL  ( BULTIN, LENBUL, PILHDR, WMOHDR, ORISTN, TIME, NCHAR,	*
C*	      IRET )							*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		WMO bulletin w/ control chars	*
C*	LENBUL		INTEGER		Length of the WMO bulletin 	*
C*									*
C* Output parameters:							*
C*	PILHDR		CHAR*		PIL header			*
C*	WMOHDR		CHAR*		WMO header			*
C*	ORISTN		CHAR*		Originating station		*
C*	TIME		CHAR*		Time				*
C*	NCHAR		INTEGER		Number of chars in the header	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-12 = cannot get header info	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/96	Copied from DC_GHDR			*
C* D. Kidwell/NCEP	 6/98	Corrected prologue  			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MAXCHR = 48 )
C*
	CHARACTER*(*)	bultin, pilhdr, wmohdr, oristn, time
C*
	CHARACTER	tarr (2)*48, c*1, carr (3)*12,
     +			tmpstr*8, tstr*48
C------------------------------------------------------------------------
	iret  = 0
C
C*	If the first four characters are not "ZCZC", there is no way to
C*	count through the header and separate the elements.
C
	IF  ( bultin (1:4) .ne. 'ZCZC' )  THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Blank out the array strings.
C
	DO  ii = 1, 2
	    tarr (ii) = ' '
	END DO
C
C*	Set the counters and loop over all the characters looking for 
C*	the second line feed.
C
	nchar = 0
	jcnt  = 0
	knt   = 0
	DO  WHILE ( ( nchar .lt. lenbul ) .and. ( knt .lt. 2 ) )
	    nchar = nchar + 1
	    c = bultin (nchar:nchar)
C
C*	    If a line feed is found, increment the counter.
C
	    IF  ( c .eq. CHLF )  THEN
		knt  = knt + 1
		jcnt = 0
	      ELSE
C
C*		Add valid characters to the current array string.
C
		IF  ( ( c .eq. ' ' ) .or.
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
C*	Break the first string into parts.
C
	CALL ST_UNPR  ( tarr(1), 48, tstr, lenout, ier )
	CALL ST_CLST  ( tstr, ' ', ' ', 2, carr, n, ier )
	pilhdr = carr (2)
C
C*	Break the second string into parts.
C
	CALL ST_UNPR  ( tarr(2), 48, tstr, lenout, ier )
	CALL ST_CLST  ( tstr, ' ', ' ', 3, carr, n, ier )
	wmohdr = carr (1)
	oristn = carr (2)
	time   = carr (3)
C
C*	Check for a time that is too long. If it is more than six
C*	characters, it is probably of the form MMDDHHMM instead of
C*	DDHHMM.
C
	CALL ST_LSTR ( time, lent, ier )
	IF  ( lent .gt. 6 )  THEN
	    tmpstr = time (3:8)
	    time   = tmpstr
	END IF
C*
	RETURN
	END
