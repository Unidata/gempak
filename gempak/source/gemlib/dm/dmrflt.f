	SUBROUTINE DM_RFLT  ( iflno, isword, nword, rdata, iret )
C************************************************************************
C* DM_RFLT								*
C*									*
C* This subroutine reads data from a DM file.				*
C*									*
C* DM_RFLT  ( IFLNO, ISWORD, NWORD, RDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NWORD		INTEGER		Number of words			*
C*									*
C* Output parameters:							*
C*	RDATA (NWORD)	REAL		Data				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C*					-33 = cannot translate data	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	 5/90	Add translation for diff machines	*
C* M. desJardins/GSFC	12/90	Note Sun, Iris and Apollo are all IEEE	*
C* K. Brill/NMC		01/91	Extend checks to cover all 4 machines	*
C* M. desJardins/GSFC	 3/91	Added calls to MV_ functions		*
C* S. Jacobs/EAI	 8/92	Added check for ULTRIX machine		*
C* S. Jacobs/EAI	10/93	Added check for ALPHA machine		*
C* S. Jacobs/NCEP	 4/99	Skip conversion for VAX-type floats	*
C* S. Jacobs/NCEP	 2/01	Made MTLNUX a separate machine type	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C*
	REAL		rdata (*)
C*
	REAL		rcdata ( MBLKSZ, MCACHE )
	EQUIVALENCE	( rcdata, kcdata )
C
C*	Note that this missing data statement function is the same as
C*	the standard GEMPAK function, but RMISSD has been replaced
C*	by RMDATA.
C
	LOGICAL		ERMISS
	ERMISS (x) = ( ABS ( x - RMDATA ) .lt. RDIFFD )
C------------------------------------------------------------------------
	iret = 0
C
C*	Compute the first record and word to read.
C
	CALL DM_WORD ( isword, irec, jword, ier )
C
C*	Loop through records to be read.
C
	ileft = nword
	knt   = 1
	DO WHILE  ( ileft .gt. 0 )
C
C*	    Read in the next record.
C
	    CALL DM_RREC  ( iflno, irec, ircpnt, iflerr, iret )
C
C*	    Print errors encountered.
C
	    IF ( iflerr .ne. 0 ) THEN
		CALL ER_WMSG  ( 'FL', iflerr, ' ', ier )
		ileft = 0
	      ELSE
C
C*		Move words into the calling buffer.
C
		jend = jword + ileft - 1
		IF  ( jend .gt. MBLKSZ ) jend = MBLKSZ
		DO  i = jword, jend
		    rdata ( knt ) = rcdata ( i, ircpnt )
		    knt = knt + 1
		END DO
		ileft = ileft - ( jend - jword + 1 )
		irec  = irec + 1
		jword = 1
	    END IF
	END DO
C
C*	Translate the data if necessary.
C*	Do nothing if the machine is the same.
C
	IF  ( kmachn (iflno) .eq. MTMACH )  THEN
C
C*	    Next, do nothing for IEEE floating point nos.
C
	  ELSE IF  ( kieeet ( iflno ) .and. mieeet )  THEN
C
C*	    Next, do nothing for VAX floating point nos.
C
	  ELSE IF  ( kvmst (iflno) .and. mvmst  )  THEN
C
C*	   Convert from VAX to IEEE.
C
	  ELSE IF  ( ( kmachn (iflno) .eq. MTVAX ) .and.
     +		     ( mieeet ) )  THEN
	    istat = MV_VE32  ( nword, rdata, rdata )
C
C*	    Convert from IEEE to VAX.
C
	  ELSE IF  ( ( kieeet ( iflno ) ) .and.
     +		     ( MTMACH .eq. MTVAX  ) )  THEN
	    istat = MV_EV32  ( nword, rdata, rdata )
C
C*	    Convert from Intergraph to IEEE by swapping bytes.
C
	  ELSE IF  ( ( ( kmachn (iflno) .eq. MTIGPH ) .or.
     +		       ( kmachn (iflno) .eq. MTULTX ) .or.
     +		       ( kmachn (iflno) .eq. MTLNUX ) .or.
     +		       ( kmachn (iflno) .eq. MTALPH ) ) .and.
     +		     ( mieeet ) )  THEN
	    istat = MV_SWP4  ( nword, rdata, rdata )
C
C*	    Convert from IEEE to Intergraph.
C
	  ELSE IF  ( ( kieeet ( iflno ) ) .and.
     +		     ( ( MTMACH .eq. MTIGPH ) .or.
     +		       ( MTMACH .eq. MTULTX ) .or.
     +		       ( MTMACH .eq. MTLNUX ) .or.
     +		       ( MTMACH .eq. MTALPH ) ) )  THEN
	    istat = MV_SWP4  ( nword, rdata, rdata )
C
C*	     Convert from Intergraph to VAX.
C
	  ELSE IF  ( ( ( kmachn (iflno) .eq. MTIGPH ) .or.
     +		       ( kmachn (iflno) .eq. MTULTX ) .or.
     +		       ( kmachn (iflno) .eq. MTLNUX ) .or.
     +		       ( kmachn (iflno) .eq. MTALPH ) ) .and.
     +		     ( MTMACH .eq. MTVAX ) )  THEN
	    istat = MV_SWP4  ( nword, rdata, rdata )
	    istat = MV_EV32  ( nword, rdata, rdata )
C
C*	    Convert from VAX to Intergraph.
C
	  ELSE IF  ( ( kmachn (iflno) .eq. MTVAX  ) .and.
     +		     ( ( MTMACH .eq. MTULTX ) .or.
     +		       ( MTMACH .eq. MTLNUX ) .or.
     +		       ( MTMACH .eq. MTALPH ) .or.
     +		       ( MTMACH .eq. MTIGPH ) ) )  THEN
	    istat = MV_VE32  ( nword, rdata, rdata )
	    istat = MV_SWP4  ( nword, rdata, rdata )
	  ELSE
	    iret = -33
	    RETURN
	END IF
C
C*	Translate the missing data value if necessary.
C
	IF  ( smissd (iflno) .ne. RMISSD )  THEN
	    DO  i = 1, nword
		RMDATA  = smissd (iflno)
		IF ( ERMISS ( rdata (i) ) ) rdata (i) = RMISSD
	    END DO
	END IF
C*
	RETURN
	END
