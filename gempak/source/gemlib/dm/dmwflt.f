	SUBROUTINE DM_WFLT  ( iflno, isword, nword, rdata, iret )
C************************************************************************
C* DM_WFLT								*
C*									*
C* This subroutine writes real data to a DM file.			*
C*									*
C* DM_WFLT  ( IFLNO, ISWORD, NWORD, RDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NWORD		INTEGER		Number of words			*
C*	RDATA (NWORD)	REAL		Data				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/NMC	 4/91	Translate data from other machines	*
C* M. desJardins/NMC	 5/91	Added logical vars for machine type	*
C* S. Jacobs/EAI	 8/92	Added check for ULTRIX machine		*
C* S. Jacobs/EAI	10/93	Added check for ALPHA machine		*
C* S. Jacobs/NCEP	 4/99	Skip conversion for VAX-type floats	*
C* S. Jacobs/NCEP	 2/01	Made MTLNUX a separate machine type	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C
	REAL		rdata (*)
C
C*	Equivalence to treat data in cache as real valued.
C
	REAL		rcdata (MBLKSZ,MCACHE)
	EQUIVALENCE	( rcdata, kcdata )
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	IF  ( nword .le. 0 ) RETURN
C
C*	Compute the first record and start word to write.
C
	CALL DM_WORD ( isword, irec, jword, ier )
C
C*	Loop through records to be written.
C
	ileft = nword
	knt   = 1
	DO WHILE  ( ileft .gt. 0 )
C
C*	    Read the next record into the cache.
C
	    CALL DM_RREC  ( iflno, irec, ircpnt, iflerr, ier )
C
C*	    Move words into the cache buffer.
C
	    jend = jword + ileft - 1
	    IF  ( jend .gt. MBLKSZ ) jend = MBLKSZ
	    DO  i = jword, jend
		rcdata (i,ircpnt) = rdata (knt)
		knt = knt + 1
	    END DO
C
C*	    Translate the missing data value if necessary.
C
	    IF  ( RMISSD .ne. smissd (iflno) )  THEN
		DO  i = jword, jend
		   IF ( ERMISS ( rcdata (i,ircpnt) ) )
     +				    rcdata (i,ircpnt) = smissd (iflno)
		END DO
	    END IF
C
C*	    Translate the data if necessary.
C*	    Do nothing if the machine is the same.
C
	    IF  ( kmachn (iflno) .eq. MTMACH )  THEN
C
C*		Next, do nothing for IEEE floating point nos.
C
	      ELSE IF  ( kieeet ( iflno ) .and. mieeet )  THEN
C
C*		Next, do nothing for VAX floating point nos.
C
	      ELSE IF  ( kvmst ( iflno ) .and. mvmst )  THEN
C
C*		Convert from IEEE to VAX.
C
	      ELSE IF  ( ( kmachn (iflno) .eq. MTVAX  ) .and.
     +			 ( mieeet ) )  THEN
		jsize = jend - jword + 1
		istat = MV_EV32  ( jsize, rcdata (jword, ircpnt),
     +				   rcdata (jword, ircpnt) )
C
C*		Convert from VAX to IEEE.
C
	      ELSE IF  ( ( kieeet ( iflno ) ) .and.
     +			 ( MTMACH .eq. MTVAX ) )  THEN
		jsize = jend - jword + 1
		istat = MV_VE32  ( jsize, rcdata (jword, ircpnt),
     +				   rcdata (jword, ircpnt) )
C
C*		Convert from Intergraph to VAX.
C
	      ELSE IF  ( ( kmachn (iflno) .eq. MTVAX  ) .and.
     +			 ( ( MTMACH .eq. MTULTX ) .or.
     +			   ( MTMACH .eq. MTALPH ) .or.
     +			   ( MTMACH .eq. MTLNUX ) .or.
     +			   ( MTMACH .eq. MTIGPH ) ) )  THEN
		jsize = jend - jword + 1
		istat = MV_SWP4  ( jsize, rcdata (jword, ircpnt),
     +				   rcdata (jword, ircpnt) )
		istat = MV_EV32  ( jsize, rcdata (jword, ircpnt),
     +				   rcdata (jword, ircpnt) )
C
C*		Convert from VAX to Intergraph.
C
	      ELSE IF  ( ( ( kmachn (iflno) .eq. MTIGPH ) .or.
     +			   ( kmachn (iflno) .eq. MTULTX ) .or.
     +			   ( kmachn (iflno) .eq. MTLNUX ) .or.
     +			   ( kmachn (iflno) .eq. MTALPH ) )  .and.
     +			 ( MTMACH .eq. MTVAX ) )  THEN
		jsize = jend - jword + 1
		istat = MV_VE32  ( jsize, rcdata (jword, ircpnt),
     +				   rcdata (jword, ircpnt) )
		istat = MV_SWP4  ( jsize, rcdata (jword, ircpnt),
     +				   rcdata (jword, ircpnt) )
C
C*		Convert from Intergraph to IEEE.
C
	      ELSE IF  ( ( kieeet ( iflno ) ) .and.
     +			 ( ( MTMACH .eq. MTULTX ) .or.
     +			   ( MTMACH .eq. MTALPH ) .or.
     +			   ( MTMACH .eq. MTLNUX ) .or.
     +			   ( MTMACH .eq. MTIGPH ) ) )  THEN
		jsize = jend - jword + 1
		istat = MV_SWP4  ( jsize, rcdata (jword, ircpnt),
     +				   rcdata (jword, ircpnt) )
C
C*		Convert from IEEE to Intergraph.
C
	      ELSE IF  ( ( ( kmachn (iflno) .eq. MTIGPH ) .or.
     +			   ( kmachn (iflno) .eq. MTULTX ) .or.
     +			   ( kmachn (iflno) .eq. MTLNUX ) .or.
     +			   ( kmachn (iflno) .eq. MTALPH ) ) .and.
     +			 ( mieeet ) )  THEN 
		jsize = jend - jword + 1
		istat = MV_SWP4  ( jsize, rcdata (jword, ircpnt),
     +				   rcdata (jword, ircpnt) )
	      ELSE
		iret = -33
		RETURN
	    END IF
C
C*	    Set flag to indicate buffer should be written to file.
C
	    kwrite ( ircpnt ) = .true.
C
C*	    Update number of words left.
C
	    ileft = ileft - ( jend - jword + 1 )
	    irec  = irec + 1
	    jword = 1
C
	END DO
C*
	RETURN
	END
