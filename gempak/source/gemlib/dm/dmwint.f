	SUBROUTINE DM_WINT  ( iflno, isword, nword, idata, iret )
C************************************************************************
C* DM_WINT								*
C*									*
C* This subroutine writes integer data to a DM file.			*
C*									*
C* DM_WINT  ( IFLNO, ISWORD, NWORD, IDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NWORD		INTEGER		Number of words			*
C*	IDATA (NWORD)	INTEGER		Data				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 6/88	Changed to write immediately		*
C* M. desJardins/NMC	 4/91	Add writes to different machines	*
C* M. desJardins/NMC	 5/91	Add logical vars for machine types	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C
	INTEGER		idata (*)
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
		kcdata (i,ircpnt) = idata (knt)
		knt = knt + 1
	    END DO
C
C*	    Translate missing data value, if necessary.
C
	    IF  ( IMISSD .ne. kmissd ( iflno ) )  THEN
		DO  i = jword, jend
		    IF  ( kcdata (i,ircpnt) .eq. IMISSD )  
     +				kcdata (i,ircpnt) = kmissd (iflno)
		END DO
	    END IF
C
C*	    Translate data for different machines.
C
	    IF  ( ( kmachn (iflno) .ne. MTMACH ) .and.
     +		  ( ( kvmst ( iflno ) .and. ( .not. mvmst ) ) .or.
     +		    ( mvmst .and. ( .not. kvmst ( iflno ) ) ) ) )  THEN
		jsize = jend - jword + 1
		ier = MV_SWP4  ( jsize, kcdata (jword,ircpnt),
     +				 kcdata (jword,ircpnt) )
	    END IF
C
C*	    Set flag to indicate buffer should be written to file.
C
C	    kwrite ( ircpnt ) = .true.
C
C*	    Write data to file.
C
	    CALL FL_WRIT  ( lundm ( iflno ), irec, MBLKSZ,
     +			    kcdata ( 1, ircpnt ), iflerr )
C
C*	    Check error message
C
	    IF  ( iflerr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'FL', iflerr, ' ', ier )
		iret = -6
	    END IF
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
