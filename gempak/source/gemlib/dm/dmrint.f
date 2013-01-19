	SUBROUTINE DM_RINT  ( iflno, isword, nword, idata, iret )
C************************************************************************
C* DM_RINT								*
C*									*
C* This subroutine reads integer data from a DM file.			*
C*									*
C* DM_RINT  ( IFLNO, ISWORD, NWORD, IDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NWORD		INTEGER		Number of words			*
C*									*
C* Output parameters:							*
C*	IDATA (NWORD)	INTEGER		Data				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 5/90	Add translation for diff machines	*
C* K. Brill/NMC		 3/91	Use MV_ functions			*
C* M. desJardins/NMC	 5/91	Add logical vars for machine type	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C*
	INTEGER		idata (*)
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
		    idata (knt) = kcdata (i,ircpnt)
		    knt = knt + 1
		END DO
		ileft = ileft - ( jend - jword + 1 )
		irec  = irec + 1
		jword = 1
	    END IF
	END DO
C
C*	Translate data from different machine.
C
	IF  ( ( kmachn (iflno) .ne. MTMACH ) .and.
     +	      ( ( kvmst ( iflno ) .and. ( .not. mvmst ) ) .or.
     +	        ( mvmst .and. ( .not. kvmst ( iflno ) ) ) ) )  THEN
	    ier = MV_SWP4 ( nword, idata, idata )
	END IF
C
C*	Translate the missing data value, if necessary.
C
	IF  ( IMISSD .ne. kmissd ( iflno ) )  THEN
	    kk = kmissd ( iflno )
	    DO  i = 1, nword
		IF  ( idata (i) .eq. kk )  idata ( i ) = IMISSD
	    END DO
	END IF
C*
	RETURN
	END
