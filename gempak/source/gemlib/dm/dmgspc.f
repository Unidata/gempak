	SUBROUTINE DM_GSPC  ( iflno, nword, istart, iret )
C************************************************************************
C* DM_GSPC								*
C*									*
C* This subroutine gets space to write data in a DM file.		*
C*									*
C* DM_GSPC  ( IFLNO, NWORD, ISTART, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	NWORD		INTEGER		Number of words			*
C*									*
C* Output parameters:							*
C*	ISTART		INTEGER		First word to write		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	12/87	Modified to write shared files		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C*
	LOGICAL		newspc
C------------------------------------------------------------------------
C*	Check free blocks for sufficient space.  Start at the end of
C*	the list with the shorter blocks.
C
	knt    = knfree ( iflno )
	istart = 0
	newspc = .false.
	DO WHILE  ( ( knt .gt. 0 ) .and. ( istart .eq. 0 ) )
C
C*	    Check for free block space large enough.  
C
	    IF  ( kfreew (1,knt,iflno) .ge. nword ) THEN
		istart = kfreew (2,knt,iflno)
C
C*		Compute the number of blocks left over.
C
		numblk = kfreew (1,knt,iflno) - nword
		locblk = kfreew (2,knt,iflno) + nword
C
C*		Eliminate these free block words.
C
		CALL DM_EFRE  ( iflno, knt, ier )
C
C*		Add leftover free blocks.
C
		IF  ( numblk .gt. 0 )
     +			CALL DM_AFRE  ( iflno, numblk, locblk, ier )
C
	    END IF
	    knt = knt - 1
	END DO
C
C*	If space was not found, allocate at the end of the file.
C
	IF  ( istart .eq. 0 ) THEN
	    istart = kpnext (iflno)
	    kpnext (iflno) = kpnext (iflno) + nword
	    newspc = .true.
	END IF
C
C*	If this is a shared file and more space is to be added,
C*	allocate 50 blocks and write to the last record.  This prevents
C*	the end of the file from being "lost" if the file is not closed
C*	properly.
C
	IF  ( newspc .and. kshare ( iflno ) )  THEN
C
C*	    Get last word to write; see if it is past last word written.
C
	    iend = istart + nword
	    IF  ( iend .gt. klstwd ( iflno ) )  THEN
C
C*		Add fifty records to the file.
C*		Write 50th record.
C*		Flush write buffers; close and reopen file.
C*		Save new last word in data mgmt block.
C
		CALL DM_WORD  ( iend, irec, iword, ier )
		irec = irec + 49
		CALL FL_WRIT  ( lundm ( iflno ), irec, 1, 0, ier )
		CALL DM_FWRT  ( iflno, ier )
		CALL DM_CLOP  ( iflno, ier )
		klstwd ( iflno ) = irec * 128
	    END IF
	END IF
C
C*	Write the data management block to file.
C
	CALL DM_WDMG ( iflno, iret )
C*
	RETURN
	END
