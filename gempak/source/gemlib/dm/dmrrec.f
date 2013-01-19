	SUBROUTINE DM_RREC  ( iflno, irec, ircpnt, iflerr, iret )
C************************************************************************
C* DM_RREC								*
C*									*
C* This subroutine reads a data record from a file.			*
C*									*
C* DM_RREC  ( IFLNO, IREC, IRCPNT, IFLERR, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IREC		INTEGER		Record number			*
C*									*
C* Output parameters:							*
C*	IRCPNT		INTEGER		Pointer to record in cache	*
C*	IFLERR		INTEGER		GEMPAK file error number	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 3/87	Added write for records replaced	*
C* M. desJardins/GSFC	11/87	Initialize iflerr			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
	iret   = 0
	iflerr = 0
C
C*	Check for data already in cache.
C
	ircpnt = 0
	knt    = 1
	DO WHILE  ( (ircpnt .eq. 0) .and. (knt .le. MCACHE) )
	  IF ((kcflno (knt) .eq. iflno) .and. (kcrecn (knt) .eq. irec))
     +			ircpnt = knt
	  knt = knt + 1
	END DO
	IF  ( ircpnt .ne. 0 ) RETURN
C
C*	Get the next record in the cache.
C
	CALL DM_NXTC  ( ircpnt, ier )
C
C*	Read requested data record.  Check first to see if this is
C*	a shared file.
C
	IF  ( kshare ( iflno ) )  THEN
	    CALL FL_RSHR  ( lundm (iflno), irec, MBLKSZ, 
     +			    kcdata (1,kclast), iflerr )
	  ELSE
	    CALL FL_READ  ( lundm (iflno), irec, MBLKSZ, 
     +			    kcdata (1,kclast), iflerr )
	END IF
C
C*	In case of error, blank out record.
C
	IF ( iflerr .ne. 0 ) THEN
	    iret = -7
	    DO  i = 1, MBLKSZ
		kcdata ( i, ircpnt ) = 0
	    END DO
	END IF
C
C*	Update file number and record number in cache.
C
	kcflno ( ircpnt ) = iflno
	kcrecn ( ircpnt ) = irec
C*
	RETURN
	END
